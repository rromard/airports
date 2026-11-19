# Integrate the validated acquisition model into the production comparison workbook.
# The formula audit writer is shared with the standalone acquisition workbook.
# Presentation tables use saved R results; all monetary results are in 2025 CAD.
# Required in caller: wb (ten observed-finance sheets), out_dir, openxlsx/tidyverse.

acquisition_dir <- here::here('output', 'airport_purchase_counterfactual')
acquisition_sources_path <- file.path(acquisition_dir, 'acquisition_workbook_sources.rds')
if (!file.exists(acquisition_sources_path)) {
  stop('Missing acquisition workbook sources: ', acquisition_sources_path,
       '. Run airport_acquisition_counterfactual.R first.', call. = FALSE)
}
acquisition_sources <- readRDS(acquisition_sources_path)
stopifnot(is.list(acquisition_sources),
          all(c('model', 'profitability_model', 'sensitivity_base',
                'acquisition_audit_cols', 'acquisition_source_cols',
                'acquisition_formula_cols') %in% names(acquisition_sources)))

old_sheets <- names(wb)
if (length(old_sheets) != 10L ||
    !all(c('Total value comparison', 'Per-passenger comparison') %in% old_sheets)) {
  stop('Unexpected observed-finance workbook sheets.', call. = FALSE)
}

years <- 2023:2025
pooled_years <- length(years)
can_airports <- c('YYC', 'YUL', 'YVR', 'YYZ')
acquisition_audit_cols <- acquisition_sources$acquisition_audit_cols
acquisition_source_cols <- acquisition_sources$acquisition_source_cols
acquisition_formula_cols <- acquisition_sources$acquisition_formula_cols

# The acquisition R model has nominal ANNUAL amounts and 2025-CAD POOLED
# amounts. Convert each annual monetary field exactly once. Ratios, debt
# shares, interest rates, passenger counts and CPI reference factors are
# unchanged. The pooled figures already have their appropriate CPI treatment.
adjust_acquisition_dollars <- function(d) {
  money_cols <- grep('_m$', names(d), value = TRUE)
  pax_cols <- grep('_per_pax_cad$', names(d), value = TRUE)
  cols <- unique(c(money_cols, pax_cols))
  if (!all(c('period', 'cpi_factor') %in% names(d))) {
    stop('Acquisition source lacks period or CPI factor.', call. = FALSE)
  }
  if (anyNA(d$cpi_factor) || any(d$cpi_factor <= 0)) {
    stop('Invalid acquisition source CPI factors.', call. = FALSE)
  }
  d |>
    mutate(across(all_of(cols),
      ~ if_else(period == 'Annual', .x * cpi_factor, .x)))
}
model <- adjust_acquisition_dollars(acquisition_sources$model)
profitability_model <- adjust_acquisition_dollars(
  acquisition_sources$profitability_model)
if (nrow(model) != 60L || nrow(profitability_model) != 360L ||
    anyDuplicated(model[c('scenario_id', 'airport', 'period', 'year_label')]) ||
    anyDuplicated(profitability_model[
      c('scenario_id', 'airport', 'period', 'year_label', 'threshold_id')])) {
  stop('Acquisition R sources have unexpected model coverage.', call. = FALSE)
}
# The 2025-CAD presentation and the two formula audits must start from the
# same price basis. Check unchanged financial identities after conversion.
if (any(abs(model$estimated_sale_value_m -
            model$valuation_ebitda_m * model$purchase_multiple) > 1e-5) ||
    any(abs(model$purchase_adjusted_pre_tax_income_m -
            (model$net_income_m + model$ground_rent_m -
               model$additional_acquisition_interest_m)) > 1e-5) ||
    any(abs(profitability_model$required_revenue_per_pax_cad -
            profitability_model$required_revenue_m * 1e6 /
              profitability_model$passengers) > 1e-5)) {
  stop('Converted acquisition amounts or per-passenger rates fail checks.',
       call. = FALSE)
}

# Capital spending was already a MODEL INPUT but absent from the slim audit.
# Display it and both pre-existing financing-capacity diagnostics without
# reclassifying capex as an operating expense or an investor cash-flow measure.
capex_cols <- c('capex_cash_m',
                'cash_after_net_financing_capex_proxy_m',
                'investment_plus_interest_exceeds_ebitda')
if (!all(capex_cols %in% names(model)) ||
    any(model$capex_cash_m < 0, na.rm = TRUE)) {
  stop('Invalid acquisition capital investment inputs.', call. = FALSE)
}
# Pooled cash capex is a sum of annual CPI-adjusted spending, not a mean.
capex_annual <- model |>
  filter(period == 'Annual', airport %in% can_airports) |>
  group_by(scenario_id, airport) |>
  summarise(
    expected = if (all(!is.na(capex_cash_m)))
      sum(capex_cash_m) else NA_real_,
    .groups = 'drop')
capex_pooled <- model |>
  filter(period == 'Pooled', airport %in% can_airports) |>
  select(scenario_id, airport, capex_cash_m) |>
  left_join(capex_annual, by = c('scenario_id', 'airport'))
capex_check <- with(capex_pooled,
  (is.na(capex_cash_m) & is.na(expected)) |
    (!is.na(capex_cash_m) & !is.na(expected) &
     abs(capex_cash_m - expected) < 1e-5))
if (nrow(capex_pooled) != 12L || !all(capex_check)) {
  stop('Pooled capital spending does not match adjusted annual cash spending.',
       call. = FALSE)
}

# Three variations of the 15% objective are additional PRESENTATION ROWS.
# The six original objectives retain their formulas and independent audit.
# 80% conversion and/or 10% savings apply to existing costs EXCLUDING rent.
sensitivity_cases <- tibble::tibble(
  sensitivity_id = c('net_15pct_conversion_80',
                     'net_15pct_savings_10',
                     'net_15pct_combined'),
  sensitivity_order = c(3.1, 3.2, 3.3),
  sensitivity_label = c('3a. 15% net: 80% revenue conversion',
                        '3b. 15% net: 10% operating savings',
                        '3c. 15% net: combined sensitivity'),
  sensitivity_conversion = c(0.80, 1.00, 0.80),
  sensitivity_savings_share = c(0, 0.10, 0.10)
)
profit_keys <- c('scenario_id', 'airport', 'period', 'year_label')
cost_base <- model |>
  transmute(across(all_of(profit_keys)),
            operating_cost_base_m = revenue_m - ebitda_m - ground_rent_m)
if (any(cost_base$operating_cost_base_m < -1e-7) ||
    anyNA(cost_base$operating_cost_base_m) ||
    anyDuplicated(cost_base[profit_keys])) {
  stop('Invalid non-rent operating-cost base for sensitivities.',
       call. = FALSE)
}

profit_base <- profitability_model |>
  left_join(cost_base, by = profit_keys) |>
  mutate(cost_saving_share = 0, operating_savings_m = 0)
base_15_individual <- profit_base |>
  filter(threshold_id == 'net_15pct', airport %in% can_airports)
sensitivity_individual <- tidyr::crossing(
  base_15_individual, sensitivity_cases) |>
  mutate(
    threshold_id = sensitivity_id,
    threshold_order = sensitivity_order,
    profitability_objective = sensitivity_label,
    incremental_revenue_to_profit = sensitivity_conversion,
    cost_saving_share = sensitivity_savings_share,
    operating_savings_m = operating_cost_base_m * cost_saving_share,
    initial_pre_tax_shortfall_m = pmax(0,
      pre_tax_profit_target_at_existing_revenue_m -
        purchase_adjusted_pre_tax_income_m - operating_savings_m),
    shortfall_closed_per_new_revenue_dollar =
      incremental_revenue_to_profit - required_pre_tax_margin,
    required_revenue_m =
      initial_pre_tax_shortfall_m / shortfall_closed_per_new_revenue_dollar,
    required_profit_m = incremental_revenue_to_profit * required_revenue_m,
    counterfactual_revenue_m = revenue_m + required_revenue_m,
    counterfactual_pre_tax_income_m =
      purchase_adjusted_pre_tax_income_m + operating_savings_m +
      required_profit_m,
    estimated_tax_m = pmax(0, counterfactual_pre_tax_income_m) *
      statutory_tax_rate,
    incremental_tax_m = estimated_tax_m - estimated_tax_purchase_adjusted_m,
    counterfactual_after_tax_income_m =
      counterfactual_pre_tax_income_m - estimated_tax_m,
    achieved_net_margin =
      counterfactual_after_tax_income_m / counterfactual_revenue_m,
    required_uplift_pct = required_revenue_m / revenue_m,
    required_revenue_per_pax_cad = required_revenue_m * 1e6 / passengers,
    unmet_target_pct = pmax(0, target_net_margin - achieved_net_margin)
  ) |>
  select(-all_of(names(sensitivity_cases)))
if (nrow(sensitivity_individual) != 144L ||
    any(sensitivity_individual$shortfall_closed_per_new_revenue_dollar <= 0) ||
    any(!is.finite(sensitivity_individual$required_revenue_m)) ||
    any(sensitivity_individual$required_revenue_m < 0) ||
    any(sensitivity_individual$unmet_target_pct > 1e-7)) {
  stop('Invalid individual-airport profitability sensitivities.',
       call. = FALSE)
}

# Compare against the independently generated Section 14 sensitivities,
# themselves reconciled with the published acquisition presentation CSVs.
expected_sensitivity <- acquisition_sources$sensitivity_base |>
  transmute(scenario_id, airport, year,
    net_15pct_conversion_80 = conversion_80_m * factor_2025,
    net_15pct_savings_10 = saving_10_m * factor_2025,
    net_15pct_combined = combined_m * factor_2025) |>
  pivot_longer(starts_with('net_15pct_'),
               names_to = 'threshold_id', values_to = 'expected_revenue_m')
sensitivity_check <- sensitivity_individual |>
  select(scenario_id, airport, year, threshold_id, required_revenue_m) |>
  left_join(expected_sensitivity,
    by = c('scenario_id', 'airport', 'year', 'threshold_id'))
if (nrow(sensitivity_check) != 144L ||
    anyNA(sensitivity_check$expected_revenue_m) ||
    any(abs(sensitivity_check$required_revenue_m -
            sensitivity_check$expected_revenue_m) > 1e-5)) {
  stop('New sensitivity rows disagree with Section 14 R results.',
       call. = FALSE)
}

# A BIG4 case is the SUM of four independently solved airport cases, not
# one synthetic taxpayer with an invented combined income-tax rate.
additive_sensitivity_fields <- c(
  'operating_cost_base_m', 'operating_savings_m',
  'pre_tax_profit_target_at_existing_revenue_m',
  'initial_pre_tax_shortfall_m', 'required_revenue_m', 'required_profit_m',
  'counterfactual_revenue_m', 'counterfactual_pre_tax_income_m',
  'estimated_tax_m', 'incremental_tax_m',
  'counterfactual_after_tax_income_m')
summed_sensitivities <- sensitivity_individual |>
  group_by(scenario_id, period, year_label, threshold_id) |>
  summarise(across(all_of(additive_sensitivity_fields), sum), .groups = 'drop')
sensitivity_big4 <- tidyr::crossing(
  profit_base |> filter(airport == 'BIG4', threshold_id == 'net_15pct'),
  sensitivity_cases) |>
  mutate(
    threshold_id = sensitivity_id,
    threshold_order = sensitivity_order,
    profitability_objective = sensitivity_label,
    incremental_revenue_to_profit = sensitivity_conversion,
    cost_saving_share = sensitivity_savings_share
  ) |>
  select(-all_of(names(sensitivity_cases))) |>
  left_join(summed_sensitivities,
    by = c('scenario_id', 'period', 'year_label', 'threshold_id'),
    suffix = c('', '.sum'))
for (nm in additive_sensitivity_fields) {
  summed_name <- paste0(nm, '.sum')
  if (summed_name %in% names(sensitivity_big4)) {
    sensitivity_big4[[nm]] <- sensitivity_big4[[summed_name]]
  } else if (nm %in% c('operating_cost_base_m', 'operating_savings_m')) {
    # These fields do not occur in the original six-objective source.
    stop('Missing BIG4 sensitivity sum: ', nm, call. = FALSE)
  } else {
    stop('Missing BIG4 field sum: ', nm, call. = FALSE)
  }
}
sensitivity_big4 <- sensitivity_big4 |>
  select(-ends_with('.sum')) |>
  mutate(
    required_uplift_pct = required_revenue_m / revenue_m,
    required_revenue_per_pax_cad = required_revenue_m * 1e6 / passengers,
    achieved_net_margin =
      counterfactual_after_tax_income_m / counterfactual_revenue_m,
    unmet_target_pct = pmax(0, target_net_margin - achieved_net_margin),
    # There is no single BIG4 marginal dollar-to-target conversion.
    shortfall_closed_per_new_revenue_dollar = NA_real_
  )
stopifnot(nrow(sensitivity_big4) == 36L)

# Nine rows per airport/scenario/period: six original independent goals,
# with the three modifiers immediately after the 15% baseline goal.
profitability_nine <- bind_rows(profit_base, sensitivity_individual,
                               sensitivity_big4) |>
  arrange(scenario_id, period, year_label,
    factor(airport, levels = c(can_airports, 'BIG4')), threshold_order)
if (nrow(profitability_nine) != 540L ||
    anyDuplicated(profitability_nine[
      c('scenario_id', 'airport', 'period', 'year_label', 'threshold_id')]) ||
    anyNA(profitability_nine$required_revenue_m) ||
    any(profitability_nine$required_revenue_m < 0)) {
  stop('Incomplete nine-case profitability presentation.', call. = FALSE)
}

# Reuse precisely the original workbook's styles and 30/29-column audit
# formula writers. On production ONLY, the annual input cells are already
# 2025 CAD, so pooled Excel source formulas must not multiply by CPI again.
sys.source(here::here('r', 'acquisition_audit_format.R'), envir = environment())
audit_2025_cad <- TRUE
sys.source(here::here('r', 'acquisition_formula_audits.R'), envir = environment())

# Presentation sheets reproduce the SLIM CENTRAL-case design of the
# standalone counterfactual workbook. The full three-scenario data and
# linked formulas remain available on the two separate audit sheets.
central_id <- 'M22.5-D60-R5.5'
period_levels <- c(as.character(years), '2023-25')
arrange_central <- function(d) {
  d |>
    arrange(factor(airport, levels = c(can_airports, 'BIG4')),
            factor(year_label, levels = period_levels))
}
style_airport_periods <- function(sheet, d, first_row, ncols) {
  pooled <- which(d[['Year / period']] == '2023-25') + first_row - 1L
  next_airport <- which(d[['Airport']] !=
    dplyr::lag(d[['Airport']], default = d[['Airport']][1L])) +
    first_row - 1L
  big4 <- which(d[['Airport']] == 'BIG4') + first_row - 1L
  if (length(pooled)) openxlsx::addStyle(wb, sheet,
    openxlsx::createStyle(textDecoration = 'bold'),
    rows = pooled, cols = seq_len(ncols), gridExpand = TRUE, stack = TRUE)
  if (length(big4)) openxlsx::addStyle(wb, sheet,
    openxlsx::createStyle(textDecoration = 'bold', fontColour = '#193954'),
    rows = big4, cols = seq_len(ncols), gridExpand = TRUE, stack = TRUE)
  if (length(next_airport)) openxlsx::addStyle(wb, sheet,
    openxlsx::createStyle(border = 'top', borderColour = '#8695A4'),
    rows = next_airport, cols = seq_len(ncols), gridExpand = TRUE, stack = TRUE)
}
central_model <- model |> filter(scenario_id == central_id) |>
  arrange_central()
central_profit <- profitability_nine |>
  filter(scenario_id == central_id) |>
  arrange(factor(airport, levels = c(can_airports, 'BIG4')),
          factor(year_label, levels = period_levels), threshold_order)
stopifnot(nrow(central_model) == 20L, nrow(central_profit) == 180L)

# Exactly the 21 original acquisition presentation fields; no capex
# diagnostics, CPI factors, or three-scenario audit metadata.
results_cols <- c(
  'scenario_id','airport','year_label',
  'revenue_m','ebitda_m','ground_rent_m','rent_adjusted_ebitda_m',
  'valuation_ebitda_m','estimated_sale_value_m',
  'gross_debt_m','cash_m','equity_purchase_consideration_m',
  'new_acquisition_debt_m','buyer_equity_contribution_m',
  'net_financing_cost_m','additional_acquisition_interest_m',
  'combined_net_financing_cost_proxy_m','purchase_adjusted_ebtda_m',
  'net_income_m','rent_adjusted_pre_tax_income_m',
  'purchase_adjusted_pre_tax_income_m'
)
stopifnot(length(results_cols) == 21L,
          all(results_cols %in% names(central_model)))
results <- central_model |> select(all_of(results_cols))
results_headers <- column_headers[results_cols]
results_headers[c('net_financing_cost_m',
                  'combined_net_financing_cost_proxy_m')] <-
  c('Existing net financing costs', 'Total net financing costs')
stopifnot(!anyDuplicated(unname(results_headers)))
names(results) <- unname(results_headers)
acq_sheet <- 'Acquisition financing'
openxlsx::addWorksheet(wb, acq_sheet, gridLines = FALSE)
res_ncol <- ncol(results)
for (r in 1:3) openxlsx::mergeCells(wb, acq_sheet,
  cols = 1:res_ncol, rows = r)
openxlsx::writeData(wb, acq_sheet,
  'Airport acquisition counterfactual | acquisition financing (central scenario)',
  startRow = 1)
openxlsx::writeData(wb, acq_sheet,
  '22.5x EBITDA | 60% new borrowing | 5.5% interest | all money in millions of 2025 CAD',
  startRow = 2)
openxlsx::writeData(wb, acq_sheet,
  'Acquisition-adjusted EBTDA = reported EBITDA + eliminated rent - existing net financing - new acquisition interest. Profit targets and taxes are on the next sheet.',
  startRow = 3)
openxlsx::writeData(wb, acq_sheet, results, startRow = 4, withFilter = TRUE)
openxlsx::setColWidths(wb, acq_sheet, cols = 1, widths = 22)
openxlsx::setColWidths(wb, acq_sheet, cols = 2:3, widths = 13)
openxlsx::setColWidths(wb, acq_sheet, cols = 4:res_ncol, widths = 19)
openxlsx::setColWidths(wb, acq_sheet,
  cols = match(c('equity_purchase_consideration_m','valuation_ebitda_m'),
               results_cols), widths = 22)
openxlsx::setRowHeights(wb, acq_sheet, rows = 1:3, heights = c(29,24,24))
openxlsx::setRowHeights(wb, acq_sheet, rows = 4, heights = 55)
openxlsx::addStyle(wb, acq_sheet, head_style, rows = 4,
  cols = 1:res_ncol, gridExpand = TRUE)
results_header_groups <- list(
  acquisition = c('revenue_m','ebitda_m','ground_rent_m',
    'rent_adjusted_ebitda_m','valuation_ebitda_m','estimated_sale_value_m',
    'gross_debt_m','cash_m','equity_purchase_consideration_m',
    'new_acquisition_debt_m','buyer_equity_contribution_m'),
  financing = c('net_financing_cost_m','additional_acquisition_interest_m',
    'combined_net_financing_cost_proxy_m'),
  earnings = c('purchase_adjusted_ebtda_m','net_income_m',
    'rent_adjusted_pre_tax_income_m','purchase_adjusted_pre_tax_income_m')
)
stopifnot(setequal(unlist(results_header_groups, use.names = FALSE),
                   results_cols[-(1:3)]))
for (group in names(results_header_groups)) {
  stage_style <- switch(group, acquisition = head_acquisition,
    financing = head_financing, earnings = head_earnings)
  openxlsx::addStyle(wb, acq_sheet, stage_style, rows = 4,
    cols = match(results_header_groups[[group]], results_cols),
    gridExpand = TRUE)
}
openxlsx::addStyle(wb, acq_sheet, num_style, rows = 5:(nrow(results)+4L),
  cols = 4:res_ncol, gridExpand = TRUE)
style_airport_periods(acq_sheet, results, 5L, res_ncol)
openxlsx::addStyle(wb, acq_sheet,
  openxlsx::createStyle(fontSize = 15, textDecoration = 'bold',
                        fontColour = '#193954'), rows = 1, cols = 1)
openxlsx::addStyle(wb, acq_sheet,
  openxlsx::createStyle(fontSize = 10, fontColour = '#445569'),
  rows = 2, cols = 1)
openxlsx::addStyle(wb, acq_sheet,
  openxlsx::createStyle(fontSize = 10, fontColour = '#445569',
                        textDecoration = 'italic'), rows = 3, cols = 1)
openxlsx::freezePane(wb, acq_sheet, firstActiveRow = 5, firstActiveCol = 4)

# Reuse the original 21-column profitability presentation sequence.
# The three sensitivities are inserted as ROWS after the baseline 15% case.
profitability <- central_profit |>
  transmute(
    Airport = airport,
    `Year / period` = year_label,
    `Profitability objective` = profitability_objective,
    `Target method` = target_type,
    `Observed revenue` = revenue_m,
    `Post-acquisition pre-tax profit` = purchase_adjusted_pre_tax_income_m,
    `Historical pre-tax profit` = net_income_m,
    `Target after-tax margin` = target_net_margin,
    `Statutory tax rate` = statutory_tax_rate,
    `Pre-tax margin needed to achieve net target` = required_pre_tax_margin,
    `Pre-tax target on existing revenue` =
      pre_tax_profit_target_at_existing_revenue_m,
    `Initial pre-tax profit shortfall` = initial_pre_tax_shortfall_m,
    `Share of extra revenue becoming pre-tax profit` =
      incremental_revenue_to_profit,
    `Shortfall closed per extra dollar` =
      shortfall_closed_per_new_revenue_dollar,
    `Additional revenue required` = required_revenue_m,
    `Additional revenue per passenger` = required_revenue_per_pax_cad,
    `New total revenue` = counterfactual_revenue_m,
    `New pre-tax profit` = counterfactual_pre_tax_income_m,
    `Estimated corporate tax` = estimated_tax_m,
    `New after-tax profit` = counterfactual_after_tax_income_m,
    `Achieved after-tax margin` = achieved_net_margin)
stopifnot(ncol(profitability) == 21L, nrow(profitability) == 180L)
profit_individual <- profitability |> filter(Airport != 'BIG4')
profit_big4 <- profitability |> filter(Airport == 'BIG4') |>
  select(Airport, `Year / period`, `Profitability objective`,
         `Target method`, `Observed revenue`,
         `Post-acquisition pre-tax profit`, `Historical pre-tax profit`,
         `Pre-tax target on existing revenue`,
         `Initial pre-tax profit shortfall`,
         `Additional revenue required`,
         `Additional revenue per passenger`,
         `New total revenue`, `New pre-tax profit`,
         `Estimated corporate tax`, `New after-tax profit`,
         `Achieved after-tax margin`)
stopifnot(nrow(profit_individual) == 144L, nrow(profit_big4) == 36L)

prof_sheet <- 'Profitability requirements'
openxlsx::addWorksheet(wb, prof_sheet, gridLines = FALSE)
prof_cols <- ncol(profit_individual)
intro <- c(
  'Airport profitability requirements | six independent targets plus three 15% operating sensitivities, central acquisition scenario',
  'Annual and pooled monetary figures: millions of 2025 CAD. Per-passenger figures: 2025 CAD. Currency units omitted from headers.',
  '1-2. Recover nonnegative pre-tax earnings (break even), then restore historical pre-tax earnings, with a zero floor for an observed loss.',
  '3-6. Test 15%, 20%, 25% and 30% AFTER-TAX margins. Convert the desired net margin into a pre-tax margin using the statutory tax rate.',
  '3a-3c. Three independent 15% variants: 80% revenue conversion, 10% savings on existing operating costs excluding rent, or both.',
  'All nine rows are independent. Extra revenue = initial pre-tax shortfall / shortfall closed per new dollar. Savings reduce the shortfall before solving. No fee or demand forecast is implied.'
)
for (r in seq_along(intro)) {
  openxlsx::mergeCells(wb, prof_sheet, cols = 1:12, rows = r)
  openxlsx::writeData(wb, prof_sheet, intro[[r]], startRow = r)
}
openxlsx::setRowHeights(wb, prof_sheet, rows = 1:6,
  heights = c(32,30,33,33,36,36))
openxlsx::addStyle(wb, prof_sheet,
  openxlsx::createStyle(fontSize = 15, textDecoration = 'bold',
                        fontColour = '#193954'), rows = 1, cols = 1)
openxlsx::addStyle(wb, prof_sheet,
  openxlsx::createStyle(fontSize = 10, fontColour = '#445569',
                        wrapText = TRUE, valign = 'center'),
  rows = 2:6, cols = 1, gridExpand = TRUE)
prof_stage_row <- 7L
prof_header_row <- 8L
prof_first_row <- 9L
prof_last_row <- prof_first_row + nrow(profit_individual) - 1L
prof_groups <- list(
  list(first = 1L, last = 4L, label = 'AIRPORT AND OBJECTIVE',
       style = head_style),
  list(first = 5L, last = 7L, label = 'STARTING FINANCES',
       style = head_acquisition),
  list(first = 8L, last = 12L,
       label = 'TARGET PRE-TAX PROFIT AND INITIAL GAP', style = head_earnings),
  list(first = 13L, last = 16L, label = 'HOW MUCH NEW REVENUE?',
       style = head_revenue),
  list(first = 17L, last = 21L, label = 'RESULTING EARNINGS AND TAX',
       style = head_profit)
)
stopifnot(identical(unlist(lapply(prof_groups,
  function(g) seq.int(g$first, g$last))), seq_len(prof_cols)))
for (g in prof_groups) {
  openxlsx::mergeCells(wb, prof_sheet, cols = g$first:g$last,
                       rows = prof_stage_row)
  openxlsx::writeData(wb, prof_sheet, g$label,
                      startRow = prof_stage_row, startCol = g$first)
  openxlsx::addStyle(wb, prof_sheet, g$style, rows = prof_stage_row,
                     cols = g$first:g$last, gridExpand = TRUE)
}
openxlsx::setRowHeights(wb, prof_sheet, rows = prof_stage_row, heights = 24)
openxlsx::writeData(wb, prof_sheet, profit_individual,
                    startRow = prof_header_row, withFilter = TRUE)
openxlsx::addStyle(wb, prof_sheet, head_style, rows = prof_header_row,
                   cols = 1:prof_cols, gridExpand = TRUE)
for (g in prof_groups) openxlsx::addStyle(wb, prof_sheet, g$style,
  rows = prof_header_row, cols = g$first:g$last, gridExpand = TRUE)
openxlsx::setRowHeights(wb, prof_sheet, rows = prof_header_row, heights = 67)
openxlsx::setColWidths(wb, prof_sheet, cols = 1:2, widths = c(12,14))
openxlsx::setColWidths(wb, prof_sheet, cols = 3:4, widths = c(37,18))
openxlsx::setColWidths(wb, prof_sheet, cols = 5:prof_cols, widths = 20)
openxlsx::setColWidths(wb, prof_sheet,
  cols = c(6,7,11,12,13,14), widths = 24)
prof_rows <- prof_first_row:prof_last_row
openxlsx::addStyle(wb, prof_sheet, num_style, rows = prof_rows,
                   cols = 5:prof_cols, gridExpand = TRUE)
openxlsx::addStyle(wb, prof_sheet, pct_style, rows = prof_rows,
                   cols = c(8,9,10,13,21), gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, prof_sheet, per_dollar_style, rows = prof_rows,
                   cols = 14, gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, prof_sheet, per_pax_style, rows = prof_rows,
                   cols = 16, gridExpand = TRUE, stack = TRUE)
style_airport_periods(prof_sheet, profit_individual,
  first_row = prof_first_row, ncols = prof_cols)
# Boundaries only between COMPLETE nine-row airport-period blocks.
stopifnot(nrow(profit_individual) %% 9L == 0L)
openxlsx::addStyle(wb, prof_sheet,
  openxlsx::createStyle(border = 'top', borderColour = '#8695A4'),
  rows = prof_first_row + seq.int(9L, nrow(profit_individual)-1L, by = 9L),
  cols = 1:prof_cols, gridExpand = TRUE, stack = TRUE)
openxlsx::freezePane(wb, prof_sheet,
  firstActiveRow = prof_first_row, firstActiveCol = 5)

# BIG4 remains a separate, narrower summary, summing the airport-specific
# financing and tax results without inventing an aggregate statutory rate.
sum_title <- prof_last_row + 4L
sum_note <- sum_title + 1L
sum_header <- sum_title + 2L
sum_first <- sum_header + 1L
sum_last <- sum_first + nrow(profit_big4) - 1L
for (r in c(sum_title, sum_note)) openxlsx::mergeCells(wb, prof_sheet,
  cols = 1:12, rows = r)
openxlsx::writeData(wb, prof_sheet,
  'BIG4 COMBINED SUMMARY | nine independent cases, summed from four airports',
  startRow = sum_title)
openxlsx::writeData(wb, prof_sheet,
  'The BIG4 revenue requirements, operating savings, profit and taxes sum airport-specific solutions; no single BIG4 tax rate or conversion is assumed.',
  startRow = sum_note)
openxlsx::addStyle(wb, prof_sheet,
  openxlsx::createStyle(fontColour = '#FFFFFF', fgFill = '#193954',
                        textDecoration = 'bold', fontSize = 11),
  rows = sum_title, cols = 1)
openxlsx::setRowHeights(wb, prof_sheet,
  rows = c(sum_title, sum_note), heights = c(29,28))
openxlsx::writeData(wb, prof_sheet, profit_big4,
                    startRow = sum_header, withFilter = FALSE)
openxlsx::addStyle(wb, prof_sheet, head_profit, rows = sum_header,
  cols = seq_len(ncol(profit_big4)), gridExpand = TRUE)
openxlsx::setRowHeights(wb, prof_sheet, rows = sum_header, heights = 65)
openxlsx::addStyle(wb, prof_sheet, num_style,
  rows = sum_first:sum_last, cols = 5:ncol(profit_big4), gridExpand = TRUE)
openxlsx::addStyle(wb, prof_sheet, pct_style,
  rows = sum_first:sum_last, cols = 16, gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, prof_sheet, per_pax_style,
  rows = sum_first:sum_last, cols = 11, gridExpand = TRUE, stack = TRUE)
style_airport_periods(prof_sheet, profit_big4,
  first_row = sum_first, ncols = ncol(profit_big4))
stopifnot(nrow(profit_big4) %% 9L == 0L)
openxlsx::addStyle(wb, prof_sheet,
  openxlsx::createStyle(border = 'top', borderColour = '#8695A4'),
  rows = sum_first + seq.int(9L, nrow(profit_big4)-1L, by = 9L),
  cols = seq_len(ncol(profit_big4)), gridExpand = TRUE, stack = TRUE)

# The full-width formula audits were created above; the slim central-case
# presentation tables deliberately do not repeat their calculations.

notes_sheet <- 'Acquisition methodology'
openxlsx::addWorksheet(wb, notes_sheet, gridLines = FALSE)
assumption_table <- readr::read_csv(
  file.path(acquisition_dir, 'scenario_assumptions.csv'),
  show_col_types = FALSE) |>
  transmute(Scenario = scenario_name,
            `Sale multiple` = purchase_multiple,
            `New borrowing share` = acquisition_debt_share,
            `New debt interest rate` = acquisition_interest_rate)
openxlsx::writeData(wb, notes_sheet,
  'Acquisition model | assumptions and interpretation', startRow = 1)
openxlsx::writeData(wb, notes_sheet, assumption_table,
                    startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, notes_sheet, head_style, rows = 3,
                   cols = 1:4, gridExpand = TRUE)
openxlsx::addStyle(wb, notes_sheet, pct_style, rows = 4:6,
                   cols = 3:4, gridExpand = TRUE)
method_notes <- tibble::tribble(
  ~Topic, ~Interpretation,
  'Analysis order',
    'Acquisition financing estimates purchase consideration, borrowing, net financing costs and post-acquisition pre-tax earnings. Profitability requirements then solves nine independent goals/variants using those earnings.',
  'Monetary units',
    'All production financial amounts, including both formula audit sheets, are in 2025 CAD millions; required revenue per passenger is in 2025 CAD. Observed financial-comparison sheets use their documented units.',
  'Annual and pooled',
    'Annual financial flows and balances are converted from each year to 2025 CAD. The 2023-25 pooled period sums three years of financial flows and passenger traffic, averages adjusted debt/cash stocks and uses annual-equivalent EBITDA for purchase valuation.',
  'Formula audit inflation',
    'Acquisition and profitability formula audits use already adjusted annual 2025-CAD inputs. The CPI reference column is informative: pooled Excel formulas sum adjusted annual values and do NOT apply CPI a second time. The standalone audit workbook retains nominal annual inputs.',
  'Finance cases',
    'Low-cost, Central and High-cost are three paired illustrative packages of sale multiples, purchase borrowing shares and interest rates, not observed privatization transactions or independently varied cross-products.',
  'Profitability objectives',
    'The six original goals are break even, restore historical pre-tax earnings, and 15%, 20%, 25% and 30% after-tax net profit margins. Each goal is solved separately and uses final revenue in the margin denominator.',
  '15% operating sensitivities',
    'Three rows after the baseline 15% case vary the conversion of additional revenue to pre-tax profit to 80%, impose a 10% saving on existing operating costs (excluding eliminated federal rent), or combine both. They are R results, not additional formula-audit goals.',
  'Capital investment',
    'Actual cash capital investment comes from the existing Canadian airport acquisition supplement. It is not an income-statement operating expense. Pooled cash capex sums 2025-CAD annual spending; missing inputs remain missing.',
  'Capital spending and profit',
    'Cash capex is not a current-period EBITDA expense; capital assets generally enter profit through depreciation or amortisation. Cash capex is retained as a model input, not displayed as a subtraction from acquisition-adjusted earnings in the presentation.',
  'BIG4 aggregation',
    'Four airports are modelled separately. BIG4 required revenue, income taxes, savings and expenses are sums of airport-specific results; no fictitious common tax rate is imposed.',
  'What is excluded',
    'No investment-demand response, fee forecast, principal amortization, transaction fees, investor IRR, working capital, forward demand growth or full cash-flow coverage model is included.',
  'Audit vs presentation',
    'The two slim central-case presentation sheets reuse the standalone audit column selections; profitability adds three 15% sensitivity rows. The separate full-width acquisition and profitability audits retain all three financing scenarios and linked Excel formulas. Excel recalculation is required for formula results.'
)
openxlsx::writeData(wb, notes_sheet, method_notes, startRow = 9)
openxlsx::addStyle(wb, notes_sheet,
  openxlsx::createStyle(wrapText = TRUE, valign = 'center'),
  rows = 10:(9L + nrow(method_notes)), cols = 1:2, gridExpand = TRUE)
openxlsx::setColWidths(wb, notes_sheet, cols = 1:4,
  widths = c(35, 110, 21, 22))
openxlsx::setRowHeights(wb, notes_sheet,
  rows = 10:(9L + nrow(method_notes)), heights = 52)
openxlsx::freezePane(wb, notes_sheet, firstActiveRow = 3)

# FINAL ANALYTIC BRIDGE | DISPLAY AND EXPORT -----------------------------
# The bridge consumes the same validated, already-adjusted R model and the
# matched Australian EBITDA comparison. The module adds two worksheets and
# chart-friendly independent CSV results; the acquisition math is unchanged.
sys.source(here::here('r', 'append_profitability_bridge.R'),
           envir = environment())

# Preserve all ten observed-finance worksheets together, then insert the
# selectable bridge immediately before the two slim acquisition panels.
new_sheets <- c(bridge_sheet, acq_sheet, prof_sheet, bridge_calc_sheet,
                acq_audit_sheet, profit_audit_sheet, notes_sheet)
sheet_order <- character()
for (nm in old_sheets) {
  sheet_order <- c(sheet_order, nm)
  if (nm == 'Per-passenger comparison') {
    sheet_order <- c(sheet_order, new_sheets)
  }
}
if (length(sheet_order) != 17L ||
    !setequal(sheet_order, names(wb)) || anyDuplicated(sheet_order)) {
  stop('Invalid 17-sheet integrated workbook order.', call. = FALSE)
}
worksheetOrder(wb) <- match(sheet_order, names(wb))
combined_book <- file.path(out_dir,
  'airport_financial_comparison_with_acquisition_2023_2025.xlsx')
openxlsx::saveWorkbook(wb, combined_book, overwrite = TRUE)
message('Saved integrated 2025-CAD acquisition workbook: ', combined_book,
        ' (', nrow(model), ' acquisition rows, ',
        nrow(profitability_nine), ' profitability rows; ',
        nrow(profitability_model), ' formula-audited baseline rows).')
