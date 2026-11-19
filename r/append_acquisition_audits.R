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

# The main two sheets retain the audit's two-table arrangement and grouped
# column headings but intentionally display authoritative R-calculated values.
acq_presentation_cols <- c(acquisition_audit_cols, capex_cols)
acq_presentation_headers <- c(
  column_headers[acquisition_audit_cols],
  capex_cash_m = 'Actual cash capital investment',
  cash_after_net_financing_capex_proxy_m =
    'EBTDA after net financing and cash capex (proxy)',
  investment_plus_interest_exceeds_ebitda =
    'Interest plus capex exceeds rent-adjusted EBITDA'
)
acq_presentation_stages <- c(acq_stages, list(
  list(first = 31L, last = 33L, label = 'CAPITAL INVESTMENT',
       style = head_profit)))
acq_sheet <- 'Acquisition financing'
create_audit_sheet(acq_sheet,
  'Acquisition financing | purchase valuation, debt, earnings and cash capital spending',
  'All financial amounts: millions of 2025 CAD. Annual snapshots and pooled 2023-25 totals are separate. The capital-spending proxy is not free cash flow.',
  length(acq_presentation_cols))
acq_data <- model |>
  arrange(scenario_id, year_label,
          factor(airport, levels = c(can_airports, 'BIG4')))
acq_annual_presentation <- acq_data |> filter(period == 'Annual')
acq_pooled_presentation <- acq_data |> filter(period == 'Pooled')
acq_pres_a <- write_audit_table(
  acq_sheet, acq_annual_presentation, acq_presentation_cols,
  acq_presentation_headers, 'ANNUAL | independent acquisition financing in 2025 CAD',
  title_row = 4L, table_name = 'PresentationAcqAnnual',
  pct_names = acq_pct,
  input_names = c(acq_fixed_inputs, 'capex_cash_m'),
  stage_groups = acq_presentation_stages)
acq_pres_p <- write_audit_table(
  acq_sheet, acq_pooled_presentation, acq_presentation_cols,
  acq_presentation_headers,
  'POOLED 2023-25 | three-year adjusted flows, mean adjusted debt/cash stocks',
  title_row = acq_pres_a$last_row + 7L,
  table_name = 'PresentationAcqPooled', pct_names = acq_pct,
  input_names = c(acq_fixed_inputs, 'capex_cash_m'),
  stage_groups = acq_presentation_stages)
for (tab in list(acq_pres_a, acq_pres_p)) {
  openxlsx::addStyle(wb, acq_sheet, mult_style, rows = tab$row_ids,
    cols = match('purchase_multiple', acq_presentation_cols),
    gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, acq_sheet, int_style, rows = tab$row_ids,
    cols = match('passengers', acq_presentation_cols),
    gridExpand = TRUE, stack = TRUE)
}

# Use the 29 original audit fields, insert the three operating assumption
# columns beside the revenue conversion field, rather than append three
# unrelated wide sensitivity result columns.
profit_insertion <- match('incremental_revenue_to_profit', profit_audit_cols)
prof_presentation_cols <- append(
  profit_audit_cols,
  c('operating_cost_base_m', 'cost_saving_share', 'operating_savings_m'),
  after = profit_insertion - 1L)
prof_presentation_headers <- c(
  profit_audit_headers,
  operating_cost_base_m = 'Operating costs excluding rent',
  cost_saving_share = 'Saving on existing costs',
  operating_savings_m = 'Operating savings'
)[prof_presentation_cols]
prof_presentation_stages <- list(
  list(first = 1L, last = 6L, label = 'SCENARIO AND NINE GOALS',
       style = head_style),
  list(first = 7L, last = 9L, label = 'STARTING FINANCES',
       style = head_acquisition),
  list(first = 10L, last = 14L, label = 'PRE-TAX TARGET AND OPENING SHORTFALL',
       style = head_earnings),
  list(first = 15L, last = 21L, label = 'OPERATING ASSUMPTIONS / REVENUE GAP',
       style = head_revenue),
  list(first = 22L, last = 28L, label = 'COUNTERFACTUAL PROFIT AND TAX',
       style = head_financing),
  list(first = 29L, last = 32L, label = 'CONTEXT AND CHECK',
       style = head_style)
)
prof_sheet <- 'Profitability requirements'
create_audit_sheet(prof_sheet,
  'Profitability requirements | six independent goals plus three 15% sensitivities',
  'All money in 2025 CAD millions; additional revenue per passenger in 2025 CAD. Each 15% modifier starts from the same financing case, not from another target.',
  length(prof_presentation_cols))
prof_annual_presentation <- profitability_nine |>
  filter(period == 'Annual')
prof_pooled_presentation <- profitability_nine |>
  filter(period == 'Pooled')
profit_pres_a <- write_audit_table(
  prof_sheet, prof_annual_presentation, prof_presentation_cols,
  prof_presentation_headers,
  'ANNUAL | nine profitability cases per airport / year / financing scenario',
  title_row = 4L, table_name = 'PresentationProfitAnnual',
  pct_names = c(profit_audit_pct, 'cost_saving_share'),
  pax_names = 'required_revenue_per_pax_cad',
  input_names = c('target_net_margin', 'statutory_tax_rate',
                  'incremental_revenue_to_profit', 'cost_saving_share'),
  stage_groups = prof_presentation_stages)
profit_pres_p <- write_audit_table(
  prof_sheet, prof_pooled_presentation, prof_presentation_cols,
  prof_presentation_headers,
  'POOLED 2023-25 | nine cases on inflation-adjusted three-year totals',
  title_row = profit_pres_a$last_row + 7L,
  table_name = 'PresentationProfitPooled',
  pct_names = c(profit_audit_pct, 'cost_saving_share'),
  pax_names = 'required_revenue_per_pax_cad',
  input_names = c('target_net_margin', 'statutory_tax_rate',
                  'incremental_revenue_to_profit', 'cost_saving_share'),
  stage_groups = prof_presentation_stages)
for (tab in list(profit_pres_a, profit_pres_p)) {
  # Visually separate the three sensitivities from the baseline 15% row.
  sr <- tab$row_ids[tab$data$threshold_order == 3.1]
  if (length(sr)) openxlsx::addStyle(
    wb, prof_sheet, openxlsx::createStyle(
      border = 'top', borderColour = '#B7A77A'),
    rows = sr, cols = seq_along(prof_presentation_cols),
    gridExpand = TRUE, stack = TRUE)
}

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
  'Financing / capex proxy',
    'EBTDA after net financing and cash capex is a partial descriptive proxy, not audited free cash flow or cash available to shareholders. The interest-plus-capex indicator does not predict changes in capital spending.',
  'BIG4 aggregation',
    'Four airports are modelled separately. BIG4 required revenue, income taxes, savings and expenses are sums of airport-specific results; no fictitious common tax rate is imposed.',
  'What is excluded',
    'No investment-demand response, fee forecast, principal amortization, transaction fees, investor IRR, working capital, forward demand growth or full cash-flow coverage model is included.',
  'Audit vs presentation',
    'The two presentation sheets contain final R-calculated values for nine profitability cases; the acquisition and profitability audit sheets show the original six objectives with linked Excel formulas. Excel recalculation is required for formula results.'
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

# Place the two analytic stages after observed totals, with the formula
# audits and methodology following the existing per-passenger comparison.
new_sheets <- c(acq_sheet, prof_sheet, acq_audit_sheet,
                profit_audit_sheet, notes_sheet)
sheet_order <- character()
for (nm in old_sheets) {
  sheet_order <- c(sheet_order, nm)
  if (nm == 'Total value comparison') {
    sheet_order <- c(sheet_order, acq_sheet, prof_sheet)
  }
  if (nm == 'Per-passenger comparison') {
    sheet_order <- c(sheet_order, acq_audit_sheet,
                     profit_audit_sheet, notes_sheet)
  }
}
if (length(sheet_order) != 15L ||
    !setequal(sheet_order, names(wb)) || anyDuplicated(sheet_order)) {
  stop('Invalid 15-sheet integrated workbook order.', call. = FALSE)
}
worksheetOrder(wb) <- match(sheet_order, names(wb))
combined_book <- file.path(out_dir,
  'airport_financial_comparison_with_acquisition_2023_2025.xlsx')
openxlsx::saveWorkbook(wb, combined_book, overwrite = TRUE)
message('Saved integrated 2025-CAD acquisition workbook: ', combined_book,
        ' (', nrow(model), ' acquisition rows, ',
        nrow(profitability_nine), ' profitability rows; ',
        nrow(profitability_model), ' formula-audited baseline rows).')
