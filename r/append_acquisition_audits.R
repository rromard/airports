# Validate workbook environment before writing integrated presentation.
old_sheets <- names(wb)
if (length(old_sheets) != 10L ||
    !all(c('Total value comparison', 'Per-passenger comparison') %in% old_sheets)) {
  stop('Unexpected observed-finance workbook sheets.', call. = FALSE)
}
# One shared implementation, also used by the standalone bridge entry point.
sys.source(here::here('r', 'prepare_acquisition_bridge_inputs.R'),
           envir = environment())

# Reuse precisely the original workbook's styles and 30/29-column audit
# formula writers. On production ONLY, the annual input cells are already
# 2025 CAD, so pooled Excel source formulas must not multiply by CPI again.
sys.source(here::here('r', 'acquisition_audit_format.R'), envir = environment())
audit_2025_cad <- TRUE
sys.source(here::here('r', 'acquisition_formula_audits.R'), envir = environment())

# Acquisition financing shows all 60 cases; profitability requirements
# retains its independent dropdown to avoid displaying 540 rows.
central_id <- 'M22.5-D60-R5.5'
financing_choices <- c('Low-cost', 'Central', 'High-cost')
finance_name <- function(multiple) {
  dplyr::case_when(multiple == 20 ~ 'Low-cost',
                   multiple == 22.5 ~ 'Central',
                   multiple == 25 ~ 'High-cost',
                   TRUE ~ NA_character_)
}
if (anyNA(finance_name(model$purchase_multiple)) ||
    anyNA(finance_name(profitability_nine$purchase_multiple))) {
  stop('Unrecognised financing case in acquisition presentation inputs.')
}
# Both independently selectable tables use INDEX/MATCH on full source cases.
# Avoid relying on positional rows or precomputing displayed scenario values.
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
results <- model |>
  mutate(Financing = finance_name(purchase_multiple)) |>
  arrange(factor(airport, levels = c(can_airports, 'BIG4')),
          factor(year_label, levels = period_levels),
          factor(Financing, levels = financing_choices)) |>
  select(Financing, all_of(results_cols))
stopifnot(nrow(results) == 60L, !anyNA(results$Financing))
results_cols <- c('Financing', results_cols)
acq_source <- model |>
  mutate(Financing = finance_name(purchase_multiple),
         Lookup_key = paste(Financing, airport, year_label, sep = '|')) |>
  select(Lookup_key, all_of(results_cols)) |>
  arrange(Lookup_key)
if (nrow(acq_source) != 60L || anyNA(acq_source$Lookup_key) ||
    anyDuplicated(acq_source$Lookup_key) ||
    !setequal(acq_source$Lookup_key,
      tidyr::expand_grid(Financing = financing_choices,
                         airport = c(can_airports, 'BIG4'),
                         period = period_levels) |>
        dplyr::transmute(Lookup_key =
          paste(Financing, airport, period, sep = '|')) |>
        dplyr::pull(Lookup_key))) {
  stop('Acquisition selector source does not contain every financing case.')
}
results_headers <- c(Financing = 'Financing scenario', column_headers[results_cols[-1L]])
results_headers[c('net_financing_cost_m',
                  'combined_net_financing_cost_proxy_m')] <-
  c('Existing net financing costs', 'Total net financing costs')
stopifnot(!anyDuplicated(unname(results_headers)))
names(results) <- unname(results_headers)
acq_sheet <- 'Acquisition financing'
add_acquisition_worksheet(wb, acq_sheet, gridLines = FALSE)
res_ncol <- ncol(results)
for (r in 1:3) openxlsx::mergeCells(wb, acq_sheet,
  cols = 1:res_ncol, rows = r)
openxlsx::writeData(wb, acq_sheet,
  'Airport acquisition counterfactual | all three financing scenarios',
  startRow = 1)
openxlsx::writeData(wb, acq_sheet,
  'All three financing cases shown for each airport and period; millions of 2025 CAD.',
  startRow = 2)
openxlsx::writeData(wb, acq_sheet,
  'Acquisition-adjusted EBTDA = reported EBITDA + eliminated rent - existing net financing - new acquisition interest. Profit targets and taxes are on the next sheet.',
  startRow = 3)
openxlsx::writeData(wb, acq_sheet, results, startRow = 4, withFilter = TRUE)

openxlsx::setColWidths(wb, acq_sheet, cols = 1, widths = 22)
openxlsx::setColWidths(wb, acq_sheet, cols = 2:4, widths = c(18,13,13))
openxlsx::setColWidths(wb, acq_sheet, cols = 5:res_ncol, widths = 19)
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
                   results_cols[-(1:4)]))
for (group in names(results_header_groups)) {
  stage_style <- switch(group, acquisition = head_acquisition,
    financing = head_financing, earnings = head_earnings)
  openxlsx::addStyle(wb, acq_sheet, stage_style, rows = 4,
    cols = match(results_header_groups[[group]], results_cols),
    gridExpand = TRUE)
}
openxlsx::addStyle(wb, acq_sheet, num_style, rows = 5:(nrow(results)+4L),
  cols = 5:res_ncol, gridExpand = TRUE)
openxlsx::addStyle(wb, acq_sheet,
  openxlsx::createStyle(border = 'top', borderColour = '#B0BAC5'),
  rows = 5L + seq.int(3L, nrow(results)-1L, by = 3L),
  cols = seq_len(res_ncol), gridExpand = TRUE, stack = TRUE)
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
openxlsx::freezePane(wb, acq_sheet, firstActiveRow = 5, firstActiveCol = 5)

# Reuse the original 21-column profitability presentation sequence.
# The three sensitivities remain rows after the baseline 15% case.
make_profitability_presentation <- function(d) d |>
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
profitability <- make_profitability_presentation(central_profit)
profitability_all <- make_profitability_presentation(profitability_nine) |>
  mutate(Financing = finance_name(profitability_nine$purchase_multiple),
         .before = 1L) |>
  mutate(Lookup_key = paste(Financing, Airport, `Year / period`,
                            `Profitability objective`, sep = '|'),
         .before = 1L) |>
  select(-Financing)
# Derive a complete grid of 540 lookup keys (including BIG4 sensitivities).
expected_profit_keys <- tidyr::expand_grid(
  Financing = financing_choices, Airport = c(can_airports, 'BIG4'),
  Period = period_levels,
  Objective = unique(profitability_nine$profitability_objective)
) |>
  transmute(Lookup_key = paste(Financing, Airport, Period, Objective, sep = '|'))
if (nrow(profitability_all) != 540L || ncol(profitability_all) != 22L ||
    anyNA(profitability_all$Lookup_key) ||
    anyDuplicated(profitability_all$Lookup_key) ||
    !setequal(profitability_all$Lookup_key, expected_profit_keys$Lookup_key)) {
  stop('Profitability selector source keys are incomplete.', call. = FALSE)
}
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
add_acquisition_worksheet(wb, prof_sheet, gridLines = FALSE)
prof_cols <- ncol(profit_individual)
intro <- c(
  'Airport profitability requirements | selectable financing and nine profitability objectives',
  'Annual and pooled monetary figures: millions of 2025 CAD. Per-passenger figures: 2025 CAD. Currency units omitted from headers.',
  '1-2. Recover nonnegative pre-tax earnings (break even), then restore historical pre-tax earnings, with a zero floor for an observed loss.',
  '3-6. Test 15%, 20%, 25% and 30% AFTER-TAX margins. Convert the desired net margin into a pre-tax margin using the statutory tax rate.',
  '3a-3c. Three independent 15% variants: 80% revenue conversion, 10% savings on existing operating costs excluding rent, or both.',
  'All nine rows are independent. Extra revenue = initial pre-tax shortfall / shortfall closed per new dollar. Savings reduce the shortfall before solving. No fee or demand forecast is implied.'
)
for (r in seq_along(intro)) {
  if (r == 2L) {
    openxlsx::mergeCells(wb, prof_sheet, cols = 4:12, rows = r)
    openxlsx::writeData(wb, prof_sheet, 'Finance case', startRow = 2L)
    openxlsx::writeData(wb, prof_sheet, 'Central',
                        startRow = 2L, startCol = 2L)
    openxlsx::writeData(wb, prof_sheet,
      'Annual and pooled amounts: millions of 2025 CAD; per passenger: 2025 CAD.',
      startRow = 2L, startCol = 4L)
  } else {
    openxlsx::mergeCells(wb, prof_sheet, cols = 1:12, rows = r)
    openxlsx::writeData(wb, prof_sheet, intro[[r]], startRow = r)
  }
}
# Each presentation has its OWN financing selector.
openxlsx::writeData(wb, prof_sheet, financing_choices,
  startCol = 50L, startRow = 3L, colNames = FALSE)
openxlsx::dataValidation(wb, prof_sheet, cols = 2L, rows = 2L,
  type = 'list', value = '$AX$3:$AX$5')
openxlsx::addStyle(wb, prof_sheet,
  openxlsx::createStyle(fgFill = '#FFF3D5', textDecoration = 'bold',
                        fontColour = '#193954'),
  rows = 2L, cols = 2L, stack = TRUE)

# Static R source for every financing and profitability objective.
prof_src_start <- 27L  # AA key; AB:AV 21 presentation values
prof_src_first <- 9L
prof_src_last <- prof_src_first + nrow(profitability_all) - 1L
openxlsx::writeData(wb, prof_sheet, profitability_all,
  startCol = prof_src_start, startRow = prof_src_first - 1L)
openxlsx::setColWidths(wb, prof_sheet, cols = 27:50,
  widths = 16, hidden = TRUE)
profit_lookup <- function(row, source_col) {
  lookup <- sprintf(paste0('INDEX($%s$%d:$%s$%d,',
    'MATCH($B$2&"|"&$A%d&"|"&$B%d&"|"&$C%d,',
    '$AA$%d:$AA$%d,0))'),
    source_col, prof_src_first, source_col, prof_src_last,
    row, row, row, prof_src_first, prof_src_last)
  sprintf('IF(COUNT(%s)=1,%s,"")', lookup, lookup)
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
# Airport/period/objective/method stay fixed. All numeric results follow B2.
for (j in 5:prof_cols) {
  source_col <- openxlsx::int2col(prof_src_start + j)
  openxlsx::writeFormula(wb, prof_sheet,
    x = vapply(prof_first_row:prof_last_row,
               profit_lookup, character(1), source_col = source_col),
    startCol = j, startRow = prof_first_row)
}
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
# The BIG4 block uses the SAME B2 selector as the individual airports.
for (j in 5:ncol(profit_big4)) {
  source_j <- match(names(profit_big4)[[j]], names(profitability))
  stopifnot(length(source_j) == 1L, !is.na(source_j))
  source_col <- openxlsx::int2col(prof_src_start + source_j)
  openxlsx::writeFormula(wb, prof_sheet,
    x = vapply(sum_first:sum_last,
               profit_lookup, character(1), source_col = source_col),
    startCol = j, startRow = sum_first)
}
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

# Full-width audits remain distinct. The two presentation selectors expose
# all three finance cases without multiplying rows or recalculating the
# independent R counterfactual model in Excel.

notes_sheet <- 'Acquisition methodology'
add_acquisition_worksheet(wb, notes_sheet, gridLines = FALSE)
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
    'Revenue impacts is the first results tab: a focused 2025 summary with observed, additional and counterfactual revenues, their per-passenger rates and each category uplift. Other presentation tabs provide observed comparisons, acquisition financing, profitability requirements and all 540 Scenario comparison cases. Profitability requirements retains its selector; the R models are not recalculated in Excel.',
  'Australian EBITDA benchmark',
    'The benchmark equals the sum of EBITDA across BNE, MEL, PER and SYD divided by the sum of the matching EBITDA operating-revenue denominators, all in 2025 CAD. It is revenue-weighted, not an average of airport EBITDA percentages or net-profit margins.',
  'Australian benchmark calculations',
    'Bridge calculations independently shows BOTH the 2023-25 pooled Australian EBITDA margin and the period-matched Australian margin, together with their revenue requirements and gaps. There is no external selector or dependency on a presentation worksheet. For the pooled Canadian period, both benchmarks coincide.',
  'Scenario comparison worksheet',
    'One filterable R-calculated row for each of 540 combinations (five airport groups, four periods, three financing cases, nine profitability objectives). The expanded table includes passengers; observed revenue, EBITDA and exclusive aeronautical/parking/other non-aeronautical sources; purchase valuation, new debt and interest; initial post-purchase pre-tax profit; EBITDA margin stages; required revenue and achieved net margin. The Australian benchmark is year-matched (pooled for 2023-25). These static values do not require Excel formula recalculation.',
  'Observed revenue components',
    'non_aero_minus_parking = non_aero_revenue - parking_transport_revenue. Parking includes ground transportation; the residual is only calculated for non-missing source components, including in pooled periods and the BIG4 aggregate. The three exclusive categories provide the source definitions for the illustrative allocation stage, but are not fee forecasts.',
  'Illustrative required-revenue allocation',
    'The fourteen rightmost columns of Scenario comparison present counterfactual total revenue (observed plus additional revenue) and its per-passenger rate, then allocate the already-solved Additional revenue value across three non-overlapping categories under both methods, in totals and per-passenger rates. Canadian 2025 mix weights are each category share of the pooled observed revenue of YYC, YUL, YVR and YYZ. Australian FY2015-2025 growth weights are each category share of the summed, inflation-adjusted revenue increase from FY2014-15 to FY2024-25 at BNE, MEL, PER and SYD. These are alternative allocations, not additional financing cases or forecasts of prices, demand or pass-through.',
  'Revenue allocation labels and impact',
    'total_aero includes Canadian AIF; parking_transport_revenue includes ground/landside transport; non_aero_minus_parking includes other non-aeronautical receipts and is not synonymous with retail sales or commercial rental income. Revenue impacts compares each allocated addition with observed 2025 revenue in the SAME category: (additional / observed) x 100. These are not fee increases or estimates of costs ultimately borne by passengers.',
  'Revenue allocation data and calculations',
    'The profitability_bridge directory retains four revenue_allocation CSVs: Canadian/ACCC source calculations, six allocation shares, complete 3,240-row scenario allocations, and 2025 chart data. It also exports revenue_impacts_2025.csv (270 wide scenario/method rows) and revenue_impacts_2025_long.csv (1,080 category rows including total). The ACCC history is in constant FY2024-25 AUD; only unitless shares are applied to 2025-CAD requirements. All category amounts are reconciled to the scenario total.',
  'Operating benchmark revenue',
    'For a target margin b, Canadian revenue R and rent-adjusted EBITDA E, revenue-to-profit conversion c, and savings S, the required incremental revenue is MAX(0,(bR-E-S)/(c-b)). A margin above c would make the positive-revenue solution infeasible.',
  'Private-acquisition revenue gap',
    'A positive private-objective-minus-Australian-benchmark revenue gap means the private objective requires more additional revenue than reaching the Australian EBITDA margin under the SAME revenue-conversion and savings assumptions. It is not a forecast of airport charges or private investment.',
  'Operating versus financing measures',
    'Scenario-adjusted EBITDA = reported EBITDA + eliminated federal ground rent + operating savings + contribution from new revenue. Scenario-adjusted EBTDA additionally deducts existing net financing costs and new acquisition interest; do not compare this directly to Australian EBITDA as though both were operating margins.',
  'Benchmark and chart exports',
    'The profitability_bridge folder keeps both Australian benchmark modes for all three financing cases, nine profitability objectives, four periods and five airport groups. Separate wide and long-form 2025 Revenue impacts CSVs support direct use in R and charting. The legacy benchmark chart metric list is editable near the top of append_profitability_bridge.R.',
  'Monetary units',
    'All production financial amounts, including both formula calculation sheets, are in 2025 CAD millions; required revenue per passenger is in 2025 CAD. Observed financial-comparison sheets use their documented units.',
  'Annual and pooled',
    'Annual financial flows and balances are converted from each year to 2025 CAD. The 2023-25 pooled period sums three years of financial flows and passenger traffic, averages adjusted debt/cash stocks and uses annual-equivalent EBITDA for purchase valuation.',
  'Formula calculation inflation',
    'Acquisition and profitability formula calculation sheets use already adjusted annual 2025-CAD inputs. The CPI reference column is informative: pooled Excel formulas sum adjusted annual values and do NOT apply CPI a second time. The standalone audit workbook retains nominal annual inputs.',
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
  'Calculations versus presentation',
    'Acquisition financing shows all 60 cases, with three financing scenarios adjacent. Profitability requirements retains its Central-default dropdown including BIG4. Scenario comparison has all 540 cases as fixed R values; Revenue impacts shows both allocation methods in 270 filterable rows. Bridge calculations contains both Australian benchmark methods without a selector and requires Excel recalculation.'
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
# matched Australian EBITDA comparison. The module adds Revenue impacts,
# Scenario comparison and Bridge calculations, plus independent CSV results.
# Isolate the integrated-mode flag so direct source() calls to the bridge
# remain standalone even later in the same R session.
bridge_sheets <- local({
  bridge_integrated_mode <- TRUE
  sys.source(here::here('r', 'append_profitability_bridge.R'),
             envir = environment())
  c(revenue_impacts_sheet, bridge_comparison_sheet, bridge_calc_sheet)
})
revenue_impacts_sheet <- bridge_sheets[[1L]]
bridge_comparison_sheet <- bridge_sheets[[2L]]
bridge_calc_sheet <- bridge_sheets[[3L]]

# The main production workbook builder applies the single final 18-tab
# presentation order and palette and saves the integrated workbook.
# This module adds the acquisition, profitability and bridge sheets only.
new_sheets <- c(revenue_impacts_sheet, bridge_comparison_sheet, acq_sheet, prof_sheet,
                bridge_calc_sheet, acq_audit_sheet, profit_audit_sheet,
                notes_sheet)
if (length(new_sheets) != 8L || anyDuplicated(new_sheets) ||
    length(names(wb)) != 18L ||
    !setequal(c(old_sheets, new_sheets), names(wb))) {
  stop('Invalid 18-sheet integrated workbook inventory.', call. = FALSE)
}
