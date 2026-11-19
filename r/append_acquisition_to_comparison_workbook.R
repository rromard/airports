# Append the illustrative airport-acquisition counterfactual to a COPY of
# the documented Canadian–Australian airport financial comparison workbook.
#
# Run after:
#   r/per_unit_comparison.R
#   r/build_financial_comparison_workbook.R
#   r/airport_acquisition_counterfactual.R (or the current development version)
#
# run_pipeline.R passes its in-memory `comparison_wb` for efficiency. When
# run standalone, this script rebuilds the original financial workbook itself
# in an isolated environment. Never reload it with loadWorkbook(): that route
# has previously led to Excel file-repair / formula-XML problems.
#
# This script adds two directly comparable scenario panels:
#   * Acquisition total amounts: millions of 2025 CAD
#   * Acquisition per passenger: 2025 CAD per passenger
# plus a shared assumptions/interpretation sheet. The documented 10-sheet
# financial workbook is NEVER overwritten.
#
# Source CSVs have unversioned filenames and live in a stable output folder.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(openxlsx)
})

comparison_dir <- here::here('output', 'airport_comparison')
acquisition_dir <- here::here('output', 'airport_purchase_counterfactual')
comparison_book <- file.path(comparison_dir,
  'airport_financial_comparison_documented_2023_2025.xlsx')
combined_book <- file.path(comparison_dir,
  'airport_financial_comparison_with_acquisition_2023_2025.xlsx')
annual_csv <- file.path(acquisition_dir, 'acquisition_per_passenger_annual.csv')
pooled_csv <- file.path(acquisition_dir, 'acquisition_per_passenger_pooled.csv')
assumptions_csv <- file.path(acquisition_dir, 'scenario_assumptions.csv')
financing_csv <- file.path(acquisition_dir, 'acquisition_scenarios.csv')
profitability_csv <- file.path(acquisition_dir, 'profitability_scenarios.csv')
observed_annual_csv <- file.path(comparison_dir,
  'comparison_annual_pp_2023_2025.csv')
observed_pooled_csv <- file.path(comparison_dir,
  'comparison_pooled_pp_2023_2025.csv')

required_files <- c(comparison_book, annual_csv, pooled_csv, assumptions_csv,
                    financing_csv, profitability_csv,
                    observed_annual_csv, observed_pooled_csv)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop('Missing input(s):\n  ', paste(missing_files, collapse = '\n  '),
       call. = FALSE)
}

# Prefer the in-memory workbook passed by run_pipeline.R. For an independent
# run, rebuild the documented workbook in an isolated environment. We do NOT
# use loadWorkbook() on the saved workbook: that route has previously caused
# Excel repair warnings in the formula-heavy sheets.
if (!exists('comparison_wb', inherits = TRUE) ||
    !inherits(get('comparison_wb', inherits = TRUE), 'Workbook')) {
  builder_script <- here::here('r', 'build_financial_comparison_workbook.R')
  if (!file.exists(builder_script)) {
    stop('Cannot rebuild the comparison workbook: missing ', builder_script,
         call. = FALSE)
  }
  message('No in-memory comparison workbook supplied. Rebuilding the original ',
          'workbook to append acquisition sheets without reloading XLSX XML.')
  builder_env <- new.env(parent = globalenv())
  tryCatch(
    base::sys.source(builder_script, envir = builder_env, chdir = FALSE),
    error = function(e) stop('Could not rebuild original financial workbook: ',
                             conditionMessage(e), call. = FALSE)
  )
  if (!exists('wb', envir = builder_env, inherits = FALSE) ||
      !inherits(builder_env$wb, 'Workbook')) {
    stop('Financial-comparison builder did not create a Workbook object.',
         call. = FALSE)
  }
  comparison_wb <- builder_env$wb
}

annual <- readr::read_csv(annual_csv, col_types = cols(period = col_character()),
                          show_col_types = FALSE)
pooled <- readr::read_csv(pooled_csv, col_types = cols(period = col_character()),
                          show_col_types = FALSE)
assumptions <- readr::read_csv(assumptions_csv, show_col_types = FALSE)
financing <- readr::read_csv(financing_csv,
  col_types = cols(period = col_character()), show_col_types = FALSE)
profitability <- readr::read_csv(profitability_csv,
  col_types = cols(period = col_character(), threshold_id = col_character()),
  show_col_types = FALSE)
observed_annual <- readr::read_csv(observed_annual_csv, show_col_types = FALSE)
observed_pooled <- readr::read_csv(observed_pooled_csv, show_col_types = FALSE)

assert_cols <- function(d, cols, label) {
  absent <- setdiff(cols, names(d))
  if (length(absent)) {
    stop(label, ' lacks: ', paste(absent, collapse = ', '), call. = FALSE)
  }
}
required_colnames <- c('country', 'airport', 'year', 'period', 'scenario',
  'variable', 'passengers', 'per_passenger_cad_2025', 'amount_cad_2025_m')
assert_cols(annual, required_colnames, 'Acquisition annual CSV')
assert_cols(pooled, required_colnames, 'Acquisition pooled CSV')
assert_cols(assumptions, c('scenario_name', 'purchase_multiple',
  'acquisition_debt_share', 'acquisition_interest_rate'),
  'Acquisition assumptions')
assert_cols(observed_annual, c('country', 'airport', 'year', 'total_revenue'),
            'Observed annual comparison')
assert_cols(observed_pooled, c('country', 'airport', 'total_revenue'),
            'Observed pooled comparison')

scenarios <- c('Low-cost', 'Central', 'High-cost')
expected_airports <- c('YYC', 'YUL', 'YVR', 'YYZ')
if (!identical(assumptions$scenario_name, scenarios) ||
    nrow(assumptions) != 3L ||
    !setequal(unique(annual$airport), expected_airports) ||
    !setequal(unique(pooled$airport), expected_airports) ||
    !setequal(unique(annual$scenario), scenarios) ||
    !setequal(unique(pooled$scenario), scenarios)) {
  stop('Acquisition scenario/airport coverage does not match the three-case model.',
       call. = FALSE)
}

# Reconcile the model's observed revenue yield with the authoritative
# comparison before adding anything. Both are already in 2025 CAD.
check_observed <- function(acq, official, keys, label) {
  a <- acq |>
    filter(scenario == 'Central', variable == 'cf_observed_total_revenue') |>
    select(all_of(keys), model_pp = per_passenger_cad_2025)
  b <- official |>
    filter(country == 'Canada') |>
    select(all_of(keys), published_pp = total_revenue)
  if (anyDuplicated(a[keys]) || anyDuplicated(b[keys])) {
    stop('Duplicate revenue-validation keys in ', label, call. = FALSE)
  }
  comparison <- inner_join(a, b, by = keys)
  if (nrow(comparison) != nrow(a) || nrow(comparison) != nrow(b) ||
      anyNA(comparison) ||
      any(abs(comparison$model_pp - comparison$published_pp) > 1e-4)) {
    stop('Acquisition revenue per passenger does not reconcile with ', label,
         '. Rebuild from the same current source inputs before combining.',
         call. = FALSE)
  }
}
check_observed(annual, observed_annual, c('airport','year'), 'annual comparison')
check_observed(pooled, observed_pooled, 'airport', 'pooled comparison')

# The presentation keeps the model's flow measures alongside a SMALL
# transaction bridge. Valuation/debt/cash/equity remain absolute 2025 CAD
# millions on BOTH panels: dividing a transaction stock by annual passengers
# would imply an inappropriate annual-flow interpretation.
metric_names <- c(
  cf_observed_total_revenue = 'Observed revenue',
  cf_ground_rent_eliminated = 'Federal ground rent eliminated',
  cf_additional_acquisition_interest = 'Additional acquisition interest',
  cf_post_acquisition_net_financing_cost = 'Post-acquisition net financing costs',
  cf_net_additional_financing_pressure = 'Net added financing pressure',
  cf_post_acquisition_pre_tax_income = 'Post-acquisition pretax income',
  cf_additional_revenue_break_even = 'Revenue needed: break even',
  cf_additional_revenue_restore = 'Revenue needed: restore profit',
  cf_total_revenue_after_restore = 'Total revenue after restoring profit',
  cf_additional_revenue_net_15pct = 'Revenue needed: 15% net margin',
  cf_additional_revenue_net_20pct = 'Revenue needed: 20% net margin',
  cf_additional_revenue_net_25pct = 'Revenue needed: 25% net margin',
  cf_additional_revenue_net_30pct = 'Revenue needed: 30% net margin'
)

# All keys use the literal period label; year remains numeric in the source.
period_name <- function(year) if_else(is.na(year),
  'Pooled 2023–25', as.character(year))
panel_keys <- c('airport', 'period_label', 'scenario')

long_panel <- bind_rows(annual, pooled) |>
  filter(variable %in% names(metric_names)) |>
  mutate(period_label = period_name(year),
         scenario = factor(scenario, levels = scenarios)) |>
  select(airport, period_label, scenario, variable, passengers,
         per_passenger_cad_2025, amount_cad_2025_m)
if (nrow(long_panel) != 4L * 4L * 3L * length(metric_names) ||
    anyNA(long_panel) ||
    anyDuplicated(long_panel[c(panel_keys, 'variable')]) ||
    !setequal(unique(long_panel$variable), names(metric_names)) ||
    any(long_panel$passengers <= 0)) {
  stop('Incomplete or duplicated annual/pooled acquisition comparison data.',
       call. = FALSE)
}
conversion_error <- abs(long_panel$per_passenger_cad_2025 -
  long_panel$amount_cad_2025_m * 1e6 / long_panel$passengers)
if (any(!is.finite(conversion_error)) || any(conversion_error > 1e-7)) {
  stop('Acquisition absolute and per-passenger figures do not reconcile.',
       call. = FALSE)
}

# One source row per airport x period x financing case (BIG4 is excluded).
# The audit model exports stock figures in nominal CAD millions for each
# annual snapshot and in 2025 CAD millions for its pooled balance-sheet
# assumptions (which are AVERAGES, not three-year stock sums).
bridge_fields <- c(
  'valuation_ebitda_m', 'estimated_sale_value_m', 'gross_debt_m',
  'cash_m', 'equity_purchase_consideration_m',
  'new_acquisition_debt_m', 'buyer_equity_contribution_m',
  'total_gross_debt_m'
)
bridge_headers <- c(
  purchase_multiple = 'Sale multiple (x)',
  acquisition_debt_share = 'New debt share (%)',
  acquisition_interest_rate = 'Acquisition rate (%)',
  valuation_ebitda_m = 'Annualized EBITDA after rent saving (2025 CAD m)',
  estimated_sale_value_m = 'Estimated enterprise value (2025 CAD m)',
  gross_debt_m = 'Existing gross debt (2025 CAD m)',
  cash_m = 'Existing cash (2025 CAD m)',
  equity_purchase_consideration_m = 'Equity purchase consideration (2025 CAD m)',
  new_acquisition_debt_m = 'New acquisition borrowing (2025 CAD m)',
  buyer_equity_contribution_m = 'Buyer equity contribution (2025 CAD m)',
  total_gross_debt_m = 'Total gross debt after purchase (2025 CAD m)',
  actual_total_gross_debt_to_sale_value = 'Post-purchase gross debt / EV (%)'
)
assert_cols(financing, c('scenario_id','airport','year','period',
                        'cpi_factor','purchase_multiple',
                        'acquisition_debt_share','acquisition_interest_rate',
                        'revenue_m','ebitda_m','ground_rent_m',
                        'purchase_adjusted_pre_tax_income_m',
                        bridge_fields, 'actual_total_gross_debt_to_sale_value'),
            'Acquisition financing CSV')
assert_cols(profitability, c('scenario_id','airport','year',
  'threshold_id','statutory_tax_rate','required_pre_tax_margin',
  'required_revenue_m'), 'Acquisition profitability CSV')

finance_rows <- financing |>
  filter(airport %in% expected_airports) |>
  left_join(assumptions |>
    select(scenario_name, purchase_multiple, acquisition_debt_share,
           acquisition_interest_rate),
    by = c('purchase_multiple','acquisition_debt_share',
           'acquisition_interest_rate')) |>
  mutate(
    scenario = scenario_name,
    period_label = period_name(year),
    # Annual source stocks were reported at that year's prices.
    # Pooled stocks already reflect CPI-adjusted means in 2025 CAD.
    factor_2025 = if_else(is.na(year), 1.0, cpi_factor),
    across(all_of(bridge_fields), ~ .x * factor_2025)
  )
if (nrow(finance_rows) != 48L ||
    anyNA(finance_rows[c(panel_keys, bridge_fields)]) ||
    anyDuplicated(finance_rows[panel_keys]) ||
    any(finance_rows$estimated_sale_value_m <= 0)) {
  stop('Invalid, unmatched or duplicated acquisition financing bridge.',
       call. = FALSE)
}

# Independent financing/purchase reconciliation before display.
is_close <- function(a, b, tol = 1e-5) {
  all(is.finite(a) & is.finite(b) &
      abs(a - b) <= tol * pmax(1, abs(a), abs(b)))
}
if (!is_close(finance_rows$estimated_sale_value_m,
              finance_rows$valuation_ebitda_m * finance_rows$purchase_multiple) ||
    !is_close(finance_rows$equity_purchase_consideration_m,
              finance_rows$estimated_sale_value_m -
              finance_rows$gross_debt_m + finance_rows$cash_m) ||
    !is_close(finance_rows$new_acquisition_debt_m,
              finance_rows$equity_purchase_consideration_m *
              finance_rows$acquisition_debt_share) ||
    !is_close(finance_rows$buyer_equity_contribution_m,
              finance_rows$equity_purchase_consideration_m -
              finance_rows$new_acquisition_debt_m) ||
    !is_close(finance_rows$total_gross_debt_m,
              finance_rows$gross_debt_m + finance_rows$new_acquisition_debt_m) ||
    !is_close(finance_rows$actual_total_gross_debt_to_sale_value,
              finance_rows$total_gross_debt_m /
              finance_rows$estimated_sale_value_m)) {
  stop('Acquisition financing bridge fails a transaction/debt check.',
       call. = FALSE)
}
bridge <- finance_rows |>
  select(all_of(panel_keys), all_of(names(bridge_headers)))
names(bridge)[-(1:3)] <- unname(bridge_headers)

# Sensitivity calculations extend the optional 2025 CENTRAL appendix to all
# financing cases and years, applying the EXACT SAME model formula but ONLY
# to the 15% after-tax margin. No change to the acquisition model itself.
# (An optional audit appendix need not be enabled for this integration.)
profit15 <- profitability |>
  filter(airport %in% expected_airports, threshold_id == 'net_15pct') |>
  select(scenario_id, airport, year, statutory_tax_rate,
         required_pre_tax_margin, required_revenue_m)
sensitivity_base <- finance_rows |>
  left_join(profit15, by = c('scenario_id','airport','year')) |>
  mutate(
    operating_cost_base_m = revenue_m - ebitda_m - ground_rent_m,
    target_pre_tax_m = required_pre_tax_margin * revenue_m,
    baseline_15_m = pmax(0, target_pre_tax_m -
      purchase_adjusted_pre_tax_income_m) /
      (1 - required_pre_tax_margin),
    conversion_80_m = pmax(0, target_pre_tax_m -
      purchase_adjusted_pre_tax_income_m) /
      (0.80 - required_pre_tax_margin),
    savings_10_m = 0.10 * operating_cost_base_m,
    saving_10_m = pmax(0, target_pre_tax_m -
      purchase_adjusted_pre_tax_income_m - savings_10_m) /
      (1 - required_pre_tax_margin),
    combined_m = pmax(0, target_pre_tax_m -
      purchase_adjusted_pre_tax_income_m - savings_10_m) /
      (0.80 - required_pre_tax_margin)
  )
if (nrow(sensitivity_base) != 48L ||
    anyNA(sensitivity_base[c(panel_keys, 'required_pre_tax_margin',
      'operating_cost_base_m','baseline_15_m','conversion_80_m',
      'saving_10_m','combined_m')]) ||
    any(sensitivity_base$operating_cost_base_m < -1e-7) ||
    any(sensitivity_base$required_pre_tax_margin >= 0.80) ||
    !is_close(sensitivity_base$baseline_15_m,
              sensitivity_base$required_revenue_m, tol = 1e-6)) {
  stop('15% net-margin sensitivity fails coverage, feasibility or baseline check.',
       call. = FALSE)
}

sensitivity_headers <- c(
  conversion_80_m = '15% margin: 80% conversion',
  saving_10_m = '15% margin: 10% cost saving',
  combined_m = '15% margin: combined'
)
sensitivity_values <- sensitivity_base |>
  mutate(across(all_of(names(sensitivity_headers)),
                ~ .x * factor_2025)) |>
  select(all_of(panel_keys), all_of(names(sensitivity_headers)),
         passengers)
sensitivity_amounts <- sensitivity_values |>
  select(-passengers)
sensitivity_per_pax <- sensitivity_values |>
  mutate(across(all_of(names(sensitivity_headers)),
                ~ .x * 1e6 / passengers)) |>
  select(-passengers)
names(sensitivity_amounts)[-(1:3)] <- unname(sensitivity_headers)
names(sensitivity_per_pax)[-(1:3)] <- unname(sensitivity_headers)

# Audit the baseline 15%-margin requirement against the exact published
# inflation-adjusted acquisition comparison export.
published_15 <- long_panel |>
  filter(variable == 'cf_additional_revenue_net_15pct') |>
  select(all_of(panel_keys),
         baseline_amount_m = amount_cad_2025_m)
baseline_check <- sensitivity_base |>
  left_join(published_15, by = panel_keys)
if (nrow(baseline_check) != 48L || anyNA(baseline_check$baseline_amount_m) ||
    !is_close(baseline_check$baseline_15_m *
                baseline_check$factor_2025,
              baseline_check$baseline_amount_m)) {
  stop('Sensitivity baseline does not reconcile to 15% margin export.',
       call. = FALSE)
}

make_panel <- function(value_col, sensitivity) {
  flows <- long_panel |>
    select(all_of(panel_keys), variable, all_of(value_col)) |>
    pivot_wider(names_from = variable, values_from = all_of(value_col))
  panel <- bridge |>
    left_join(flows, by = panel_keys) |>
    left_join(sensitivity, by = panel_keys) |>
    arrange(airport, factor(period_label,
      levels = c('2023','2024','2025','Pooled 2023–25')),
      factor(scenario, levels = scenarios)) |>
    select(all_of(panel_keys), all_of(unname(bridge_headers)),
           all_of(names(metric_names)), all_of(unname(sensitivity_headers)))
  if (nrow(panel) != 48L || anyNA(panel) ||
      anyDuplicated(panel[panel_keys])) {
    stop('Incomplete joined panel: ', value_col, call. = FALSE)
  }
  names(panel)[match(names(metric_names), names(panel))] <-
    unname(metric_names)
  names(panel)[1:3] <- c('Airport','Period','Scenario')
  panel
}
panel_amounts <- make_panel('amount_cad_2025_m', sensitivity_amounts)
panel_per_pax <- make_panel('per_passenger_cad_2025', sensitivity_per_pax)
stopifnot(identical(panel_amounts[1:3], panel_per_pax[1:3]))

# The workbook builder has already added the ten original worksheets.
# Starting from that object, rather than reopening its saved XLSX, avoids
# exposing existing formulas and sheet references to a load/save round trip.
wb <- comparison_wb
old_sheets <- names(wb)
if (length(old_sheets) != 10L ||
    !all(c('Total value comparison', 'Per-passenger comparison') %in% old_sheets)) {
  stop('Unexpected documented financial workbook structure.', call. = FALSE)
}

# Source valuation/stock columns are always in 2025 CAD millions.
# Only the operating-flow and sensitivity columns change units by sheet.
write_acquisition_panel <- function(sheet, panel, unit_label, pooled_note) {
  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
  n <- ncol(panel)
  bridge_start <- 4L
  bridge_end <- 3L + length(bridge_headers)
  flow_start <- bridge_end + 1L
  flow_end <- flow_start + length(metric_names) - 1L
  sens_start <- flow_end + 1L

  openxlsx::writeData(wb, sheet,
    paste0('Illustrative acquisition | ', unit_label), startRow = 1)
  openxlsx::writeData(wb, sheet,
    paste0('Independent annual purchase scenarios; ', pooled_note,
           '. Valuation, debt, cash and purchase-financing columns ',
           'are ALWAYS millions of 2025 CAD.'), startRow = 2)
  openxlsx::writeData(wb, sheet,
    paste0('The three rightmost columns vary ONLY the 15% after-tax margin ',
      'target: 80% conversion, 10% operating-cost savings, or both. ',
      'Revenue requirements are hypothetical, not observed costs or fees.'),
    startRow = 3)
  openxlsx::writeData(wb, sheet, panel, startRow = 5, withFilter = TRUE)
  for (rr in 1:3) {
    openxlsx::mergeCells(wb, sheet, cols = 1:n, rows = rr)
  }
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(
    fontSize = 14, textDecoration = 'bold', fontColour = '#1B3854'),
    rows = 1, cols = 1)
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(
    fontColour = '#455668', wrapText = TRUE, valign = 'center'),
    rows = 2:3, cols = 1)
  openxlsx::setRowHeights(wb, sheet, rows = 1:3,
                          heights = c(27, 32, 38))
  header_style <- function(fill) openxlsx::createStyle(
    textDecoration = 'bold', fontColour = 'white', fgFill = fill,
    wrapText = TRUE, valign = 'center')
  openxlsx::addStyle(wb, sheet, header_style('#244665'),
    rows = 5, cols = 1:3, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet, header_style('#3E6585'),
    rows = 5, cols = bridge_start:bridge_end, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet, header_style('#244665'),
    rows = 5, cols = flow_start:flow_end, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet, header_style('#586C80'),
    rows = 5, cols = sens_start:n, gridExpand = TRUE)
  openxlsx::setRowHeights(wb, sheet, rows = 5, heights = 66)

  data_rows <- 6:(nrow(panel) + 5L)
  num_style <- openxlsx::createStyle(
    numFmt = '#,##0.00;[Red](#,##0.00);"-"')
  openxlsx::addStyle(wb, sheet, num_style, rows = data_rows,
    cols = bridge_start:n, gridExpand = TRUE)
  percent_names <- c('New debt share (%)', 'Acquisition rate (%)',
                    'Post-purchase gross debt / EV (%)')
  for (nm in percent_names) {
    openxlsx::addStyle(wb, sheet,
      openxlsx::createStyle(numFmt = '0.0%'),
      rows = data_rows, cols = match(nm, names(panel)),
      gridExpand = TRUE, stack = TRUE)
  }
  openxlsx::setColWidths(wb, sheet, cols = 1:3,
                         widths = c(12, 20, 16))
  openxlsx::setColWidths(wb, sheet,
    cols = bridge_start:bridge_end, widths = 22)
  openxlsx::setColWidths(wb, sheet,
    cols = flow_start:flow_end, widths = 24)
  openxlsx::setColWidths(wb, sheet,
    cols = sens_start:n, widths = 25)
  openxlsx::freezePane(wb, sheet, firstActiveRow = 6,
                       firstActiveCol = 4)
}

amount_sheet <- 'Acquisition total amounts'
pax_sheet <- 'Acquisition per passenger'
notes_sheet <- 'Acquisition assumptions'

write_acquisition_panel(
  amount_sheet, panel_amounts, 'millions of 2025 CAD',
  'pooled values are TOTAL 2023–25 flows (not annual averages)'
)
write_acquisition_panel(
  pax_sheet, panel_per_pax, '2025 CAD per passenger',
  'pooled values divide three-year flows by three-year passenger totals'
)

openxlsx::addWorksheet(wb, notes_sheet, gridLines = FALSE)
assumption_table <- assumptions |>
  transmute(Scenario = scenario_name,
            `Purchase multiple (x)` = purchase_multiple,
            `New debt share (%)` = 100 * acquisition_debt_share,
            `Interest rate (%)` = 100 * acquisition_interest_rate)
openxlsx::writeData(wb, notes_sheet,
  'Acquisition assumption packages | illustrative, not forecasts',
  startRow = 1)
openxlsx::writeData(wb, notes_sheet, assumption_table,
                    startRow = 3, withFilter = TRUE)
openxlsx::addStyle(wb, notes_sheet, openxlsx::createStyle(
  fontSize = 14, textDecoration = 'bold', fontColour = '#1B3854'),
  rows = 1, cols = 1)
openxlsx::addStyle(wb, notes_sheet, openxlsx::createStyle(
  textDecoration = 'bold', fontColour = 'white', fgFill = '#244665'),
  rows = 3, cols = 1:4, gridExpand = TRUE)
openxlsx::setColWidths(wb, notes_sheet, cols = 1:4,
                       widths = c(19, 24, 23, 22))

method_notes <- data.frame(
  Topic = c('Source model', 'Absolute-amount units', 'Per-passenger units',
            'Acquisition-adjusted amounts', 'Net financing pressure',
            'Revenue requirements', 'Pooled years',
            'Financing scenarios', 'Valuation and debt bridge',
            '15% sensitivities', 'Costs and reinvestment', 'Comparable prices'),
  Interpretation = c(
    'Acquisition counterfactual model; inputs from the documented Canadian–Australian comparison and Canadian airport debt/cash supplement. The same unversioned CSV export filenames are used for every run.',
    'Acquisition total amounts uses millions of 2025 CAD. Annual operating lines use inflation-adjusted one-year flows; pooled operating lines use inflation-adjusted THREE-YEAR TOTAL flows. Stock/transaction columns use annual snapshots or pooled 2025-CAD stock averages.',
    'Acquisition per passenger divides ONLY the operating-flow and revenue-requirement columns by passengers. Valuation, debt, cash and equity columns remain millions of 2025 CAD and do not turn into per-passenger stocks.',
    'Reported existing net financing costs remain in place. Post-acquisition financing costs add the new borrowing interest. Post-acquisition pretax income reflects both ground-rent elimination and new acquisition interest.',
    'Net added financing pressure = additional interest on acquisition debt MINUS eliminated federal ground rent. It can be negative when rent savings exceed new interest.',
    'Break-even, restoration and four private net-margin targets are separate, independently solved scenarios. Extra revenue is a lower-bound illustration under 100% conversion to extra pretax profit, not an observed expense or actual airport fee.',
    'Pooled revenue requirements are independently modelled on 2023–25 data. They need not equal the sum of the independently solved annual requirements.',
    'Low-cost 20x/50%/4.5%; Central 22.5x/60%/5.5%; High-cost 25x/70%/6.5%. No factorial cross-products are calculated.',
    'The bridge displays enterprise value, existing gross debt and cash, purchase consideration, buyer debt/equity funding and consolidated gross debt. These are 2025-CAD stock/transaction values on BOTH sheets (not dollars per passenger).',
    'Three sensitivity columns change ONLY the 15% net-margin requirement. The operating-cost base is revenue minus EBITDA minus federal ground rent (so rent is not saved twice). Conversion is 100% in the baseline, 80% in alternatives; cost savings are 10% of the cost base. The model used for these sensitivities is extended across all years and three financing cases.',
    'No investor cash-flow/IRR, principal amortization, maintenance capital, demand response or post-sale productivity gains beyond ground-rent elimination are included in the main financing model.',
    'Canadian calendar 2025 vs Australian financial year 2024–25, as documented in the original comparison workbook.'
  ), stringsAsFactors = FALSE)
openxlsx::writeData(wb, notes_sheet, method_notes, startRow = 9)
openxlsx::addStyle(wb, notes_sheet,
  openxlsx::createStyle(wrapText = TRUE, valign = 'center'),
  rows = 10:(9 + nrow(method_notes)), cols = 1:2, gridExpand = TRUE)
openxlsx::setColWidths(wb, notes_sheet, cols = 1, widths = 31)
openxlsx::setColWidths(wb, notes_sheet, cols = 2, widths = 115)
openxlsx::setRowHeights(wb, notes_sheet,
                        rows = 10:(9 + nrow(method_notes)), heights = 39)
openxlsx::freezePane(wb, notes_sheet, firstActiveRow = 3)

# Put each hypothetical panel immediately after its observed equivalent.
# The acquisition notes follow the new per-passenger panel.
new_sheets <- c(amount_sheet, pax_sheet, notes_sheet)
sheet_order <- character()
for (nm in old_sheets) {
  sheet_order <- c(sheet_order, nm)
  if (nm == 'Total value comparison') {
    sheet_order <- c(sheet_order, amount_sheet)
  }
  if (nm == 'Per-passenger comparison') {
    sheet_order <- c(sheet_order, pax_sheet, notes_sheet)
  }
}
if (length(sheet_order) != length(names(wb)) ||
    anyNA(match(sheet_order, names(wb))) ||
    anyDuplicated(sheet_order)) {
  stop('Could not establish final workbook worksheet order.', call. = FALSE)
}
worksheetOrder(wb) <- match(sheet_order, names(wb))
openxlsx::saveWorkbook(wb, combined_book, overwrite = TRUE)
message('Saved documented workbook with acquisition total amounts and ',
        'per-passenger rates: ', combined_book)
message('Added sheets: ', paste(new_sheets, collapse = ' | '),
        '; ', nrow(panel_amounts), ' airport/period/scenario rows each.')
