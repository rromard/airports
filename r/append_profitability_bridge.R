# Profitability bridge | Canada versus Australian EBITDA benchmarks
# ---------------------------------------------------------------------
# Run only from append_acquisition_audits.R, after the validated model has
# been converted to 2025 CAD. Requires: wb, model, profitability_nine,
# final_totals, out_dir, and original acquisition workbook formatting styles.
#
# This module adds a 20-row Excel presentation controlled by 3 selectors and
# a 540-row formula calculation sheet. It also exports independent R results
# for every financing scenario, objective, airport, period and benchmark mode.
# No financing, tax or profitability-target solution is recomputed here.

# EDIT HERE when preparing alternative Datawrapper / chart-ready datasets.
bridge_chart_metrics <- c(
  observed_ebitda_margin_pct = 'Observed Canadian EBITDA margin',
  rent_adjusted_ebitda_margin_pct = 'Canadian EBITDA margin, excluding rent',
  australian_ebitda_margin_pct = 'Australian weighted EBITDA benchmark',
  counterfactual_ebitda_margin_pct = 'Scenario-adjusted EBITDA margin',
  counterfactual_ebtda_margin_pct = 'Scenario-adjusted EBTDA margin',
  acquisition_required_revenue_m = 'Revenue needed for selected private objective',
  benchmark_required_revenue_m = 'Revenue needed for Australian EBITDA margin',
  additional_revenue_gap_m = 'Private objective less operating benchmark, CAD m',
  additional_revenue_gap_per_pax_cad = 'Private objective less operating benchmark, CAD per passenger'
)
bridge_export_chart_long <- TRUE

bridge_sheet <- 'Profitability bridge'
bridge_calc_sheet <- 'Bridge calculations'
bridge_modes <- c('Pooled 2023-25', 'Year-matched')
bridge_output_dir <- file.path(out_dir, 'profitability_bridge')
dir.create(bridge_output_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(!any(c(bridge_sheet, bridge_calc_sheet) %in% names(wb)))

required_compare <- c('country', 'airport', 'year', 'period_type',
                      'ebitda', 'ebitda_ebit_revenue_basis')
if (!all(required_compare %in% names(final_totals))) {
  stop('Comparison workbook lacks the matched EBITDA/revenue benchmark fields.',
       call. = FALSE)
}
# All financial amounts in final_totals have already been expressed in 2025
# CAD. Use the actual EBITDA-matched operating revenue denominator, not
# SCACH's separate Sydney net-profit revenue basis.
bridge_australia <- final_totals |>
  filter(country == 'Australia') |>
  mutate(year_label = if_else(is.na(year), '2023-25',
                              as.character(year))) |>
  group_by(year_label) |>
  summarise(
    n_airports = n_distinct(airport),
    ebitda_m = sum(ebitda),
    matched_revenue_m = sum(ebitda_ebit_revenue_basis),
    .groups = 'drop'
  ) |>
  mutate(benchmark_margin = ebitda_m / matched_revenue_m) |>
  arrange(factor(year_label, levels = c('2023','2024','2025','2023-25')))
if (nrow(bridge_australia) != 4L ||
    !setequal(bridge_australia$year_label, c('2023','2024','2025','2023-25')) ||
    any(bridge_australia$n_airports != 4L) ||
    anyNA(bridge_australia) ||
    any(!is.finite(bridge_australia$benchmark_margin)) ||
    any(bridge_australia$benchmark_margin <= 0 |
        bridge_australia$benchmark_margin >= 1)) {
  stop('Incomplete Australian four-airport weighted EBITDA benchmark.',
       call. = FALSE)
}
bridge_pooled_margin <- bridge_australia$benchmark_margin[
  bridge_australia$year_label == '2023-25']
# Verify the 3-year ratio is calculated from flows rather than a mean
# of individual airport ratios or a mean of percentages.
annual_bench <- bridge_australia |>
  filter(year_label != '2023-25')
if (abs(sum(annual_bench$ebitda_m) /
        sum(annual_bench$matched_revenue_m) - bridge_pooled_margin) > 1e-8) {
  stop('Australian pooled EBITDA ratio does not reconcile to annual flows.',
       call. = FALSE)
}
readr::write_csv(bridge_australia,
  file.path(bridge_output_dir, 'australian_ebitda_benchmarks.csv'))

bridge_keys <- c('scenario_id', 'airport', 'period', 'year_label')
if (!all(c('ebitda_m', 'ground_rent_m', 'net_financing_cost_m',
           'additional_acquisition_interest_m', 'passengers') %in% names(model)) ||
    !all(c('operating_savings_m', 'incremental_revenue_to_profit',
           'required_revenue_m', 'achieved_net_margin') %in%
         names(profitability_nine))) {
  stop('Acquisition scenario data is missing bridge financial inputs.',
       call. = FALSE)
}
bridge_model_financing <- model |>
  select(all_of(bridge_keys), ebitda_m, ground_rent_m,
         net_financing_cost_m, additional_acquisition_interest_m,
         observed_revenue_check_m = revenue_m,
         observed_passenger_check = passengers)
bridge_inputs <- profitability_nine |>
  left_join(bridge_model_financing, by = bridge_keys) |>
  left_join(bridge_australia |>
    select(year_label, benchmark_matching = benchmark_margin),
    by = 'year_label') |>
  mutate(
    benchmark_pooled = bridge_pooled_margin,
    scenario_name = case_when(
      purchase_multiple == 20 ~ 'Low-cost',
      purchase_multiple == 22.5 ~ 'Central',
      purchase_multiple == 25 ~ 'High-cost',
      TRUE ~ NA_character_
    ),
    base_operating_ebitda_m = ebitda_m + ground_rent_m +
      operating_savings_m,
    counterfactual_ebitda_m = base_operating_ebitda_m +
      incremental_revenue_to_profit * required_revenue_m,
    counterfactual_ebtda_m = counterfactual_ebitda_m -
      net_financing_cost_m - additional_acquisition_interest_m,
    counterfactual_ebitda_margin_pct =
      100 * counterfactual_ebitda_m / counterfactual_revenue_m,
    counterfactual_ebtda_margin_pct =
      100 * counterfactual_ebtda_m / counterfactual_revenue_m,
    observed_ebitda_margin_pct = 100 * ebitda_m / revenue_m,
    rent_adjusted_ebitda_margin_pct =
      100 * (ebitda_m + ground_rent_m) / revenue_m,
    purchase_adjusted_ebtda_margin_pct =
      100 * (ebitda_m + ground_rent_m -
        net_financing_cost_m - additional_acquisition_interest_m) / revenue_m
  )
if (nrow(bridge_inputs) != 540L ||
    anyDuplicated(bridge_inputs[c(bridge_keys, 'threshold_id')]) ||
    anyNA(bridge_inputs[c('scenario_name', 'benchmark_matching',
                          'benchmark_pooled', 'base_operating_ebitda_m',
                          'counterfactual_ebitda_m')]) ||
    any(abs(bridge_inputs$revenue_m -
            bridge_inputs$observed_revenue_check_m) > 1e-5) ||
    any(bridge_inputs$passengers != bridge_inputs$observed_passenger_check) ||
    any(abs(bridge_inputs$counterfactual_revenue_m -
            (bridge_inputs$revenue_m + bridge_inputs$required_revenue_m)) > 1e-5) ||
    any(abs(bridge_inputs$counterfactual_pre_tax_income_m -
            (bridge_inputs$purchase_adjusted_pre_tax_income_m +
             bridge_inputs$operating_savings_m +
             bridge_inputs$incremental_revenue_to_profit *
               bridge_inputs$required_revenue_m)) > 1e-5)) {
  stop('Acquisition-to-operating bridge reconciliation failed.', call. = FALSE)
}
# Check the independently compiled per-unit comparison against the acquisition
# model before comparing the countries (both use the same 2025 CAD base).
canadian_check <- bridge_inputs |>
  filter(airport %in% can_airports, scenario_name == 'Central',
         threshold_id == 'net_15pct') |>
  left_join(final_totals |>
    filter(country == 'Canada') |>
    mutate(year_label = if_else(is.na(year), '2023-25',
                                as.character(year))) |>
    select(airport, year_label, reported_comparison_ebitda_m = ebitda,
           reported_comparison_revenue_m = ebitda_ebit_revenue_basis),
    by = c('airport', 'year_label'))
if (nrow(canadian_check) != 16L ||
    anyNA(canadian_check[c('reported_comparison_ebitda_m',
                           'reported_comparison_revenue_m')]) ||
    any(abs(canadian_check$ebitda_m -
            canadian_check$reported_comparison_ebitda_m) > 1e-4) ||
    any(abs(canadian_check$revenue_m -
            canadian_check$reported_comparison_revenue_m) > 1e-4)) {
  stop('Canadian acquisition and comparison EBITDA/revenue do not reconcile.',
       call. = FALSE)
}

# Keep the base data at one row per original acquisition case. Generate
# the TWO benchmark choices in the CSV, so charting is independent of
# whichever selector is currently chosen in the Excel presentation.
bridge_cases <- bind_rows(
  bridge_inputs |> mutate(benchmark_mode = bridge_modes[[1L]],
                          benchmark_margin = benchmark_pooled),
  bridge_inputs |> mutate(benchmark_mode = bridge_modes[[2L]],
                          benchmark_margin = benchmark_matching)
) |>
  mutate(
    benchmark_denominator = incremental_revenue_to_profit -
      benchmark_margin,
    benchmark_required_revenue_m = if_else(
      benchmark_denominator > 0,
      pmax(0, (benchmark_margin * revenue_m -
                base_operating_ebitda_m) / benchmark_denominator),
      if_else(base_operating_ebitda_m >= benchmark_margin * revenue_m,
              0, NA_real_)
    )
  )
# A BIG4 revenue solution is a sum of four airport-level zero-floored
# requirements. Solving once on BIG4's aggregate operating costs would
# hide airport-specific shortfalls.
big4_benchmark_sums <- bridge_cases |>
  filter(airport %in% can_airports) |>
  group_by(scenario_id, year_label, threshold_id, benchmark_mode) |>
  summarise(airport_sum = if (all(!is.na(benchmark_required_revenue_m)))
    sum(benchmark_required_revenue_m) else NA_real_,
    .groups = 'drop')
bridge_cases <- bridge_cases |>
  left_join(big4_benchmark_sums,
    by = c('scenario_id','year_label','threshold_id','benchmark_mode')) |>
  mutate(
    benchmark_required_revenue_m = if_else(
      airport == 'BIG4', airport_sum, benchmark_required_revenue_m),
    benchmark_required_revenue_per_pax_cad =
      benchmark_required_revenue_m * 1e6 / passengers,
    acquisition_required_revenue_m = required_revenue_m,
    acquisition_required_revenue_per_pax_cad =
      required_revenue_m * 1e6 / passengers,
    additional_revenue_gap_m =
      acquisition_required_revenue_m - benchmark_required_revenue_m,
    additional_revenue_gap_per_pax_cad =
      additional_revenue_gap_m * 1e6 / passengers,
    australian_ebitda_margin_pct = 100 * benchmark_margin,
    remaining_operating_margin_gap_pp =
      australian_ebitda_margin_pct - rent_adjusted_ebitda_margin_pct
  ) |>
  select(-benchmark_denominator, -airport_sum)
if (nrow(bridge_cases) != 1080L ||
    anyNA(bridge_cases[c('benchmark_required_revenue_m',
                         'additional_revenue_gap_m')]) ||
    any(bridge_cases$benchmark_required_revenue_m < 0) ||
    anyDuplicated(bridge_cases[
      c(bridge_keys,'threshold_id','benchmark_mode')])) {
  stop('Incomplete 1080-case Australian acquisition benchmark panel.',
       call. = FALSE)
}
# Explicitly defined, chart-ready long CSV. Add or remove names at the top of
# this script to change the metric selection without editing the model.
bridge_export <- bridge_cases |>
  mutate(country = 'Canada') |>
  select(country, airport, year_label, period, scenario_id, scenario_name,
    threshold_id, profitability_objective, target_type, benchmark_mode,
    passengers, benchmark_margin, australian_ebitda_margin_pct,
    observed_ebitda_margin_pct, rent_adjusted_ebitda_margin_pct,
    purchase_adjusted_ebtda_margin_pct,
    counterfactual_ebitda_margin_pct, counterfactual_ebtda_margin_pct,
    acquisition_required_revenue_m, acquisition_required_revenue_per_pax_cad,
    benchmark_required_revenue_m, benchmark_required_revenue_per_pax_cad,
    additional_revenue_gap_m, additional_revenue_gap_per_pax_cad,
    remaining_operating_margin_gap_pp,
    observed_revenue_m = revenue_m, observed_ebitda_m = ebitda_m,
    ground_rent_saving_m = ground_rent_m,
    existing_net_financing_m = net_financing_cost_m,
    new_acquisition_interest_m = additional_acquisition_interest_m,
    operating_savings_m, incremental_revenue_to_profit,
    counterfactual_revenue_m, counterfactual_ebitda_m,
    counterfactual_ebtda_m, achieved_net_margin, target_net_margin)
readr::write_csv(bridge_export,
  file.path(bridge_output_dir, 'profitability_bridge_scenarios_long.csv'))
if (bridge_export_chart_long) {
  if (!all(names(bridge_chart_metrics) %in% names(bridge_export))) {
    stop('Unknown selected Datawrapper bridge metric.', call. = FALSE)
  }
  chart_export <- bridge_export |>
    select(airport, year_label, period, scenario_name, threshold_id,
           profitability_objective, benchmark_mode,
           all_of(names(bridge_chart_metrics))) |>
    pivot_longer(all_of(names(bridge_chart_metrics)),
      names_to = 'metric', values_to = 'value') |>
    mutate(
      metric_label = unname(bridge_chart_metrics[metric]),
      unit = case_when(
        grepl('_per_pax_cad$', metric) ~ '2025 CAD per passenger',
        grepl('_pct$', metric) ~ 'percent',
        grepl('_pp$', metric) ~ 'percentage points',
        TRUE ~ '2025 CAD millions'
      )
    ) |>
    select(airport, year_label, scenario_name, threshold_id,
           profitability_objective, benchmark_mode, metric,
           metric_label, value, unit, period)
  readr::write_csv(chart_export,
    file.path(bridge_output_dir, 'profitability_bridge_chart_long.csv'))
}

# Excel formula table: one row for each unique original model case (540).
# Changing the benchmark selector updates the formula results, without
# executing R or touching the separately audited acquisition model.
bridge_options <- bridge_inputs |>
  filter(scenario_name == 'Central', airport == 'YYC',
         year_label == '2023-25') |>
  arrange(threshold_order) |>
  pull(profitability_objective)
stopifnot(length(bridge_options) == 9L, !anyDuplicated(bridge_options))
calc_input <- bridge_inputs |>
  arrange(factor(scenario_name, levels = c('Low-cost','Central','High-cost')),
          factor(year_label, levels = c('2023','2024','2025','2023-25')),
          factor(airport, levels = c(can_airports,'BIG4')),
          threshold_order) |>
  transmute(
    Key = paste(airport, year_label, scenario_name,
                profitability_objective, sep = '|'),
    Airport = airport, Period = year_label, Financing = scenario_name,
    Objective = profitability_objective, Target = target_type,
    Passengers = passengers, Revenue = revenue_m, EBITDA = ebitda_m,
    Rent = ground_rent_m, Existing_finance = net_financing_cost_m,
    New_interest = additional_acquisition_interest_m,
    Conversion = incremental_revenue_to_profit,
    Savings = operating_savings_m, Required_revenue = required_revenue_m,
    Target_net = target_net_margin, Achieved_net = achieved_net_margin,
    Australian_pooled = benchmark_pooled,
    Australian_matched = benchmark_matching
  )
stopifnot(nrow(calc_input) == 540L, !anyDuplicated(calc_input$Key),
          ncol(calc_input) == 19L)
calc_output_headers <- c(
  'Base EBITDA after savings', 'Scenario total revenue', 'Scenario EBITDA',
  'Scenario EBITDA margin', 'Scenario EBTDA', 'Scenario EBTDA margin',
  'Selected benchmark margin', 'Benchmark revenue requirement',
  'Benchmark revenue per passenger', 'Revenue gap versus benchmark',
  'Revenue gap per passenger', 'Observed EBITDA margin',
  'Rent-adjusted EBITDA margin', 'Purchase-adjusted EBTDA margin',
  'Benchmark gap versus rent-adjusted (pp)'
)
calc_data <- as.data.frame(calc_input)
for (nm in calc_output_headers) calc_data[[nm]] <- NA_real_
openxlsx::addWorksheet(wb, bridge_calc_sheet, gridLines = FALSE)
openxlsx::writeData(wb, bridge_calc_sheet,
  'Profitability bridge | transparent 2025-CAD formula calculations',
  startRow = 1)
openxlsx::writeData(wb, bridge_calc_sheet,
  '540 unique model cases. Input columns A:S come from validated R; columns T:AH are Excel formulas. Australian benchmark follows the selector on Profitability bridge.',
  startRow = 2)
openxlsx::writeDataTable(wb, bridge_calc_sheet, calc_data, startRow = 8,
  tableName = 'BridgeCalculationInputs', tableStyle = 'TableStyleMedium2')
calc_first <- 9L
calc_last <- calc_first + nrow(calc_data) - 1L
rr <- calc_first:calc_last
cell <- function(letter, r) paste0(letter, r)
formulas <- list(
  T = function(r) sprintf('I%d+J%d+N%d',r,r,r),
  U = function(r) sprintf('H%d+O%d',r,r),
  V = function(r) sprintf('T%d+M%d*O%d',r,r,r),
  W = function(r) sprintf('IF(U%d>0,V%d/U%d,NA())',r,r,r),
  X = function(r) sprintf('V%d-K%d-L%d',r,r,r),
  Y = function(r) sprintf('IF(U%d>0,X%d/U%d,NA())',r,r,r),
  Z = function(r) sprintf('IF(%s!$B$5="Pooled 2023-25",R%d,S%d)',
                         paste0("'", bridge_sheet, "'"),r,r),
  AA = function(r) {
    if (calc_input$Airport[[r-calc_first+1L]] == 'BIG4') {
      # Do not use SUMIFS on the complete AA column: that would include
      # the BIG4 cell itself in its dependency range and be circular.
      i <- r - calc_first + 1L
      constituents <- which(
        calc_input$Airport %in% can_airports &
        calc_input$Period == calc_input$Period[[i]] &
        calc_input$Financing == calc_input$Financing[[i]] &
        calc_input$Objective == calc_input$Objective[[i]])
      stopifnot(length(constituents) == 4L)
      paste0('SUM(', paste0('AA', calc_first + constituents - 1L,
                            collapse = ','), ')')
    } else {
      sprintf('IF(M%d>Z%d,MAX(0,(Z%d*H%d-T%d)/(M%d-Z%d)),IF(T%d>=Z%d*H%d,0,NA()))',
              r,r,r,r,r,r,r,r,r,r)
    }
  },
  AB = function(r) sprintf('AA%d*1000000/G%d',r,r),
  AC = function(r) sprintf('O%d-AA%d',r,r),
  AD = function(r) sprintf('AC%d*1000000/G%d',r,r),
  AE = function(r) sprintf('I%d/H%d',r,r),
  AF = function(r) sprintf('(I%d+J%d)/H%d',r,r,r),
  AG = function(r) sprintf('(I%d+J%d-K%d-L%d)/H%d',r,r,r,r,r),
  AH = function(r) sprintf('(Z%d-AF%d)*100',r,r)
)
stopifnot(length(formulas) == length(calc_output_headers))
for (colname in names(formulas)) {
  openxlsx::writeFormula(wb, bridge_calc_sheet,
    x = vapply(rr, formulas[[colname]], character(1)),
    startCol = match(colname, vapply(seq_len(ncol(calc_data)),
      openxlsx::int2col, character(1))), startRow = calc_first)
}
openxlsx::setColWidths(wb, bridge_calc_sheet, cols = 1, widths = 67)
openxlsx::setColWidths(wb, bridge_calc_sheet, cols = 2:6,
  widths = c(12,13,15,45,18))
openxlsx::setColWidths(wb, bridge_calc_sheet, cols = 7:34, widths = 21)
openxlsx::addStyle(wb, bridge_calc_sheet, pct_style,
  rows = rr, cols = c(13,16,17,18,19,23,25,26,31,32,33),
  gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, bridge_calc_sheet, per_pax_style,
  rows = rr, cols = c(28,30), gridExpand = TRUE, stack = TRUE)
openxlsx::setRowHeights(wb, bridge_calc_sheet, rows = 8, heights = 60)
openxlsx::freezePane(wb, bridge_calc_sheet,
  firstActiveRow = calc_first, firstActiveCol = 7)

# Main 20-row interactive table. All measured results are XLOOKUP formulas
# pointing to the calculation sheet, and refresh when a selector is changed.
openxlsx::addWorksheet(wb, bridge_sheet, gridLines = FALSE)
openxlsx::writeData(wb, bridge_sheet,
  'Canadian airport profitability | Australia benchmark and private acquisition bridge',
  startRow = 1)
openxlsx::writeData(wb, bridge_sheet,
  'Finance case', startRow = 3, startCol = 1)
openxlsx::writeData(wb, bridge_sheet,
  'Profitability objective', startRow = 4, startCol = 1)
openxlsx::writeData(wb, bridge_sheet,
  'Australian benchmark', startRow = 5, startCol = 1)
openxlsx::writeData(wb, bridge_sheet, 'Central', startRow = 3, startCol = 2)
openxlsx::writeData(wb, bridge_sheet, bridge_options[[3L]],
                    startRow = 4, startCol = 2)
openxlsx::writeData(wb, bridge_sheet, bridge_modes[[1L]],
                    startRow = 5, startCol = 2)
openxlsx::writeData(wb, bridge_sheet,
  'Annual and pooled amounts: millions of 2025 CAD; per-passenger: 2025 CAD. Positive revenue difference means the private objective requires MORE than the Australian EBITDA benchmark.',
  startRow = 6)
openxlsx::mergeCells(wb, bridge_sheet, cols = 1:20, rows = 1)
openxlsx::mergeCells(wb, bridge_sheet, cols = 1:20, rows = 6)
openxlsx::addStyle(wb, bridge_sheet,
  openxlsx::createStyle(fontSize = 15, fontColour = '#193954',
                        textDecoration = 'bold'), rows = 1, cols = 1)
openxlsx::addStyle(wb, bridge_sheet,
  openxlsx::createStyle(wrapText = TRUE, fontColour = '#445569'),
  rows = 6, cols = 1)
openxlsx::addStyle(wb, bridge_sheet,
  openxlsx::createStyle(fgFill = '#FFF3D5', textDecoration = 'bold',
    fontColour = '#193954'), rows = 3:5, cols = 2, gridExpand = TRUE)
openxlsx::setRowHeights(wb, bridge_sheet, rows = c(1,6),
                        heights = c(34,34))
openxlsx::setColWidths(wb, bridge_sheet, cols = 1, widths = 28)
openxlsx::setColWidths(wb, bridge_sheet, cols = 2, widths = 47)

# Source lists for Excel data-validation drop-downs. Keep them on the same
# sheet to avoid cross-sheet validation references; hide the helper columns.
openxlsx::writeData(wb, bridge_sheet,
  c('Low-cost', 'Central', 'High-cost'), startRow = 3, startCol = 27,
  colNames = FALSE)
openxlsx::writeData(wb, bridge_sheet, bridge_options,
  startRow = 3, startCol = 28, colNames = FALSE)
openxlsx::writeData(wb, bridge_sheet, bridge_modes,
  startRow = 3, startCol = 29, colNames = FALSE)
for (args in list(
  list(row = 3L, range = "'Profitability bridge'!$AA$3:$AA$5"),
  list(row = 4L, range = "'Profitability bridge'!$AB$3:$AB$11"),
  list(row = 5L, range = "'Profitability bridge'!$AC$3:$AC$4")
)) {
  openxlsx::dataValidation(wb, bridge_sheet, cols = 2, rows = args$row,
    type = 'list', value = args$range)
}
openxlsx::setColWidths(wb, bridge_sheet, cols = 27:29,
                       widths = 15, hidden = TRUE)

bridge_presentation_cols <- c(
  'Airport', 'Period', 'Observed revenue', 'Observed EBITDA',
  'Ground rent eliminated', 'Observed EBITDA margin',
  'Rent-adjusted EBITDA margin', 'Australian EBITDA benchmark',
  'Baseline operating gap (pp)', 'New acquisition interest',
  'Purchase-adjusted EBTDA margin', 'Private revenue requirement',
  'Private revenue per passenger', 'Scenario-adjusted EBITDA margin',
  'Scenario-adjusted EBTDA margin', 'Benchmark revenue requirement',
  'Benchmark revenue per passenger', 'Extra revenue beyond benchmark',
  'Extra revenue beyond benchmark per pax', 'Achieved after-tax margin'
)
bridge_presentation <- tidyr::expand_grid(
  Airport = c(can_airports,'BIG4'),
  Period = c('2023','2024','2025','2023-25')
) |>
  mutate(Period = as.character(Period))
for (nm in bridge_presentation_cols[-(1:2)]) {
  bridge_presentation[[nm]] <- NA_real_
}
bridge_presentation <- bridge_presentation[,bridge_presentation_cols]
stopifnot(ncol(bridge_presentation) == 20L,
          nrow(bridge_presentation) == 20L)
bridge_header_row <- 9L
bridge_first <- 10L
bridge_last <- bridge_first + nrow(bridge_presentation) - 1L
openxlsx::writeData(wb, bridge_sheet, bridge_presentation,
                    startRow = bridge_header_row, withFilter = TRUE)
bridge_groups <- list(
  list(from=1L,to=5L,label='AIRPORT / OBSERVED FINANCES',style=head_acquisition),
  list(from=6L,to=9L,label='OPERATING BENCHMARK',style=head_earnings),
  list(from=10L,to=11L,label='ACQUISITION FINANCING',style=head_financing),
  list(from=12L,to=15L,label='PRIVATE PROFITABILITY',style=head_profit),
  list(from=16L,to=20L,label='BENCHMARK REVENUE AND REMAINING GAP',style=head_revenue)
)
for (g in bridge_groups) {
  openxlsx::mergeCells(wb, bridge_sheet, cols = g$from:g$to, rows = 8L)
  openxlsx::writeData(wb, bridge_sheet, g$label,
                      startRow = 8L, startCol = g$from)
  openxlsx::addStyle(wb, bridge_sheet, g$style,
    rows = 8L:bridge_header_row, cols = g$from:g$to,
    gridExpand = TRUE)
}
openxlsx::setRowHeights(wb, bridge_sheet,
  rows = c(8L, bridge_header_row), heights = c(26,68))
openxlsx::setColWidths(wb, bridge_sheet, cols = 3:20, widths = 21)
openxlsx::setColWidths(wb, bridge_sheet, cols = c(13,19), widths = 26)
bridge_lookup <- function(r, source_col) {
  sprintf(paste0(
    'IFERROR(XLOOKUP($A%d&"|"&$B%d&"|"&$B$3&"|"&$B$4,',
    "'Bridge calculations'!$A$%d:$A$%d,",
    "'Bridge calculations'!$%s$%d:$%s$%d),\"\")"),
    r,r,calc_first,calc_last,source_col,calc_first,source_col,calc_last)
}
link_cols <- c(
  C='H', D='I', E='J', F='AE', G='AF', H='Z', I='AH',
  J='L', K='AG', L='O', N='W', O='Y', P='AA', Q='AB',
  R='AC', S='AD', T='Q'
)
for (dest_col in names(link_cols)) {
  formulas_col <- vapply(bridge_first:bridge_last, bridge_lookup,
                        character(1), source_col = link_cols[[dest_col]])
  openxlsx::writeFormula(wb, bridge_sheet, x = formulas_col,
    startCol = match(dest_col,
      vapply(seq_len(ncol(bridge_presentation)),
             openxlsx::int2col, character(1))),
    startRow = bridge_first)
}
# Per-passenger private revenue is computed from the selected model source
# row's passenger denominator; no independent passenger-level assumption.
per_pax_formulas <- vapply(bridge_first:bridge_last, function(r) {
  p <- bridge_lookup(r, 'G')
  sprintf('IFERROR(L%d*1000000/(%s),"")',r,p)
}, character(1))
openxlsx::writeFormula(wb, bridge_sheet, x = per_pax_formulas,
                       startCol = 13, startRow = bridge_first)
bridge_percent_cols <- c(6,7,8,11,14,15,20)
openxlsx::addStyle(wb, bridge_sheet, num_style,
  rows = bridge_first:bridge_last, cols = 3:20, gridExpand = TRUE)
openxlsx::addStyle(wb, bridge_sheet, pct_style,
  rows = bridge_first:bridge_last, cols = bridge_percent_cols,
  gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, bridge_sheet, per_pax_style,
  rows = bridge_first:bridge_last, cols = c(13,17,19),
  gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, bridge_sheet,
  openxlsx::createStyle(numFmt='0.0 "pp"'),
  rows = bridge_first:bridge_last, cols = 9,
  gridExpand = TRUE, stack = TRUE)
for (row in c(14L,18L,22L,26L)) {
  openxlsx::addStyle(wb, bridge_sheet,
    openxlsx::createStyle(border = 'top', borderColour = '#8695A4'),
    rows = row, cols = 1:20, gridExpand = TRUE, stack = TRUE)
}
openxlsx::addStyle(wb, bridge_sheet,
  openxlsx::createStyle(textDecoration = 'bold', fontColour = '#193954'),
  rows = 26:29, cols = 1:20, gridExpand = TRUE, stack = TRUE)
openxlsx::freezePane(wb, bridge_sheet,
  firstActiveRow = bridge_first, firstActiveCol = 3)

message('Prepared profitability bridge: 20 selectable Excel rows; ',
        nrow(calc_data), ' formula audit cases; ',
        nrow(bridge_export), ' independent R scenario / benchmark rows; ',
        nrow(bridge_australia), ' Australian benchmark observations.')
