# Profitability bridge | Canada versus Australian EBITDA benchmarks
# ---------------------------------------------------------------------
# Run directly for rapid iteration:
#   source(here::here('r', 'append_profitability_bridge.R'))
#
# Direct execution reads SAVED 2023-25 comparison CSVs and the saved
# acquisition RDS (no financial model regeneration and no global objects).
# It saves a standalone three-sheet workbook and three CSVs.
#
# When sourced by append_acquisition_audits.R with bridge_integrated_mode
# set TRUE, the exact same calculation / worksheet code appends the three
# worksheets to the already-open integrated workbook; saving remains the
# responsibility of the integrated workbook builder.
#
# Both modes need tidyverse, here and openxlsx.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(openxlsx)
})

if (!exists('bridge_integrated_mode', inherits = FALSE)) {
  bridge_integrated_mode <- FALSE
}
if (!is.logical(bridge_integrated_mode) ||
    length(bridge_integrated_mode) != 1L ||
    is.na(bridge_integrated_mode)) {
  stop('bridge_integrated_mode must be TRUE or FALSE.', call. = FALSE)
}

if (!bridge_integrated_mode) {
  out_dir <- here::here('output', 'airport_comparison')
  annual_path <- file.path(out_dir,
    'comparison_annual_absolute_2023_2025.csv')
  pooled_path <- file.path(out_dir,
    'comparison_pooled_absolute_2023_2025.csv')
  needed <- c(annual_path, pooled_path,
    here::here('output', 'airport_purchase_counterfactual',
               'acquisition_workbook_sources.rds'))
  missing <- needed[!file.exists(needed)]
  if (length(missing)) {
    stop('The standalone profitability bridge needs saved model inputs. ',
         'Missing:\n', paste(missing, collapse = '\n'),
         '\nRun the full financial/acquisition pipeline once to create ',
         'these files, then rerun this script independently.',
         call. = FALSE)
  }
  annual <- readr::read_csv(annual_path, show_col_types = FALSE)
  pooled <- readr::read_csv(pooled_path, show_col_types = FALSE)
  compare_vars <- c('country','airport','ebitda','total_revenue')
  if (!all(c(compare_vars,'year') %in% names(annual)) ||
      !all(compare_vars %in% names(pooled))) {
    stop('Comparison CSVs lack required airport, revenue or EBITDA inputs.',
         call. = FALSE)
  }
  final_totals <- dplyr::bind_rows(
    annual |>
      dplyr::mutate(period_type = 'annual', .before = year),
    pooled |>
      dplyr::mutate(period_type = 'pooled 2023-25',
                    year = NA_integer_, .after = airport)
  ) |>
    dplyr::rename(non_aero_revenue = commercial_revenue)
  if (nrow(annual) != 24L || nrow(pooled) != 8L ||
      anyDuplicated(annual[c('country','airport','year')]) ||
      anyDuplicated(pooled[c('country','airport')])) {
    stop('Unexpected or duplicated saved airport comparison records.',
         call. = FALSE)
  }

  # Uses exactly the SAME 2025-CAD conversion, nine objectives, and
  # independently summed BIG4 sensitivity rows as the integrated workbook.
  sys.source(here::here('r', 'prepare_acquisition_bridge_inputs.R'),
             envir = environment())
  wb <- openxlsx::createWorkbook()
  sys.source(here::here('r', 'acquisition_audit_format.R'),
             envir = environment())
} else {
  required_objects <- c('wb', 'out_dir', 'final_totals', 'model',
                        'profitability_nine', 'can_airports', 'pct_style',
                        'num_style', 'per_pax_style',
                        'head_acquisition', 'head_earnings',
                        'head_financing', 'head_profit', 'head_revenue')
  bridge_parent <- environment()
  missing_objects <- required_objects[!vapply(required_objects,
    function(nm) exists(nm, envir = bridge_parent, inherits = TRUE),
    logical(1))]
  if (length(missing_objects)) {
    stop('Integrated bridge is missing prepared objects: ',
         paste(missing_objects, collapse = ', '), call. = FALSE)
  }
}

# EDIT HERE when preparing alternative Datawrapper / chart-ready datasets.
bridge_chart_metrics <- c(
  observed_ebitda_margin_pct = 'Observed Canadian EBITDA margin',
  rent_adjusted_ebitda_margin_pct = 'Canadian EBITDA margin, excluding rent',
  australian_ebitda_margin_pct = 'Australian weighted EBITDA benchmark',
  counterfactual_ebitda_margin_pct = 'Scenario-adjusted EBITDA margin',
  counterfactual_ebtda_margin_pct = 'Scenario-adjusted EBTDA margin',
  acquisition_required_revenue_m = 'Revenue needed for selected private objective',
  benchmark_required_revenue_m = 'Revenue needed for Australian EBITDA margin',
  additional_revenue_gap_m = 'Private objective less Australian benchmark, CAD m',
  additional_revenue_gap_per_pax_cad = 'Private objective less Australian benchmark, CAD per passenger'
)
bridge_export_chart_long <- TRUE

bridge_sheet <- 'Profitability bridge'
bridge_comparison_sheet <- 'Scenario comparison'
bridge_calc_sheet <- 'Bridge calculations'
bridge_modes <- c('Pooled 2023-25', 'Year-matched')
bridge_output_dir <- file.path(out_dir, 'profitability_bridge')
dir.create(bridge_output_dir, showWarnings = FALSE, recursive = TRUE)
stopifnot(!any(c(bridge_sheet, bridge_comparison_sheet,
                 bridge_calc_sheet) %in% names(wb)))

required_compare <- c('country', 'airport', 'year', 'period_type',
                      'ebitda', 'total_revenue', 'total_aero', 'non_aero_revenue',
                       'parking_transport_revenue', 'non_aero_minus_parking')
if (!all(required_compare %in% names(final_totals))) {
  stop('Comparison workbook lacks the matched EBITDA/revenue benchmark fields.',
       call. = FALSE)
}
# All financial amounts in final_totals have already been expressed in 2025
# CAD. Use the actual EBITDA-ACCC total operating revenue, not
# SCACH's separate Sydney net-profit revenue basis.
bridge_australia <- final_totals |>
  filter(country == 'Australia') |>
  mutate(year_label = if_else(is.na(year), '2023-25',
                              as.character(year))) |>
  group_by(year_label) |>
  summarise(
    n_airports = n_distinct(airport),
    ebitda_m = sum(ebitda),
    matched_revenue_m = sum(total_revenue),
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
         acquisition_sale_value_m = estimated_sale_value_m,
         acquisition_borrowing_m = new_acquisition_debt_m,
         observed_revenue_check_m = revenue_m,
         observed_passenger_check = passengers)

# Observed revenues stay fixed across all financing and profit objectives.
# BIG4 sums the four source airports; a missing parking component stays NA.
bridge_mix_fields <- c('total_aero', 'non_aero_revenue',
                       'parking_transport_revenue', 'non_aero_minus_parking')
bridge_mix_airports <- final_totals |>
  filter(country == 'Canada') |>
  mutate(year_label = if_else(is.na(year), '2023-25',
                              as.character(year))) |>
  select(airport, year_label, all_of(bridge_mix_fields))
bridge_mix_big4 <- bridge_mix_airports |>
  group_by(year_label) |>
  summarise(across(all_of(bridge_mix_fields),
                   ~ if (all(is.finite(.x))) sum(.x) else NA_real_),
            .groups = 'drop') |>
  mutate(airport = 'BIG4', .before = 1)
bridge_revenue_mix <- bind_rows(bridge_mix_airports, bridge_mix_big4)
if (nrow(bridge_revenue_mix) != 20L ||
    anyDuplicated(bridge_revenue_mix[c('airport', 'year_label')])) {
  stop('Incomplete Canadian revenue-mix keys.', call. = FALSE)
}
mix_complete <- is.finite(bridge_revenue_mix$non_aero_revenue) &
  is.finite(bridge_revenue_mix$parking_transport_revenue)
if (!all(is.finite(bridge_revenue_mix$non_aero_minus_parking) ==
         mix_complete) ||
    any(abs(bridge_revenue_mix$non_aero_minus_parking[mix_complete] -
            (bridge_revenue_mix$non_aero_revenue[mix_complete] -
             bridge_revenue_mix$parking_transport_revenue[mix_complete])) >
          1e-5)) {
  stop('Canadian non-aeronautical revenue residual fails reconciliation.',
       call. = FALSE)
}
bridge_inputs <- profitability_nine |>
  left_join(bridge_model_financing, by = bridge_keys) |>
  left_join(bridge_revenue_mix, by = c('airport', 'year_label')) |>
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
           reported_comparison_revenue_m = total_revenue),
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
  'Selected Australian benchmark margin', 'Australian benchmark revenue requirement',
  'Australian benchmark revenue per passenger', 'Revenue gap versus Australian benchmark',
  'Revenue gap per passenger', 'Observed EBITDA margin',
  'Rent-adjusted EBITDA margin', 'Purchase-adjusted EBTDA margin',
  'Australian benchmark gap versus rent-adjusted (pp)'
)
calc_data <- as.data.frame(calc_input)
for (nm in calc_output_headers) calc_data[[nm]] <- NA_real_
add_acquisition_worksheet(wb, bridge_calc_sheet, gridLines = FALSE)
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

# Main 20-row interactive table. Results use INDEX/MATCH formulas
# referencing Bridge calculations, and refresh when a selector is changed.
add_acquisition_worksheet(wb, bridge_sheet, gridLines = FALSE)
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
  list(row = 3L, range = '$AA$3:$AA$5'),
  list(row = 4L, range = '$AB$3:$AB$11'),
  list(row = 5L, range = '$AC$3:$AC$4')
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
  'Australian operating gap (pp)', 'New acquisition interest',
  'Purchase-adjusted EBTDA margin', 'Private revenue requirement',
  'Private revenue per passenger', 'Scenario-adjusted EBITDA margin',
  'Scenario-adjusted EBTDA margin', 'Australian benchmark revenue requirement',
  'Australian benchmark revenue per passenger', 'Extra revenue beyond Australian benchmark',
  'Extra revenue beyond Australian benchmark per pax', 'Achieved after-tax margin'
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
  list(from=6L,to=9L,label='AUSTRALIAN OPERATING BENCHMARK',style=head_earnings),
  list(from=10L,to=11L,label='ACQUISITION FINANCING',style=head_financing),
  list(from=12L,to=15L,label='PRIVATE PROFITABILITY',style=head_profit),
  list(from=16L,to=20L,label='AUSTRALIAN BENCHMARK REVENUE AND GAP',style=head_revenue)
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
# INDEX/MATCH works in Excel and LibreOffice without newer _xlfn
# function registration. Deliberately expose #N/A for missing keys.
bridge_lookup <- function(r, source_col) {
  sprintf(paste0(
    'INDEX(\'Bridge calculations\'!$%s$%d:$%s$%d,',
    'MATCH($A%d&"|"&$B%d&"|"&$B$3&"|"&$B$4,',
    '\'Bridge calculations\'!$A$%d:$A$%d,0))'),
    source_col, calc_first, source_col, calc_last,
    r, r, calc_first, calc_last)
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
# Assert that all twenty visible keys exist once in the 540-case
# source table for every selectable scenario/objective, not just the default.
bridge_test_keys <- tidyr::crossing(
  Airport = c(can_airports, 'BIG4'),
  Period = c('2023','2024','2025','2023-25'),
  Financing = c('Low-cost','Central','High-cost'),
  Objective = bridge_options
) |>
  dplyr::mutate(Key = paste(Airport, Period, Financing, Objective, sep = '|'))
if (nrow(bridge_test_keys) != 540L ||
    anyDuplicated(bridge_test_keys$Key) ||
    !setequal(bridge_test_keys$Key, calc_input$Key)) {
  stop('Profitability bridge lookup keys do not match the 540 source cases.',
       call. = FALSE)
}

# Per-passenger private revenue is computed from the selected model source
# row's passenger denominator; no independent passenger-level assumption.
per_pax_formulas <- vapply(bridge_first:bridge_last, function(r) {
  p <- bridge_lookup(r, 'G')
  sprintf('L%d*1000000/(%s)',r,p)
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

# Flat scenario comparison | 540 fully calculated, filterable R cases.
# Use the same validated inputs and financing/profitability solutions as the
# interactive bridge. Unlike the formula audit, these are WRITTEN VALUES,
# so the comparison is visible without spreadsheet formula recalculation.
# The Australian reference is matched by period; the three-year pooled
# Canadian period uses the Australian 2023-25 pooled revenue-weighted margin.
comparison_source <- bridge_inputs |>
  arrange(factor(airport, levels = c(can_airports,'BIG4')),
          factor(year_label, levels = c('2023','2024','2025','2023-25')),
          factor(scenario_name, levels = c('Low-cost','Central','High-cost')),
          threshold_order)
bridge_comparison <- comparison_source |>
  transmute(
    Airport = airport,
    Period = year_label,
    Financing = scenario_name,
    `Profitability objective` = profitability_objective,
    Passengers = passengers,
    `Observed revenue` = revenue_m,
    `Observed EBITDA` = ebitda_m,
    `Aeronautical revenue` = total_aero,
    `Parking and ground transportation revenue` = parking_transport_revenue,
    `Non-aeronautical revenue minus parking` = non_aero_minus_parking,
    `Estimated purchase price` = acquisition_sale_value_m,
    `New acquisition debt` = acquisition_borrowing_m,
    `Additional acquisition interest` = additional_acquisition_interest_m,
    `Post-acquisition pre-tax profit` = purchase_adjusted_pre_tax_income_m,
    `Observed EBITDA margin` = ebitda_m / revenue_m,
    `Australian EBITDA benchmark` = benchmark_matching,
    `Margin after rent removal` = (ebitda_m + ground_rent_m) / revenue_m,
    `Margin after operating savings` = base_operating_ebitda_m / revenue_m,
    `Final scenario EBITDA margin` =
      counterfactual_ebitda_m / counterfactual_revenue_m,
    `Additional revenue` = required_revenue_m,
    `Additional revenue per passenger` = required_revenue_m * 1e6 / passengers,
    `Increase vs total revenue` = required_revenue_m / revenue_m,
    `Achieved after-tax profit margin` = achieved_net_margin
  )
comparison_keys <- paste(bridge_comparison[[1L]], bridge_comparison[[2L]],
                         bridge_comparison[[3L]], bridge_comparison[[4L]],
                         sep = '|')
comparison_numeric_required <- c(
  'Passengers', 'Observed revenue', 'Observed EBITDA',
  'Estimated purchase price', 'New acquisition debt',
  'Additional acquisition interest', 'Post-acquisition pre-tax profit',
  'Observed EBITDA margin', 'Australian EBITDA benchmark',
  'Margin after rent removal', 'Margin after operating savings',
  'Final scenario EBITDA margin', 'Additional revenue',
  'Additional revenue per passenger', 'Increase vs total revenue',
  'Achieved after-tax profit margin'
)
if (nrow(bridge_comparison) != 540L || ncol(bridge_comparison) != 23L ||
    anyDuplicated(comparison_keys) ||
    !setequal(comparison_keys, calc_input$Key) ||
    any(!is.finite(as.matrix(bridge_comparison[comparison_numeric_required]))) ||
    any(bridge_comparison$Passengers <= 0) ||
    any(abs(bridge_comparison[['Additional revenue per passenger']] -
            comparison_source$required_revenue_per_pax_cad) > 1e-5) ||
    any(abs(bridge_comparison[['Increase vs total revenue']] -
            comparison_source$required_uplift_pct) > 1e-8) ||
    any(abs(bridge_comparison[['Achieved after-tax profit margin']] -
            comparison_source$achieved_net_margin) > 1e-8)) {
  stop('Expanded scenario comparison fails its 540-case reconciliation.',
       call. = FALSE)
}

add_acquisition_worksheet(wb, bridge_comparison_sheet, gridLines = FALSE)
openxlsx::writeData(wb, bridge_comparison_sheet,
  'Scenario comparison | Canadian acquisition outcomes and Australian reference',
  startRow = 1)
openxlsx::mergeCells(wb, bridge_comparison_sheet, cols = 1:23, rows = 1)
openxlsx::addStyle(wb, bridge_comparison_sheet,
  openxlsx::createStyle(fontSize = 15, fontColour = '#193954',
                        textDecoration = 'bold'), rows = 1, cols = 1)
openxlsx::writeData(wb, bridge_comparison_sheet,
  '540 cases: 5 airport groups x 4 periods x 3 financing packages x 9 profitability objectives. Use the column filters to compare results.',
  startRow = 3)
openxlsx::writeData(wb, bridge_comparison_sheet,
  'Revenue split: aeronautical, parking/ground transportation, and non-aeronautical minus parking. Missing components remain blank. Australian EBITDA benchmark is year-matched (pooled for 2023-25). Financial values are millions of 2025 CAD; per passenger, 2025 CAD. Revenue uplift is not a fee forecast.',
  startRow = 4)
for (r in c(3L,4L)) {
  openxlsx::mergeCells(wb, bridge_comparison_sheet, cols = 1:23, rows = r)
}
openxlsx::addStyle(wb, bridge_comparison_sheet,
  openxlsx::createStyle(wrapText = TRUE, fontColour = '#445569'),
  rows = 3:4, cols = 1, gridExpand = TRUE)
openxlsx::setRowHeights(wb, bridge_comparison_sheet,
  rows = c(1L,3L,4L,6L,7L), heights = c(34,30,36,26,64))

comparison_groups <- list(
  list(from = 1L, to = 4L, label = 'SCENARIO', style = head_acquisition),
  list(from = 5L, to = 7L, label = 'OBSERVED FINANCES',
       style = head_earnings),
  list(from = 8L, to = 10L, label = 'REVENUE COMPOSITION',
       style = head_acquisition),
  list(from = 11L, to = 14L, label = 'ACQUISITION FINANCING',
       style = head_financing),
  list(from = 15L, to = 19L, label = 'OPERATING PROFITABILITY',
       style = head_profit),
  list(from = 20L, to = 23L, label = 'REVENUE REQUIREMENT AND OUTCOME',
       style = head_revenue)
)
for (g in comparison_groups) {
  openxlsx::mergeCells(wb, bridge_comparison_sheet,
                       cols = g$from:g$to, rows = 6L)
  openxlsx::writeData(wb, bridge_comparison_sheet, g$label,
                      startRow = 6L, startCol = g$from)
  # As on Profitability bridge, colour the group and table headers.
  openxlsx::addStyle(wb, bridge_comparison_sheet, g$style,
                     rows = 6L:7L, cols = g$from:g$to,
                     gridExpand = TRUE)
}
openxlsx::writeDataTable(wb, bridge_comparison_sheet,
  bridge_comparison, startRow = 7L, tableName = 'BridgeScenarioComparison',
  tableStyle = 'TableStyleMedium2')
comparison_first <- 8L
comparison_last <- comparison_first + nrow(bridge_comparison) - 1L
comparison_pct_cols <- match(c(
  'Observed EBITDA margin', 'Australian EBITDA benchmark',
  'Margin after rent removal', 'Margin after operating savings',
  'Final scenario EBITDA margin', 'Increase vs total revenue',
  'Achieved after-tax profit margin'), names(bridge_comparison))
openxlsx::addStyle(wb, bridge_comparison_sheet, pct_style,
  rows = comparison_first:comparison_last, cols = comparison_pct_cols,
  gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, bridge_comparison_sheet, int_style,
  rows = comparison_first:comparison_last, cols = 5,
  gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, bridge_comparison_sheet, num_style,
  rows = comparison_first:comparison_last, cols = c(6:14, 20),
  gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, bridge_comparison_sheet, per_pax_style,
  rows = comparison_first:comparison_last, cols = 21,
  gridExpand = TRUE, stack = TRUE)
# Default reading order groups all objectives by airport, period and
# financing. Draw borders only where airport or period changes; avoid
# adding a rule to every profitability-objective row.
airport_change <- which(comparison_source$airport !=
  dplyr::lag(comparison_source$airport))
period_change <- which(comparison_source$year_label !=
  dplyr::lag(comparison_source$year_label) &
  comparison_source$airport == dplyr::lag(comparison_source$airport))
if (length(airport_change) != 4L || length(period_change) != 15L ||
    any(airport_change %in% period_change)) {
  stop('Unexpected scenario comparison group boundaries.', call. = FALSE)
}
openxlsx::addStyle(wb, bridge_comparison_sheet,
  openxlsx::createStyle(border = 'top', borderStyle = 'thin',
                        borderColour = '#D1DBE3'),
  rows = comparison_first + period_change - 1L, cols = 1:23,
  gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, bridge_comparison_sheet,
  openxlsx::createStyle(border = 'top', borderStyle = 'medium',
                        borderColour = '#8393A0'),
  rows = comparison_first + airport_change - 1L, cols = 1:23,
  gridExpand = TRUE, stack = TRUE)
comparison_widths <- c(
  12, 13, 15, 47, 17, 22, 22, 22, 29, 31,
  22, 22, 22, 25, 22, 24, 22, 24, 25, 22, 27, 22, 25
)
stopifnot(length(comparison_widths) == ncol(bridge_comparison))
openxlsx::setColWidths(wb, bridge_comparison_sheet,
  cols = seq_along(comparison_widths), widths = comparison_widths)
openxlsx::freezePane(wb, bridge_comparison_sheet,
  firstActiveRow = comparison_first, firstActiveCol = 5)

message('Prepared profitability bridge: 20 selectable Excel rows; ',
        nrow(calc_data), ' formula audit cases; ',
        nrow(bridge_comparison), ' static scenario comparison rows; ',
        nrow(bridge_export), ' independent R scenario / benchmark rows; ',
        nrow(bridge_australia), ' Australian benchmark observations.')

# Save only the small standalone workbook when executed directly.
# The integrated builder saves its expanded 18-sheet workbook separately.
if (!bridge_integrated_mode) {
  openxlsx::worksheetOrder(wb) <- match(
    c(bridge_sheet, bridge_comparison_sheet, bridge_calc_sheet), names(wb))
  standalone_path <- file.path(bridge_output_dir,
    'airport_profitability_bridge_standalone_2023_2025.xlsx')
  openxlsx::saveWorkbook(wb, standalone_path, overwrite = TRUE)
  message('Saved standalone profitability workbook: ', standalone_path)
  message('CSV exports: ', bridge_output_dir)
}
