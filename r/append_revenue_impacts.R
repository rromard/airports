# Revenue impacts | concise 2025 counterfactual results
# Sourced at the end of append_profitability_bridge.R, after the 540-row
# scenario comparison and both illustrative allocation methods are validated.
# Currency: millions of 2025 CAD; passenger rates: 2025 CAD.
# A percentage is an implied CATEGORY REVENUE uplift, not a price/fee forecast.

impact_basis <- bridge_comparison |>
  dplyr::filter(Period == '2025') |>
  dplyr::transmute(
    airport = Airport, period = Period, financing = Financing,
    profitability_objective = `Profitability objective`,
    observed_total_m = `Observed revenue`,
    observed_aero_m = `Aeronautical revenue`,
    observed_parking_m = `Parking and ground transportation revenue`,
    observed_other_m = `Non-aeronautical revenue minus parking`,
    passenger_check = Passengers
  )
impact_keys <- c('airport', 'period', 'financing',
                 'profitability_objective')
if (nrow(impact_basis) != 135L ||
    anyDuplicated(impact_basis[impact_keys]) ||
    anyNA(impact_basis) ||
    any(!is.finite(as.matrix(impact_basis[c(
      'observed_total_m', 'observed_aero_m', 'observed_parking_m',
      'observed_other_m', 'passenger_check')]))) ||
    any(impact_basis$passenger_check <= 0) ||
    any(abs(impact_basis$observed_total_m -
      (impact_basis$observed_aero_m + impact_basis$observed_parking_m +
       impact_basis$observed_other_m)) > 1e-5)) {
  stop('Incomplete or unreconciled 2025 revenue-impact source categories.',
       call. = FALSE)
}

impact_allocated <- revenue_allocation_scenarios_long |>
  dplyr::filter(period == '2025') |>
  dplyr::left_join(impact_basis, by = impact_keys) |>
  dplyr::mutate(
    observed_category_m = dplyr::case_when(
      revenue_category == 'total_aero' ~ observed_aero_m,
      revenue_category == 'parking_transport_revenue' ~ observed_parking_m,
      revenue_category == 'non_aero_minus_parking' ~ observed_other_m,
      TRUE ~ NA_real_
    ),
    additional_category_m = allocated_revenue_m
  )
impact_cases <- impact_allocated |>
  dplyr::distinct(
    airport, period, financing, profitability_objective,
    allocation_method, allocation_method_label,
    passengers, passenger_check, observed_total_m, required_revenue_m,
    counterfactual_revenue_m
  )
if (nrow(impact_cases) != 270L ||
    anyDuplicated(impact_cases[c(impact_keys, 'allocation_method')]) ||
    any(impact_cases$passengers != impact_cases$passenger_check) ||
    any(abs(impact_cases$counterfactual_revenue_m -
      (impact_cases$observed_total_m + impact_cases$required_revenue_m)) >
      1e-5)) {
  stop('Incomplete 270-case 2025 revenue impact panel.', call. = FALSE)
}
impact_totals <- impact_cases |>
  dplyr::transmute(
    airport, period, financing, profitability_objective,
    allocation_method, allocation_method_label, passengers,
    revenue_category = 'total',
    observed_category_m = observed_total_m,
    additional_category_m = required_revenue_m
  )
impact_long <- dplyr::bind_rows(
  impact_totals,
  impact_allocated |>
    dplyr::select(airport, period, financing, profitability_objective,
                  allocation_method, allocation_method_label, passengers,
                  revenue_category, observed_category_m,
                  additional_category_m)
) |>
  dplyr::mutate(
    counterfactual_category_m =
      observed_category_m + additional_category_m,
    observed_per_pax = observed_category_m * 1e6 / passengers,
    additional_per_pax = additional_category_m * 1e6 / passengers,
    counterfactual_per_pax =
      counterfactual_category_m * 1e6 / passengers,
    increase_pct = 100 * additional_category_m / observed_category_m
  )
impact_check <- impact_long |>
  dplyr::group_by(dplyr::across(dplyr::all_of(
    c(impact_keys, 'allocation_method')))) |>
  dplyr::summarise(
    n_categories = dplyr::n(),
    observed_total = observed_category_m[revenue_category == 'total'],
    observed_parts = sum(observed_category_m[revenue_category != 'total']),
    additional_total = additional_category_m[revenue_category == 'total'],
    additional_parts = sum(additional_category_m[revenue_category != 'total']),
    counterfactual_total =
      counterfactual_category_m[revenue_category == 'total'],
    counterfactual_parts =
      sum(counterfactual_category_m[revenue_category != 'total']),
    .groups = 'drop'
  )
if (nrow(impact_long) != 1080L ||
    nrow(impact_check) != 270L ||
    any(impact_check$n_categories != 4L) ||
    anyDuplicated(impact_long[c(impact_keys, 'allocation_method',
                                 'revenue_category')]) ||
    anyNA(impact_long) ||
    any(impact_long$observed_category_m <= 0) ||
    any(!is.finite(as.matrix(impact_long[c(
      'observed_category_m', 'additional_category_m',
      'counterfactual_category_m', 'observed_per_pax',
      'additional_per_pax', 'counterfactual_per_pax', 'increase_pct')]))) ||
    any(abs(impact_check$observed_total -
            impact_check$observed_parts) > 1e-5) ||
    any(abs(impact_check$additional_total -
            impact_check$additional_parts) >
          1e-7 * pmax(1, abs(impact_check$additional_total))) ||
    any(abs(impact_check$counterfactual_total -
            impact_check$counterfactual_parts) > 1e-5)) {
  stop('Revenue impacts fail 2025 category/total reconciliation.',
       call. = FALSE)
}

impact_sections <- c(
  total = 'Total revenue',
  total_aero = 'Aeronautical',
  parking_transport_revenue = 'Parking and transport',
  non_aero_minus_parking = 'Other non-aeronautical'
)
impact_metrics <- c(
  observed_category_m = 'Observed ($m)',
  additional_category_m = 'Additional ($m)',
  counterfactual_category_m = 'Counterfactual ($m)',
  observed_per_pax = 'Observed ($/passenger)',
  additional_per_pax = 'Additional ($/passenger)',
  counterfactual_per_pax = 'Counterfactual ($/passenger)',
  increase_pct = 'Increase vs observed (%)'
)
impact_measure_keys <- unlist(lapply(names(impact_sections), function(category) {
  paste0(category, '__', names(impact_metrics))
}), use.names = FALSE)
impact_wide <- impact_long |>
  dplyr::select(
    airport, period, financing, profitability_objective,
    allocation_method_label, revenue_category,
    dplyr::all_of(names(impact_metrics))
  ) |>
  tidyr::pivot_wider(
    names_from = revenue_category,
    values_from = dplyr::all_of(names(impact_metrics)),
    names_glue = '{revenue_category}__{.value}'
  ) |>
  dplyr::select(airport, period, financing, profitability_objective,
                allocation_method_label,
                dplyr::all_of(impact_measure_keys))
names(impact_wide) <- c(
  'Airport', 'Period', 'Financing', 'Profitability objective',
  'Allocation method',
  unlist(lapply(unname(impact_sections), function(section) {
    paste(section, unname(impact_metrics), sep = ' | ')
  }), use.names = FALSE)
)
impact_objectives <- comparison_source |>
  dplyr::filter(airport == can_airports[[1L]],
                year_label == '2025', scenario_name == 'Central') |>
  dplyr::arrange(threshold_order) |>
  dplyr::pull(profitability_objective)
if (length(impact_objectives) != 9L ||
    anyDuplicated(impact_objectives)) {
  stop('Unexpected 2025 profitability objective labels.', call. = FALSE)
}
impact_wide <- impact_wide |>
  dplyr::arrange(
    factor(Airport, levels = c(can_airports, 'BIG4')),
    factor(Financing, levels = c('Low-cost', 'Central', 'High-cost')),
    factor(`Profitability objective`, levels = impact_objectives),
    factor(`Allocation method`,
      levels = c('Canadian 2025 revenue mix',
                 'Australian FY2015-2025 real revenue growth'))
  )
if (nrow(impact_wide) != 270L || ncol(impact_wide) != 33L ||
    anyDuplicated(impact_wide[1:5]) ||
    anyNA(impact_wide) ||
    any(!is.finite(as.matrix(impact_wide[-(1:5)])))) {
  stop('Incomplete 33-column, 270-row revenue impact summary.',
       call. = FALSE)
}
readr::write_csv(impact_wide,
  file.path(bridge_output_dir, 'revenue_impacts_2025.csv'))
readr::write_csv(impact_long |>
  dplyr::mutate(revenue_category_label = dplyr::recode(
    revenue_category, !!!impact_sections)) |>
  dplyr::select(airport, period, financing, profitability_objective,
                allocation_method, allocation_method_label, revenue_category,
                revenue_category_label, passengers, observed_category_m,
                additional_category_m, counterfactual_category_m,
                observed_per_pax, additional_per_pax, counterfactual_per_pax,
                increase_pct),
  file.path(bridge_output_dir, 'revenue_impacts_2025_long.csv'))

# Excel: complete but focused presentation, without acquisition/financing
# mechanics. All values are generated in R and need no Excel recalculation.
add_acquisition_worksheet(wb, revenue_impacts_sheet, gridLines = FALSE)
openxlsx::writeData(wb, revenue_impacts_sheet,
  'Revenue impacts | observed versus illustrative private-acquisition outcomes (2025)',
  startRow = 1L)
openxlsx::mergeCells(wb, revenue_impacts_sheet, cols = 1:33, rows = 1L)
openxlsx::addStyle(wb, revenue_impacts_sheet,
  openxlsx::createStyle(fontSize = 15, textDecoration = 'bold',
                        fontColour = '#193954'), rows = 1L, cols = 1L)
openxlsx::writeData(wb, revenue_impacts_sheet,
  paste('All amounts in millions of 2025 CAD; per-passenger amounts in 2025 CAD.',
        'Increase (%) uses observed 2025 revenue in the SAME category as the denominator.',
        'Allocation methods distribute required additional revenue, not observed revenue.',
        'These are illustrative category revenue uplifts, NOT airport-fee or passenger-incidence forecasts.'),
  startRow = 2L)
openxlsx::mergeCells(wb, revenue_impacts_sheet, cols = 1:33, rows = 2L)
openxlsx::addStyle(wb, revenue_impacts_sheet,
  openxlsx::createStyle(wrapText = TRUE, valign = 'center',
                        fontColour = '#445569'), rows = 2L, cols = 1L)
openxlsx::setRowHeights(wb, revenue_impacts_sheet, rows = c(1L,2L),
                        heights = c(34,46))
impact_groups <- list(
  list(from = 1L, to = 5L, label = 'SCENARIO', style = head_acquisition),
  list(from = 6L, to = 12L, label = 'TOTAL REVENUE', style = head_revenue),
  list(from = 13L, to = 19L, label = 'AERONAUTICAL', style = head_earnings),
  list(from = 20L, to = 26L, label = 'PARKING & GROUND TRANSPORT',
       style = head_financing),
  list(from = 27L, to = 33L, label = 'OTHER NON-AERONAUTICAL',
       style = head_profit)
)
for (g in impact_groups) {
  openxlsx::mergeCells(wb, revenue_impacts_sheet,
                       cols = g$from:g$to, rows = 5L)
  openxlsx::writeData(wb, revenue_impacts_sheet, g$label,
                      startRow = 5L, startCol = g$from)
}
openxlsx::writeDataTable(wb, revenue_impacts_sheet, impact_wide,
  startRow = 6L, tableName = 'RevenueImpacts2025',
  tableStyle = 'TableStyleMedium2')
for (g in impact_groups) {
  openxlsx::addStyle(wb, revenue_impacts_sheet, g$style,
    rows = 5L:6L, cols = g$from:g$to,
    gridExpand = TRUE, stack = TRUE)
}
impact_first <- 7L
impact_last <- impact_first + nrow(impact_wide) - 1L
openxlsx::addStyle(wb, revenue_impacts_sheet, num_style,
  rows = impact_first:impact_last,
  cols = c(6:8,13:15,20:22,27:29),
  gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, revenue_impacts_sheet, per_pax_style,
  rows = impact_first:impact_last,
  cols = c(9:11,16:18,23:25,30:32),
  gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, revenue_impacts_sheet,
  openxlsx::createStyle(numFmt = '0.0"%"'),
  rows = impact_first:impact_last,
  cols = c(12,19,26,33),
  gridExpand = TRUE, stack = TRUE)
openxlsx::setColWidths(wb, revenue_impacts_sheet,
  cols = 1:5, widths = c(13,11,15,43,47))
openxlsx::setColWidths(wb, revenue_impacts_sheet,
  cols = 6:33, widths = 26)
openxlsx::setRowHeights(wb, revenue_impacts_sheet,
  rows = c(5L,6L), heights = c(28,72))
impact_airport_change <- which(!duplicated(impact_wide$Airport))
if (length(impact_airport_change) != 5L) {
  stop('Revenue impacts have unexpected airport group boundaries.',
       call. = FALSE)
}
openxlsx::addStyle(wb, revenue_impacts_sheet,
  openxlsx::createStyle(border = 'top', borderColour = '#8695A4'),
  rows = impact_first + impact_airport_change[-1L] - 1L,
  cols = 1:33, gridExpand = TRUE, stack = TRUE)
openxlsx::freezePane(wb, revenue_impacts_sheet,
  firstActiveRow = impact_first, firstActiveCol = 6)
message('Prepared revenue impacts: ', nrow(impact_wide),
        ' 2025 scenario/allocation rows and ', nrow(impact_long),
        ' category-level chart rows.')
