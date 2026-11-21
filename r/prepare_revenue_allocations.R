# Illustrative allocation of acquisition revenue requirements | November 2026
# -------------------------------------------------------------------------
# Sourced by append_profitability_bridge.R after comparison_source is built.
# Required objects: final_totals, can_airports, comparison_source,
# bridge_output_dir. This is presentation analysis, not a model assumption.
# Three exclusive categories:
#   total_aero = aeronautical revenue including Canadian AIF
#   parking_transport_revenue = parking and ground/landside transport
#   non_aero_minus_parking = other non-aeronautical revenue, net of parking
#
# Canadian 2025 mix uses summed revenues, not the mean airport shares.
# Australian FY2015-2025 growth uses the share of total real revenue
# INCREASE contributed by each category, pooled across BNE/MEL/PER/SYD.
# The ACCC financial export is already in FY2024-25 AUD. Only dimensionless
# shares transfer to the Canadian scenarios; no cross-currency conversion.
# No pass-through or fee-incidence assumptions are made here.

allocation_categories <- c('total_aero', 'parking_transport_revenue',
                           'non_aero_minus_parking')
allocation_measure_cols <- c('total_revenue', allocation_categories)
allocation_aus_airports <- c('BNE', 'MEL', 'PER', 'SYD')
allocation_aus_years <- sprintf('%d-%02d', 2014:2024, 15:25)

allocation_can_2025 <- final_totals |>
  dplyr::filter(country == 'Canada', year == 2025L,
                airport %in% can_airports) |>
  dplyr::select(airport, dplyr::all_of(allocation_measure_cols))
if (nrow(allocation_can_2025) != 4L ||
    anyDuplicated(allocation_can_2025['airport']) ||
    !setequal(allocation_can_2025$airport, can_airports) ||
    any(!is.finite(as.matrix(
      allocation_can_2025[allocation_measure_cols])))) {
  stop('Revenue allocation requires complete 2025 Canadian Big Four revenues.',
       call. = FALSE)
}

allocation_aus_path <- here::here(
  'output', 'accc_airport_financial_analysis_2007_2025.xlsx')
if (!file.exists(allocation_aus_path)) {
  stop('Missing ACCC revenue-allocation input: ', allocation_aus_path,
       call. = FALSE)
}
allocation_aus_raw <- readxl::read_excel(allocation_aus_path,
                                         sheet = 'financial')
allocation_aus_fields <- c('airport', 'year', 'total_revenue', 'aero_revenue',
                           'parking_transport_revenue', 'other_revenue')
if (!all(allocation_aus_fields %in% names(allocation_aus_raw))) {
  stop('ACCC financial workbook lacks allocation fields: ',
       paste(setdiff(allocation_aus_fields, names(allocation_aus_raw)),
             collapse = ', '), call. = FALSE)
}
allocation_aus_annual <- allocation_aus_raw |>
  dplyr::filter(airport %in% allocation_aus_airports,
                year %in% allocation_aus_years) |>
  dplyr::transmute(
    airport = as.character(airport), year = as.character(year),
    total_revenue = as.numeric(total_revenue),
    total_aero = as.numeric(aero_revenue),
    parking_transport_revenue = as.numeric(parking_transport_revenue),
    non_aero_minus_parking = as.numeric(other_revenue)
  )
if (nrow(allocation_aus_annual) != 4L * length(allocation_aus_years) ||
    anyDuplicated(allocation_aus_annual[c('airport', 'year')]) ||
    !setequal(allocation_aus_annual$airport, allocation_aus_airports) ||
    !setequal(allocation_aus_annual$year, allocation_aus_years) ||
    any(!is.finite(as.matrix(
      allocation_aus_annual[allocation_measure_cols])))) {
  stop('Revenue allocation requires the complete ACCC four-airport FY2015-2025 panel.',
       call. = FALSE)
}

allocation_reconcile <- function(d, label) {
  if (any(as.matrix(d[allocation_measure_cols]) < 0) ||
      any(d$total_revenue <= 0) ||
      any(abs(rowSums(d[allocation_categories]) - d$total_revenue) >
          1e-4 * pmax(1, abs(d$total_revenue)))) {
    stop('Revenue allocation sources fail three-category reconciliation: ',
         label, call. = FALSE)
  }
}
allocation_reconcile(allocation_can_2025, 'Canada 2025')
allocation_reconcile(allocation_aus_annual, 'Australia 2015-2025')

# Retain all 11 Australian fiscal years in a transparent source audit, not
# just the two endpoints used to calculate growth contribution shares.
allocation_source_audit <- dplyr::bind_rows(
  allocation_can_2025 |>
    dplyr::mutate(country = 'Canada', year = '2025',
                  price_basis = '2025 CAD millions'),
  allocation_aus_annual |>
    dplyr::mutate(country = 'Australia',
                  price_basis = 'FY2024-25 AUD millions')
) |>
  tidyr::pivot_longer(dplyr::all_of(allocation_measure_cols),
                      names_to = 'revenue_category',
                      values_to = 'revenue_m') |>
  dplyr::select(country, airport, year, price_basis,
                revenue_category, revenue_m) |>
  dplyr::arrange(country, airport, year,
    factor(revenue_category, levels = allocation_measure_cols))
if (nrow(allocation_source_audit) != 192L) {
  stop('Revenue allocation source audit is incomplete.', call. = FALSE)
}

allocation_can_totals <- allocation_can_2025 |>
  dplyr::summarise(dplyr::across(dplyr::all_of(allocation_measure_cols), sum))
allocation_can_shares <- allocation_can_totals |>
  tidyr::pivot_longer(dplyr::all_of(allocation_categories),
                      names_to = 'revenue_category', values_to = 'component_m') |>
  dplyr::transmute(allocation_method = 'canada_2025_mix',
                   revenue_category,
                   allocation_share = component_m / total_revenue)

allocation_aus_change <- allocation_aus_annual |>
  dplyr::group_by(airport) |>
  dplyr::summarise(
    dplyr::across(dplyr::all_of(allocation_measure_cols),
                  ~ .x[year == '2024-25'] - .x[year == '2014-15']),
    .groups = 'drop'
  )
allocation_aus_totals <- allocation_aus_change |>
  dplyr::summarise(dplyr::across(dplyr::all_of(allocation_measure_cols), sum))
if (allocation_aus_totals$total_revenue <= 0) {
  stop('ACCC pooled FY2015-2025 revenue growth must be positive.',
       call. = FALSE)
}
allocation_aus_shares <- allocation_aus_totals |>
  tidyr::pivot_longer(dplyr::all_of(allocation_categories),
                      names_to = 'revenue_category', values_to = 'change_m') |>
  dplyr::transmute(allocation_method = 'australia_2015_2025_growth',
                   revenue_category,
                   allocation_share = change_m / total_revenue)

allocation_category_labels <- c(
  total_aero = 'Aeronautical',
  parking_transport_revenue = 'Parking and ground transportation',
  non_aero_minus_parking =
    'Non-aeronautical excluding parking and ground transportation'
)
revenue_allocation_shares <- dplyr::bind_rows(allocation_can_shares,
                                             allocation_aus_shares) |>
  dplyr::mutate(
    allocation_method_label = dplyr::recode(
      allocation_method,
      canada_2025_mix = 'Canadian 2025 revenue mix',
      australia_2015_2025_growth =
        'Australian FY2015-2025 real revenue growth'),
    revenue_category_label =
      unname(allocation_category_labels[revenue_category])
  ) |>
  dplyr::arrange(
    factor(allocation_method,
           levels = c('canada_2025_mix', 'australia_2015_2025_growth')),
    factor(revenue_category, levels = allocation_categories)
  )
allocation_share_check <- revenue_allocation_shares |>
  dplyr::group_by(allocation_method) |>
  dplyr::summarise(n_categories = dplyr::n(),
                   share_sum = sum(allocation_share), .groups = 'drop')
if (nrow(revenue_allocation_shares) != 6L ||
    anyDuplicated(revenue_allocation_shares[
      c('allocation_method','revenue_category')]) ||
    anyNA(revenue_allocation_shares) ||
    any(!is.finite(revenue_allocation_shares$allocation_share)) ||
    any(revenue_allocation_shares$allocation_share < 0) ||
    nrow(allocation_share_check) != 2L ||
    any(allocation_share_check$n_categories != 3L) ||
    any(abs(allocation_share_check$share_sum - 1) > 1e-8)) {
  stop('Both revenue allocation methods require three nonnegative shares summing to one.',
       call. = FALSE)
}
readr::write_csv(allocation_source_audit,
  file.path(bridge_output_dir, 'revenue_allocation_source_audit.csv'))
readr::write_csv(revenue_allocation_shares,
  file.path(bridge_output_dir, 'revenue_allocation_shares.csv'))

# Each profitability requirement is already solved for each financing
# package. The two allocation methods merely distribute the same amount.
revenue_allocation_scenarios_long <- tidyr::crossing(
  comparison_source |>
    dplyr::transmute(airport, period = year_label,
                     financing = scenario_name, threshold_id,
                     profitability_objective, passengers, observed_revenue_m = revenue_m,
                     counterfactual_revenue_m, required_revenue_m),
  revenue_allocation_shares
) |>
  dplyr::mutate(
    allocated_revenue_m = required_revenue_m * allocation_share,
    allocated_revenue_per_passenger_cad =
      allocated_revenue_m * 1e6 / passengers
  )
allocation_case_check <- revenue_allocation_scenarios_long |>
  dplyr::group_by(airport, period, financing, threshold_id,
                  allocation_method) |>
  dplyr::summarise(n_categories = dplyr::n(),
                   allocated_total_m = sum(allocated_revenue_m),
                   required_revenue_m = dplyr::first(required_revenue_m),
                   .groups = 'drop')
if (nrow(revenue_allocation_scenarios_long) != 3240L ||
    nrow(allocation_case_check) != 1080L ||
    anyDuplicated(revenue_allocation_scenarios_long[
      c('airport', 'period', 'financing', 'threshold_id',
        'allocation_method', 'revenue_category')]) ||
    anyNA(revenue_allocation_scenarios_long) ||
    any(allocation_case_check$n_categories != 3L) ||
    any(abs(allocation_case_check$allocated_total_m -
            allocation_case_check$required_revenue_m) >
          1e-7 * pmax(1, abs(allocation_case_check$required_revenue_m)))) {
  stop('Revenue allocation fails the 540-case, two-method sum check.',
       call. = FALSE)
}
readr::write_csv(revenue_allocation_scenarios_long,
  file.path(bridge_output_dir, 'revenue_allocation_scenarios_long.csv'))
readr::write_csv(
  revenue_allocation_scenarios_long |>
    dplyr::filter(period == '2025') |>
    dplyr::select(airport, period, financing, threshold_id,
                  profitability_objective, allocation_method,
                  allocation_method_label, revenue_category,
                  revenue_category_label, observed_revenue_m,
                  counterfactual_revenue_m, required_revenue_m,
                  allocation_share, allocated_revenue_m,
                  allocated_revenue_per_passenger_cad),
  file.path(bridge_output_dir, 'revenue_allocation_chart_long.csv')
)

# Twelve allocations: six amounts plus the corresponding six per-passenger
# rates. Keep each allocation method's components and per-passenger rates
# together in the comparison worksheet. The scenario total is sourced from
# the existing validated model rather than reconstructed from these values.
allocation_headers_amount <- c(
  'Canada mix: aeronautical' = 'canada_2025_mix__total_aero',
  'Canada mix: parking/transport' =
    'canada_2025_mix__parking_transport_revenue',
  'Canada mix: non-aero less parking' =
    'canada_2025_mix__non_aero_minus_parking',
  'Australia growth: aeronautical' =
    'australia_2015_2025_growth__total_aero',
  'Australia growth: parking/transport' =
    'australia_2015_2025_growth__parking_transport_revenue',
  'Australia growth: non-aero less parking' =
    'australia_2015_2025_growth__non_aero_minus_parking'
)
allocation_headers_per_pax <- c(
  'Canada mix: aero per passenger' = 'canada_2025_mix__total_aero',
  'Canada mix: parking per passenger' =
    'canada_2025_mix__parking_transport_revenue',
  'Canada mix: non-aero per passenger' =
    'canada_2025_mix__non_aero_minus_parking',
  'Australia growth: aero per passenger' =
    'australia_2015_2025_growth__total_aero',
  'Australia growth: parking per passenger' =
    'australia_2015_2025_growth__parking_transport_revenue',
  'Australia growth: non-aero per passenger' =
    'australia_2015_2025_growth__non_aero_minus_parking'
)
allocation_headers <- c(names(allocation_headers_amount),
                        names(allocation_headers_per_pax))
allocation_wide <- revenue_allocation_scenarios_long |>
  dplyr::mutate(allocation_column = paste(
    allocation_method, revenue_category, sep = '__')) |>
  dplyr::select(airport, period, financing, profitability_objective,
                allocation_column, allocated_revenue_m,
                allocated_revenue_per_passenger_cad) |>
  tidyr::pivot_wider(
    names_from = allocation_column,
    values_from = c(allocated_revenue_m,
                    allocated_revenue_per_passenger_cad),
    names_glue = '{.value}__{allocation_column}'
  )
allocation_amount_keys <- paste0('allocated_revenue_m__',
                                  unname(allocation_headers_amount))
allocation_per_pax_keys <- paste0('allocated_revenue_per_passenger_cad__',
                                   unname(allocation_headers_per_pax))
allocation_wide <- allocation_wide |>
  dplyr::select(airport, period, financing, profitability_objective,
                dplyr::all_of(allocation_amount_keys),
                dplyr::all_of(allocation_per_pax_keys))
names(allocation_wide)[-(1:4)] <- allocation_headers
# Keep a single method's amounts immediately beside its passenger rates.
allocation_wide <- allocation_wide |>
  dplyr::select(airport, period, financing, profitability_objective,
                dplyr::all_of(names(allocation_headers_amount)[1:3]),
                dplyr::all_of(names(allocation_headers_per_pax)[1:3]),
                dplyr::all_of(names(allocation_headers_amount)[4:6]),
                dplyr::all_of(names(allocation_headers_per_pax)[4:6]))
if (nrow(allocation_wide) != 540L ||
    ncol(allocation_wide) != 16L ||
    anyDuplicated(allocation_wide[1:4]) ||
    anyNA(allocation_wide) ||
    any(!is.finite(as.matrix(allocation_wide[-(1:4)])))) {
  stop('Incomplete twelve-column wide revenue allocation join.', call. = FALSE)
}
