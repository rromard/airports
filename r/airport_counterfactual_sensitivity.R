# Airport counterfactual | pandemic-index and scenario-timing sensitivities
# -----------------------------------------------------------------------------
# Run AFTER r/airport_revenue_counterfactual.R from the project root:
#   source(here::here('r', 'airport_counterfactual_sensitivity.R'))
#
# Inputs: existing outputs in output/airport_counterfactual/ (no new downloads).
# Output: seven diagnostic CSVs in output/airport_counterfactual/sensitivity/.
# Does NOT overwrite or modify the main counterfactual, its data, or its plots.
# Compatible with EITHER primary GRADUAL mode. Internally it always reports
# GRADUAL = originally observed path and GRADUAL_INTERP = adjusted path.
# All revenue amounts are millions of constant 2025 CAD.
#
# GRADUAL_INTERP is a sensitivity, NOT an estimate of actual pandemic prices:
# - For each Australian airport's real aeronautical revenue/passenger index
#   (2015 = 100), linearly interpolate ONLY 2020, 2021 and 2022 between its
#   OBSERVED 2019 and 2023 indices.
# - Then take the same unweighted mean of the four Australian airport indices.
# - Keep Canadian 2015 revenue/passenger baselines, actual Canadian revenue,
#   actual passenger volumes, and both historical scenarios unchanged.
# - Thus only GRADUAL's 2020-2022 hypothetical revenues change.
# - The interpolation does not affect any 2025 scenario result or the separate
#   2023-2025 fixed-cost EBITDA sensitivity.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

in_dir <- here('output', 'airport_counterfactual')
out_dir <- file.path(in_dir, 'sensitivity')
can_airports <- c('YYC', 'YUL', 'YVR', 'YYZ')
aus_airports <- c('BNE', 'MEL', 'PER', 'SYD')
main_scenarios <- c('GRADUAL', 'CAP_THEN_SHOCK', 'IMMEDIATE_SHOCK')
all_scenarios <- c('GRADUAL', 'GRADUAL_INTERP',
                   'CAP_THEN_SHOCK', 'IMMEDIATE_SHOCK')
years <- 2015:2025
pandemic_years <- 2020:2022
period_order <- c('2016-2019', '2020-2022', '2023-2025', '2016-2025')
tolerance <- 1e-7

read_required <- function(filename) {
  path <- file.path(in_dir, filename)
  if (!file.exists(path)) stop('Missing counterfactual output: ', path)
  readr::read_csv(path, show_col_types = FALSE)
}
require_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop(label, ' is missing: ', paste(missing, collapse = ', '))
}
assert_panel <- function(data, keys, n, label) {
  if (nrow(data) != n || anyDuplicated(data[keys])) {
    stop('Incomplete or duplicated keys in ', label)
  }
}

actual <- read_required('actual_growth_canada_australia.csv')
paths <- read_required('scenario_growth_paths.csv')
annual <- read_required('counterfactual_annual.csv')
require_columns(actual, c('country', 'airport', 'year', 'index_2015'), 'Actual growth')
require_columns(paths, c('scenario', 'year', 'growth_multiple_2015'), 'Scenario paths')
require_columns(annual, c('airport', 'year', 'scenario', 'passengers',
  'actual_pp_cad_2025', 'actual_revenue_cad_2025_m',
  'baseline_pp_cad_2025', 'scenario_pp_cad_2025',
  'scenario_revenue_cad_2025_m', 'additional_revenue_cad_2025_m',
  'difference_pp_cad_2025'), 'Annual counterfactual')

actual <- actual |> mutate(year = as.integer(year))
paths <- paths |> mutate(year = as.integer(year))
annual <- annual |> mutate(year = as.integer(year))
assert_panel(annual, c('scenario', 'airport', 'year'),
             length(main_scenarios) * 5L * length(years), 'main annual outputs')
if (!setequal(annual$scenario, main_scenarios) ||
    !setequal(annual$airport, c(can_airports, 'BIG4')) ||
    !setequal(annual$year, years) ||
    anyNA(annual) || any(annual$passengers <= 0)) {
  stop('Unexpected scenario, airport, year, or missing inputs in main outputs.')
}

# 1. AUSTRALIAN-ONLY INDEX INTERPOLATION --------------------------------------
au <- actual |>
  filter(country == 'Australia', airport %in% aus_airports,
         year %in% years) |>
  transmute(airport, year, observed_index_2015 = as.numeric(index_2015)) |>
  arrange(airport, year)
assert_panel(au, c('airport', 'year'), 4L * length(years), 'AU airport indices')
if (!setequal(au$airport, aus_airports) ||
    !setequal(au$year, years) ||
    any(!is.finite(au$observed_index_2015)) ||
    any(au$observed_index_2015 <= 0) ||
    any(abs(au$observed_index_2015[au$year == 2015] - 100) > tolerance)) {
  stop('Australian index data failed coverage, positivity or rebasing checks.')
}

au <- au |>
  group_by(airport) |>
  mutate(
    anchor_2019 = observed_index_2015[year == 2019],
    anchor_2023 = observed_index_2015[year == 2023],
    interpolated_index_2015 = if_else(
      year %in% pandemic_years,
      anchor_2019 + ((year - 2019) / 4) * (anchor_2023 - anchor_2019),
      observed_index_2015
    )
  ) |>
  ungroup() |>
  select(airport, year, observed_index_2015, interpolated_index_2015)

au_mean <- au |>
  group_by(year) |>
  summarise(
    n_airports = n_distinct(airport),
    observed_index_2015 = mean(observed_index_2015),
    interpolated_index_2015 = mean(interpolated_index_2015),
    .groups = 'drop'
  )
if (any(au_mean$n_airports != 4L) ||
    any(!is.finite(au_mean$interpolated_index_2015)) ||
    any(au_mean$interpolated_index_2015 <= 0)) {
  stop('Australian interpolation is incomplete or nonpositive.')
}

# Reconcile the independently reconstructed annual mean with the main model.
main_gradual <- paths |>
  filter(scenario == 'GRADUAL') |>
  select(year, growth_multiple_2015)
assert_panel(main_gradual, 'year', length(years), 'GRADUAL path')
reconcile <- au_mean |>
  left_join(main_gradual, by = 'year')
observed_is_primary <- all(abs(reconcile$observed_index_2015 / 100 -
                               reconcile$growth_multiple_2015) < tolerance)
adjusted_is_primary <- all(abs(reconcile$interpolated_index_2015 / 100 -
                               reconcile$growth_multiple_2015) < tolerance)
if (anyNA(reconcile$growth_multiple_2015) ||
    !(observed_is_primary || adjusted_is_primary)) {
  stop('Main GRADUAL path matches neither the observed nor interpolated index.')
}
if (any(abs(au_mean$interpolated_index_2015[
              !au_mean$year %in% pandemic_years] -
            au_mean$observed_index_2015[
              !au_mean$year %in% pandemic_years]) > tolerance)) {
  stop('Interpolation unexpectedly modified non-pandemic years.')
}

index_diagnostic <- bind_rows(
  au,
  au_mean |>
    transmute(airport = 'AUS4', year,
              observed_index_2015, interpolated_index_2015)
) |>
  mutate(index_difference_pp = interpolated_index_2015 - observed_index_2015) |>
  arrange(match(airport, c(aus_airports, 'AUS4')), year)

# Normalize the sensitivity comparisons to the OBSERVED original path,
# regardless of which path the current primary reporting pipeline selected.
# This preserves the historical meaning of GRADUAL and GRADUAL_INTERP here.
if (!observed_is_primary && adjusted_is_primary) {
  original_airports <- annual |>
    filter(scenario == 'GRADUAL', airport %in% can_airports) |>
    left_join(au_mean |>
                transmute(year, observed_multiple = observed_index_2015 / 100),
              by = 'year') |>
    mutate(
      scenario_pp_cad_2025 = baseline_pp_cad_2025 * observed_multiple,
      scenario_revenue_cad_2025_m = scenario_pp_cad_2025 * passengers / 1e6,
      additional_revenue_cad_2025_m = scenario_revenue_cad_2025_m -
        actual_revenue_cad_2025_m,
      difference_pp_cad_2025 = scenario_pp_cad_2025 - actual_pp_cad_2025
    ) |>
    select(all_of(names(annual)))
  original_big4_base <- annual |>
    filter(scenario == 'GRADUAL', airport == 'BIG4', year == 2015) |>
    pull(baseline_pp_cad_2025)
  original_big4 <- original_airports |>
    group_by(scenario, year) |>
    summarise(
      passengers = sum(passengers),
      actual_revenue_cad_2025_m = sum(actual_revenue_cad_2025_m),
      scenario_revenue_cad_2025_m = sum(scenario_revenue_cad_2025_m),
      additional_revenue_cad_2025_m = sum(additional_revenue_cad_2025_m),
      .groups = 'drop'
    ) |>
    mutate(
      airport = 'BIG4', baseline_pp_cad_2025 = original_big4_base,
      actual_pp_cad_2025 = actual_revenue_cad_2025_m * 1e6 / passengers,
      scenario_pp_cad_2025 = scenario_revenue_cad_2025_m * 1e6 / passengers,
      difference_pp_cad_2025 = additional_revenue_cad_2025_m * 1e6 / passengers
    ) |>
    select(all_of(names(annual)))
  annual <- bind_rows(
    annual |> filter(scenario != 'GRADUAL'), original_airports, original_big4
  )
  assert_panel(annual, c('scenario', 'airport', 'year'),
               length(main_scenarios) * 5L * length(years),
               'reconstructed observed reference')
}

# 2. REPRICE ONLY THE GRADUAL SCENARIO USING THE ALTERNATIVE INDEX ------------
# Recompute at each Canadian airport, THEN sum for BIG4. Do not multiply a
# BIG4-average baseline yield by current total traffic (passenger mix changes).
interp_airports <- annual |>
  filter(scenario == 'GRADUAL', airport %in% can_airports) |>
  left_join(au_mean |>
              transmute(year,
                        interpolated_multiple = interpolated_index_2015 / 100),
            by = 'year') |>
  mutate(
    scenario = 'GRADUAL_INTERP',
    scenario_pp_cad_2025 = baseline_pp_cad_2025 * interpolated_multiple,
    scenario_revenue_cad_2025_m = scenario_pp_cad_2025 * passengers / 1e6,
    additional_revenue_cad_2025_m = scenario_revenue_cad_2025_m -
      actual_revenue_cad_2025_m,
    difference_pp_cad_2025 = scenario_pp_cad_2025 - actual_pp_cad_2025
  ) |>
  select(all_of(names(annual)))
assert_panel(interp_airports, c('scenario', 'airport', 'year'),
             length(can_airports) * length(years), 'interpolated Canadian airports')
if (anyNA(interp_airports)) stop('Missing interpolated airport calculations.')

big4_base <- annual |>
  filter(scenario == 'GRADUAL', airport == 'BIG4', year == 2015) |>
  pull(baseline_pp_cad_2025)
if (length(big4_base) != 1L || !is.finite(big4_base)) {
  stop('Main BIG4 2015 baseline is missing.')
}

interp_big4_sums <- interp_airports |>
  group_by(year, scenario) |>
  summarise(
    n_airports = n_distinct(airport),
    passengers = sum(passengers),
    actual_revenue_cad_2025_m = sum(actual_revenue_cad_2025_m),
    scenario_revenue_cad_2025_m = sum(scenario_revenue_cad_2025_m),
    additional_revenue_cad_2025_m = sum(additional_revenue_cad_2025_m),
    .groups = 'drop'
  )
if (any(interp_big4_sums$n_airports != 4L)) {
  stop('Interpolated BIG4 totals lack one or more airports.')
}
interp_big4 <- interp_big4_sums |>
  mutate(
    airport = 'BIG4',
    baseline_pp_cad_2025 = big4_base,
    actual_pp_cad_2025 = actual_revenue_cad_2025_m * 1e6 / passengers,
    scenario_pp_cad_2025 = scenario_revenue_cad_2025_m * 1e6 / passengers,
    difference_pp_cad_2025 = additional_revenue_cad_2025_m * 1e6 / passengers
  ) |>
  select(all_of(names(annual)))

annual_combined <- bind_rows(annual, interp_airports, interp_big4) |>
  arrange(match(scenario, all_scenarios), airport, year)
assert_panel(annual_combined, c('scenario', 'airport', 'year'),
             length(all_scenarios) * 5L * length(years), 'combined annual results')
if (anyNA(annual_combined) ||
    any(abs(annual_combined$additional_revenue_cad_2025_m[
          annual_combined$year == 2015]) > tolerance)) {
  stop('Combined scenario results contain missing values or a nonzero 2015 base.')
}

# Outside 2020-2022, the alternative scenario must equal the original GRADUAL.
unchanged_years <- annual_combined |>
  filter(scenario %in% c('GRADUAL', 'GRADUAL_INTERP'),
         !year %in% pandemic_years) |>
  select(airport, year, scenario, additional_revenue_cad_2025_m) |>
  pivot_wider(names_from = scenario,
              values_from = additional_revenue_cad_2025_m)
if (any(abs(unchanged_years$GRADUAL -
            unchanged_years$GRADUAL_INTERP) > tolerance)) {
  stop('Interpolated GRADUAL differs from the main model outside 2020-2022.')
}

# 3. CUMULATIVE EFFECT (2016-2025) --------------------------------------------
cumulative <- annual_combined |>
  filter(year > 2015) |>
  group_by(scenario, airport) |>
  summarise(
    years = n_distinct(year),
    actual_revenue_2016_2025_m = sum(actual_revenue_cad_2025_m),
    scenario_revenue_2016_2025_m = sum(scenario_revenue_cad_2025_m),
    additional_revenue_2016_2025_m = sum(additional_revenue_cad_2025_m),
    additional_revenue_2025_m = additional_revenue_cad_2025_m[year == 2025],
    .groups = 'drop'
  ) |>
  mutate(additional_revenue_pct_actual = 100 *
           additional_revenue_2016_2025_m / actual_revenue_2016_2025_m) |>
  arrange(match(scenario, all_scenarios), airport)
if (any(cumulative$years != 10L)) stop('Missing cumulative years.')

interp_effect <- cumulative |>
  filter(scenario %in% c('GRADUAL', 'GRADUAL_INTERP')) |>
  select(airport, scenario, additional_revenue_2016_2025_m) |>
  pivot_wider(names_from = scenario,
              values_from = additional_revenue_2016_2025_m) |>
  mutate(
    interpolation_change_cad_2025_m = GRADUAL_INTERP - GRADUAL,
    reduction_cad_2025_m = GRADUAL - GRADUAL_INTERP,
    reduction_pct_original = if_else(abs(GRADUAL) > tolerance,
                                     100 * reduction_cad_2025_m / GRADUAL,
                                     NA_real_)
  )

# 4. TIME DECOMPOSITION: same periods for all scenarios ------------------------
post_base <- annual_combined |> filter(year > 2015)
period_panel <- bind_rows(
  post_base |>
    mutate(period = case_when(
      year <= 2019 ~ '2016-2019',
      year <= 2022 ~ '2020-2022',
      TRUE ~ '2023-2025'
    )),
  post_base |> mutate(period = '2016-2025')
)
timing <- period_panel |>
  group_by(airport, scenario, period) |>
  summarise(
    years = n_distinct(year),
    passengers = sum(passengers),
    actual_revenue_cad_2025_m = sum(actual_revenue_cad_2025_m),
    scenario_revenue_cad_2025_m = sum(scenario_revenue_cad_2025_m),
    additional_revenue_cad_2025_m = sum(additional_revenue_cad_2025_m),
    .groups = 'drop'
  ) |>
  arrange(match(airport, c('BIG4', can_airports)),
          match(period, period_order), match(scenario, all_scenarios))

# Explicitly compare CAP_THEN_SHOCK with both versions of GRADUAL. Include
# IMMEDIATE_SHOCK to make the alternative historical timing visible too.
make_big4_comparison <- function(data, key) {
  wide <- data |>
    filter(airport == 'BIG4') |>
    select(all_of(key), scenario, additional_revenue_cad_2025_m) |>
    pivot_wider(names_from = scenario,
                values_from = additional_revenue_cad_2025_m)
  wide |>
    mutate(
      cap_minus_gradual_m = CAP_THEN_SHOCK - GRADUAL,
      cap_minus_gradual_interp_m = CAP_THEN_SHOCK - GRADUAL_INTERP,
      immediate_minus_gradual_m = IMMEDIATE_SHOCK - GRADUAL,
      gradual_interpolation_reduction_m = GRADUAL - GRADUAL_INTERP
    )
}
timing_big4 <- make_big4_comparison(timing, 'period') |>
  arrange(match(period, period_order))
annual_big4 <- make_big4_comparison(post_base, 'year') |>
  left_join(annual |>
              filter(scenario == 'GRADUAL', airport == 'BIG4', year > 2015) |>
              select(year, canadian_passengers = passengers),
            by = 'year') |>
  relocate(canadian_passengers, .after = year) |>
  arrange(year)

# Reconcile the period sums with annual and cumulative totals.
check_period <- timing |>
  filter(period == '2016-2025') |>
  left_join(cumulative |>
              select(airport, scenario, additional_revenue_2016_2025_m),
            by = c('airport', 'scenario'))
if (anyNA(check_period) ||
    any(abs(check_period$additional_revenue_cad_2025_m -
              check_period$additional_revenue_2016_2025_m) > tolerance)) {
  stop('Timing decomposition does not reconcile with cumulative results.')
}

# Write only after all checks have passed.
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
readr::write_csv(index_diagnostic,
                 file.path(out_dir, 'australian_index_interpolation.csv'))
readr::write_csv(annual_combined,
                 file.path(out_dir, 'counterfactual_sensitivity_annual.csv'))
readr::write_csv(cumulative,
                 file.path(out_dir, 'counterfactual_sensitivity_cumulative.csv'))
readr::write_csv(interp_effect,
                 file.path(out_dir, 'gradual_interpolation_effect.csv'))
readr::write_csv(timing,
                 file.path(out_dir, 'scenario_timing_decomposition.csv'))
readr::write_csv(timing_big4,
                 file.path(out_dir, 'scenario_timing_comparison_big4.csv'))
readr::write_csv(annual_big4,
                 file.path(out_dir, 'scenario_annual_comparison_big4.csv'))

cat('\nAustralian-only interpolation: GRADUAL cumulative effect (2025 CAD millions)\n')
print(interp_effect |> filter(airport == 'BIG4'), n = Inf)
cat('\nBIG4 timing comparison (2025 CAD millions)\n')
print(timing_big4, n = Inf)
message('Saved seven sensitivity CSVs to: ', out_dir)
