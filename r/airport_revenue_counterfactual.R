# Canadian airport aeronautical-revenue counterfactuals | V18 (November 2026)
# -------------------------------------------------------------------------
# THREE EMPIRICAL PATHS, applied to each Canadian airport's actual 2015
# aeronautical + AIF revenue per passenger (constant 2025 CAD):
#
# 1. GRADUAL:         Australian 2015-2025 average real revenue/pax growth.
# 2. CAP_THEN_SHOCK:  Australian 1998-2007 history mapped to Canada 2015-2024.
#                      The post-cap increases beginning in 2002 map to 2019.
# 3. IMMEDIATE_SHOCK: Australian 2001-2007 history mapped to Canada 2015-2021.
#                      Post-cap increases begin immediately in Canadian 2016.
#
# Each Australian airport is indexed to 1 in its starting year BEFORE taking
# the unweighted mean of BNE, MEL, PER, SYD. After the historical observations
# end in 2007, extend using the observed 2008-2019 real CAGR in the ACCC
# supplementary series (roughly 3.4% per year). Each airport is rebased to
# 2008 first; only the growth RATE is used, avoiding a break in the revenue
# definition between the historical chart and the newer ACCC data.
# Extrapolated years are flagged and are not observed financial results.
# The historical exercise does not prove causation or predict future prices.
# Revenue per passenger is an airport-charge proxy, not a posted-fee schedule.
# Canadian passenger volumes are held fixed: no price/demand feedback is
# modelled. Incremental revenue increases EBITDA dollar-for-dollar only
# under the separate fixed-operating-cost assumption for 2023-2025.
# GRADUAL follows the observed annual Australian path, not a constant CAGR.
#
# Main assumptions:
#   - All scenarios begin at each Canadian airport's actual 2015 real
#     aeronautical + AIF revenue per passenger.
#   - Australian real per-passenger revenue growth gives illustrative
#     trajectories, not estimated causal effects of privatization.
#   - Four Australian airports receive equal weight in each growth index.
#   - Canadian passenger volumes are fixed at observed levels; pricing-related
#     changes in passenger demand are not modelled.
#   - Historical paths extrapolate beyond 2007 at the 2008-2019 Australian
#     annualized growth rate; extrapolated observations are identified.
#   - Aeronautical revenue/passenger approximates pricing, but also reflects
#     changes in service and passenger composition.
#   - For 2023-2025 only, the EBITDA sensitivity holds operating costs
#     fixed: each $1 change in aeronautical revenue changes EBITDA by $1.
#     This does not estimate EBIT, taxes or after-tax earnings, and is
#     not a forecast of realized profitability.
#   - Australia has June-ending fiscal years; Canada has calendar years.
#     The observations are aligned by ending year, not identical periods.

# Inputs:
#   output/airport_comparison/aero_revenue_growth_2015_2025.csv
#   output/airport_comparison/comparison_annual_absolute_2023_2025.csv
#   output/accc_legacy_vector_selected.csv (includes Sydney 1998)
#   output/accc_airport_financial_analysis_2007_2025.xlsx (per_passenger sheet)
# Main outputs (output/airport_counterfactual/):
#   scenario_growth_paths.csv, actual_growth_canada_australia.csv,
#   counterfactual_annual.csv, counterfactual_summary.csv,
#   ebitda_annual_2023_2025.csv, airport_counterfactual.xlsx.
# Plot creation belongs to airport_counterfactual_plots.R.
# Notes are embedded below so there is no external notes file to maintain.

library(tidyverse)
library(here)

# PATHS AND LOCATIONS ------------------------------------------------------
start_year <- 2015L
end_year <- 2025L
years <- start_year:end_year
can_airports <- c('YYC', 'YUL', 'YVR', 'YYZ')
aus_airports <- c('BNE', 'MEL', 'PER', 'SYD')

input_path <- here('output', 'airport_comparison',
                   'aero_revenue_growth_2015_2025.csv')
profitability_path <- here('output', 'airport_comparison',
                           'comparison_annual_absolute_2023_2025.csv')
historical_path <- here('output', 'accc_legacy_vector_selected.csv')
out_dir <- here('output', 'airport_counterfactual')
long_run_path <- here('output', 'accc_airport_financial_analysis_2007_2025.xlsx')

# Verify that all three upstream datasets exist before reading them.
if (!file.exists(long_run_path)) stop('Missing ACCC financial workbook: ', long_run_path)
if (!file.exists(input_path)) stop('Missing comparison input: ', input_path)
if (!file.exists(profitability_path)) stop('Missing financial comparison: ', profitability_path)
if (!file.exists(historical_path)) stop('Missing historical ACCC CSV: ', historical_path)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

scenario_info <- tribble(
  ~scenario,          ~label,                              ~type,
  'GRADUAL',          'Australian gradual growth',           'recent_2015_2025',
  'CAP_THEN_SHOCK',   'Australian cap period, then shock',   'historical_1998_2007',
  'IMMEDIATE_SHOCK',  'Australian immediate post-cap shock','historical_2001_2007'
)

# CANADIAN REVENUE AND RECENT AUSTRALIAN GROWTH ----------------------------
source <- readr::read_csv(input_path, show_col_types = FALSE)
needed <- c('country', 'airport', 'year', 'variable', 'passengers',
            'amount_cad_2025_m', 'per_passenger_cad_2025',
            'real_pp_index_2015')
# Confirm the comparison export includes every field used in the scenarios.
if (!all(needed %in% names(source))) {
  stop('Missing comparison columns: ', paste(setdiff(needed, names(source)),
                                              collapse = ', '))
}

can <- source |>
  filter(country == 'Canada', airport %in% can_airports,
         variable == 'aero_plus_aif', year %in% years) |>
  transmute(airport, year = as.integer(year),
            passengers = as.numeric(passengers),
            actual_pp_cad_2025 = as.numeric(per_passenger_cad_2025),
            actual_revenue_cad_2025_m = as.numeric(amount_cad_2025_m))

aus <- source |>
  filter(country == 'Australia', airport %in% aus_airports,
         variable == 'aero_revenue', year %in% years) |>
  transmute(airport, year = as.integer(year),
            index = as.numeric(real_pp_index_2015) / 100)

# Require one finite observation for every airport-year, with no duplicate keys.
check_panel <- function(data, airports, numeric_cols) {
  if (nrow(data) != length(airports) * length(years) ||
      anyDuplicated(data[c('airport', 'year')]) ||
      !setequal(data$airport, airports) || !setequal(data$year, years) ||
      any(!is.finite(as.matrix(data[numeric_cols])))) {
    stop('Missing or duplicated airport-year source data.')
  }
}
check_panel(can, can_airports,
            c('passengers', 'actual_pp_cad_2025', 'actual_revenue_cad_2025_m'))
check_panel(aus, aus_airports, 'index')
# Check positive traffic/yields, Australian 2015 rebasing, and Canadian
# revenue = revenue per passenger × passengers (allowing rounding).
if (any(can$passengers <= 0) || any(can$actual_pp_cad_2025 <= 0) ||
    any(aus$index <= 0) ||
    any(abs(aus$index[aus$year == start_year] - 1) > 1e-8) ||
    any(abs(can$actual_pp_cad_2025 * can$passengers / 1e6 -
              can$actual_revenue_cad_2025_m) > 1e-4)) {
  stop('Revenue, passengers or 2015 indices do not reconcile.')
}

recent_path <- aus |>
  group_by(year) |>
  summarise(growth_multiple_2015 = mean(index), .groups = 'drop') |>
  mutate(scenario = 'GRADUAL', australian_year = year,
         source_status = 'observed')

# OBSERVED CANADA–AUSTRALIA GROWTH AND LEVEL COMPARISON ---------------------
# Preserve actual annual revenue and revenue per passenger, alongside the
# airport-specific growth indices (2015 = 100). Figures are real 2025 CAD.
# The Australian source is converted from real AUD using the comparison
# pipeline's exchange-rate assumption; actual levels therefore have a
# currency-conversion qualification not needed for percentage growth.
# Aggregates: SUM revenue and passengers, and calculate passenger-weighted
# revenue per passenger. The aggregate growth index remains the UNWEIGHTED
# mean of the four airports' individual growth indices, as in V16.
actual_growth_airports <- source |>
  filter(
    (country == 'Canada' & airport %in% can_airports &
       variable == 'aero_plus_aif') |
      (country == 'Australia' & airport %in% aus_airports &
         variable == 'aero_revenue'),
    year %in% years
  ) |>
  transmute(
    country, airport, year = as.integer(year),
    passengers = as.numeric(passengers),
    actual_revenue_cad_2025_m = as.numeric(amount_cad_2025_m),
    actual_pp_cad_2025 = as.numeric(per_passenger_cad_2025),
    index_2015 = as.numeric(real_pp_index_2015)
  )

actual_growth_aggregates <- actual_growth_airports |>
  group_by(country, year) |>
  summarise(
    passengers = sum(passengers),
    actual_revenue_cad_2025_m = sum(actual_revenue_cad_2025_m),
    actual_pp_cad_2025 = actual_revenue_cad_2025_m * 1e6 / passengers,
    index_2015 = mean(index_2015),
    .groups = 'drop'
  ) |>
  mutate(airport = if_else(country == 'Canada', 'BIG4', 'AUS4'))

actual_growth <- bind_rows(actual_growth_airports, actual_growth_aggregates) |>
  mutate(real_growth_since_2015_pct = index_2015 - 100) |>
  select(country, airport, year, passengers,
         actual_revenue_cad_2025_m, actual_pp_cad_2025,
         index_2015, real_growth_since_2015_pct) |>
  arrange(country, airport, year)

# One row per airport (plus aggregates), with endpoint levels and growth.
actual_growth_2025 <- actual_growth |>
  filter(year %in% c(start_year, end_year)) |>
  select(country, airport, year,
         actual_revenue_cad_2025_m, actual_pp_cad_2025) |>
  pivot_wider(
    names_from = year,
    values_from = c(actual_revenue_cad_2025_m, actual_pp_cad_2025),
    names_glue = '{.value}_{year}'
  ) |>
  left_join(
    actual_growth |>
      filter(year == end_year) |>
      select(country, airport, index_2015, real_growth_since_2015_pct),
    by = c('country', 'airport')
  ) |>
  transmute(
    country, airport,
    revenue_2015_cad_2025_m = actual_revenue_cad_2025_m_2015,
    revenue_2025_cad_2025_m = actual_revenue_cad_2025_m_2025,
    pp_2015_cad_2025 = actual_pp_cad_2025_2015,
    pp_2025_cad_2025 = actual_pp_cad_2025_2025,
    index_2015, real_growth_since_2015_pct
  ) |>
  arrange(country, airport)

# Validate the ten airport/aggregate growth series: complete years,
# unique keys, finite values, 2015 = 100 and revenue arithmetic.
if (nrow(actual_growth) != 10L * length(years) ||
    nrow(actual_growth_2025) != 10L ||
    anyDuplicated(actual_growth[c('country', 'airport', 'year')]) ||
    any(!is.finite(as.matrix(actual_growth[
      c('passengers', 'actual_revenue_cad_2025_m',
        'actual_pp_cad_2025', 'index_2015')
    ]))) ||
    any(actual_growth$passengers <= 0) ||
    any(abs(actual_growth$index_2015[actual_growth$year == start_year] - 100) > 1e-8) ||
    any(abs(actual_growth$actual_pp_cad_2025 * actual_growth$passengers / 1e6 -
              actual_growth$actual_revenue_cad_2025_m) > 1e-4)) {
  stop('Airport-level observed growth and actual revenue comparison is incomplete.')
}

# HISTORICAL AUSTRALIAN SERIES --------------------------------------------
# ACCC 2006-07 report, Figure 2.2: aeronautical revenue per passenger,
# adjusted for comparability (including Ansett); values remain NOMINAL AUD.
# Historical ABS CPI financial-year averages convert these to REAL indices.
# Sydney 1998 has been added from the 1997-98 FAC financial and traffic data.
# This manually sourced FAC observation is not an Ansett-adjusted ACCC bar;
# it may differ in reporting scope from other historical chart observations.
# Use the complete four-airport 1998-2007 panel.

# Australian CPI, aggregated to financial years ending June
historical_cpi <- readabs::read_cpi() |>
  mutate(
    year = as.integer(format(date, "%Y")) +
      as.integer(format(date, "%m") >= "07")
  ) |> 
  group_by(year) |>
  summarise(cpi = mean(cpi), .groups = "drop") |>
  filter(year %in% 1998:2007)

history <- readr::read_csv(historical_path, show_col_types = FALSE) |> 
  filter(airport %in% aus_airports, as.character(chart) == '2.2',
         measure == 'aero_revenue_per_pax',
         adjustment %in% c('ansett_adjusted', 'adjusted') |
           (airport == 'SYD' & year == 1998L &
              adjustment == 'FAC_reported'),
         year %in% 1998:2007) |>
  transmute(airport, year = as.integer(year),
            nominal_aud_pp = as.numeric(value)) |>
  left_join(historical_cpi, by = 'year') |>
  mutate(real_yield = nominal_aud_pp / cpi)

# Require all 40 historical airport-years, with unique keys and
# positive real yields after the CPI join.
if (nrow(history) != 4L * 10L ||
    anyDuplicated(history[c('airport', 'year')]) ||
    any(!is.finite(history$real_yield)) ||
    any(history$real_yield <= 0)) {
  stop('Historical ACCC 1998-2007 four-airport panel is incomplete.')
}

# Real Australian growth from a given base year. An airport-specific index
# avoids overweighting airports with higher starting AUD-per-passenger charges.
historical_index <- function(base_au_year) {
  history |>
    group_by(airport) |>
    mutate(index = real_yield / real_yield[year == base_au_year]) |>
    ungroup() |>
    filter(year >= base_au_year) |>
    group_by(year) |>
    summarise(growth_multiple_2015 = mean(index), .groups = 'drop')
}

# LONG-RUN POST-SHOCK GROWTH -----------------------------------------------
# ACCC supplementary yields are already expressed in constant FY2024-25 AUD.
# Compute the 2008-2019 CAGR of the unweighted mean of individual airport
# indices, with each airport starting at 2008 = 1. This is a RATE, not a
# splice of potentially differently defined aeronautical revenue levels.
growth_endpoints <- readxl::read_excel(long_run_path, sheet = 'per_passenger') |> 
  filter(
    airport %in% aus_airports,
    year %in% c('2007-08', '2018-19')
  ) |>
  group_by(airport) |>
  summarise(
    y2008 = aero_revenue_per_passenger[year == '2007-08'],
    y2019 = aero_revenue_per_passenger[year == '2018-19'],
    .groups = 'drop'
  )

# Require valid 2008 and 2019 endpoints for all four Australian
# airports before calculating the post-2007 extrapolation rate.
stopifnot(
  nrow(growth_endpoints) == 4L,
  !anyNA(growth_endpoints),
  all(growth_endpoints$y2008 > 0)
)

post_shock_growth <- mean(
  growth_endpoints$y2019 / growth_endpoints$y2008
)^(1 / 11) - 1

# Map historical observations onto Canada from 2015. Only beyond Australian
# 2007 do the two shock scenarios use the 2008-2019 annualized growth rate.
replay_history <- function(base_au_year, scenario_name) {
  curve <- historical_index(base_au_year)
  index_2007 <- curve$growth_multiple_2015[curve$year == 2007L]

  tibble(
    scenario = scenario_name,
    year = years,
    australian_year = base_au_year + year - start_year
  ) |>
    left_join(curve, by = c('australian_year' = 'year')) |>
    mutate(
      growth_multiple_2015 = coalesce(
        growth_multiple_2015,
        index_2007 * (1 + post_shock_growth)^pmax(australian_year - 2007L, 0L)
      ),
      source_status = if_else(australian_year > 2007L,
                              'extrapolated_2008_2019_growth', 'observed')
    )
}

trajectories <- bind_rows(
  recent_path,
  replay_history(1998L, 'CAP_THEN_SHOCK'),
  replay_history(2001L, 'IMMEDIATE_SHOCK')
) |>
  left_join(scenario_info, by = 'scenario') |>
  mutate(real_growth_since_2015_pct = 100 * (growth_multiple_2015 - 1)) |>
  select(scenario, label, type, year, australian_year, source_status,
         growth_multiple_2015, real_growth_since_2015_pct) |>
  arrange(match(scenario, scenario_info$scenario), year)

# Check all three annual scenario paths are complete, unique,
# finite, positive and normalized to 1 in Canadian base year 2015.
if (nrow(trajectories) != 3L * length(years) ||
    anyDuplicated(trajectories[c('scenario', 'year')]) ||
    any(!is.finite(trajectories$growth_multiple_2015)) ||
    any(trajectories$growth_multiple_2015 <= 0) ||
    any(abs(trajectories$growth_multiple_2015[
      trajectories$year == start_year] - 1) > 1e-8)) {
  stop('Invalid scenario trajectories.')
}

# COMMON CANADIAN COUNTERFACTUAL ------------------------------------------
can_base <- can |>
  filter(year == start_year) |>
  select(airport, baseline_pp_cad_2025 = actual_pp_cad_2025)

annual_airport <- tidyr::crossing(can, scenario = scenario_info$scenario) |>
  left_join(can_base, by = 'airport') |>
  left_join(trajectories |>
              select(scenario, year, growth_multiple_2015),
            by = c('scenario', 'year')) |>
  mutate(
    scenario_pp_cad_2025 = baseline_pp_cad_2025 * growth_multiple_2015,
    scenario_revenue_cad_2025_m = scenario_pp_cad_2025 * passengers / 1e6,
    additional_revenue_cad_2025_m = scenario_revenue_cad_2025_m -
      actual_revenue_cad_2025_m,
    difference_pp_cad_2025 = scenario_pp_cad_2025 - actual_pp_cad_2025
  ) |>
  select(-growth_multiple_2015)

# Aggregate 2015 aeronautical + AIF revenue per passenger (weighted by traffic).
big4_baseline <- can |>
  filter(year == start_year) |>
  summarise(value = sum(actual_revenue_cad_2025_m) * 1e6 / sum(passengers)) |>
  pull(value)

annual_big4 <- annual_airport |>
  group_by(scenario, year) |>
  summarise(n_airports = n_distinct(airport),
            passengers = sum(passengers),
            actual_revenue_cad_2025_m = sum(actual_revenue_cad_2025_m),
            scenario_revenue_cad_2025_m = sum(scenario_revenue_cad_2025_m),
            additional_revenue_cad_2025_m = sum(additional_revenue_cad_2025_m),
            .groups = 'drop') |>
  mutate(airport = 'BIG4', baseline_pp_cad_2025 = big4_baseline,
         actual_pp_cad_2025 = actual_revenue_cad_2025_m * 1e6 / passengers,
         scenario_pp_cad_2025 = scenario_revenue_cad_2025_m * 1e6 / passengers,
         difference_pp_cad_2025 = additional_revenue_cad_2025_m * 1e6 / passengers)

annual <- bind_rows(annual_airport, annual_big4 |> select(-n_airports)) |>
  arrange(match(scenario, scenario_info$scenario), airport, year)

# Ensure BIG4 includes four airports per scenario-year, all annual
# keys are unique and complete, and 2015 adds zero counterfactual revenue.
if (any(annual_big4$n_airports != 4L) ||
    nrow(annual) != 3L * 5L * length(years) ||
    anyDuplicated(annual[c('scenario', 'airport', 'year')]) ||
    anyNA(annual[c('passengers', 'scenario_revenue_cad_2025_m',
                   'additional_revenue_cad_2025_m')]) ||
    any(abs(annual$additional_revenue_cad_2025_m[
      annual$year == start_year]) > 1e-4)) {
  stop('Canadian counterfactual totals do not reconcile.')
}

# CUMULATIVE RESULTS -------------------------------------------------------
summary <- annual |>
  filter(year > start_year) |>
  group_by(scenario, airport) |>
  summarise(
    years = n_distinct(year),
    actual_revenue_2016_2025_m = sum(actual_revenue_cad_2025_m),
    scenario_revenue_2016_2025_m = sum(scenario_revenue_cad_2025_m),
    additional_revenue_2016_2025_m = sum(additional_revenue_cad_2025_m),
    additional_revenue_2025_m =
      additional_revenue_cad_2025_m[year == end_year],
    .groups = 'drop'
  ) |>
  mutate(additional_revenue_pct_actual = 100 *
           additional_revenue_2016_2025_m / actual_revenue_2016_2025_m) |>
  left_join(trajectories |>
              filter(year == end_year) |>
              select(scenario, label, type, real_growth_since_2015_pct),
            by = 'scenario') |>
  arrange(match(scenario, scenario_info$scenario), airport)

# Require ten post-baseline years (2016–2025) for every
# scenario-airport cumulative revenue total.
if (any(summary$years != end_year - start_year)) {
  stop('Incomplete cumulative Canadian airport series.')
}

# FIXED-OPERATING-COST EBITDA SENSITIVITY: 2023-2025 -----------------------
# Recent Canadian financial data give total revenue and EBITDA in real
# 2025 CAD millions. Aeronautical+AIF revenue is already included in total
# revenue, so add ONLY the incremental revenue from each scenario.
# Holding operating costs unchanged makes the incremental EBITDA equal to
# incremental aeronautical revenue. This is an accounting sensitivity, not
# an estimate of EBIT, after-tax profit or the actual effects of privatization.
profitability_years <- 2023:2025
financial_actual <- readr::read_csv(profitability_path, show_col_types = FALSE) |>
  filter(country == 'Canada', airport %in% can_airports,
         year %in% profitability_years) |>
  transmute(
    airport, year = as.integer(year),
    actual_total_revenue_cad_2025_m = as.numeric(total_revenue),
    actual_aero_plus_aif_cad_2025_m = as.numeric(total_aero),
    actual_ebitda_cad_2025_m = as.numeric(ebitda)
  )

# Require one complete financial record for each Canadian airport/year,
# with positive revenue and aeronautical+AIF no greater than total revenue.
if (nrow(financial_actual) != length(can_airports) * length(profitability_years) ||
    anyDuplicated(financial_actual[c('airport', 'year')]) ||
    any(!is.finite(as.matrix(financial_actual[-(1:2)]))) ||
    any(financial_actual$actual_total_revenue_cad_2025_m <= 0) ||
    any(financial_actual$actual_aero_plus_aif_cad_2025_m <= 0) ||
    any(financial_actual$actual_aero_plus_aif_cad_2025_m >
          financial_actual$actual_total_revenue_cad_2025_m + 1e-4)) {
  stop('Incomplete or inconsistent Canadian 2023-2025 financial inputs.')
}

profitability_airport <- annual |>
  filter(airport %in% can_airports, year %in% profitability_years) |>
  left_join(financial_actual, by = c('airport', 'year')) |>
  mutate(
    scenario_total_revenue_cad_2025_m =
      actual_total_revenue_cad_2025_m + additional_revenue_cad_2025_m,
    scenario_ebitda_cad_2025_m =
      actual_ebitda_cad_2025_m + additional_revenue_cad_2025_m
  )

# Confirm the detailed comparison and revenue counterfactual agree on
# actual aeronautical+AIF revenue, and projected total revenue stays positive.
if (nrow(profitability_airport) != 3L * length(can_airports) *
      length(profitability_years) ||
    anyNA(profitability_airport) ||
    any(abs(profitability_airport$actual_aero_plus_aif_cad_2025_m -
              profitability_airport$actual_revenue_cad_2025_m) > 1e-4) ||
    any(profitability_airport$scenario_total_revenue_cad_2025_m <= 0)) {
  stop('EBITDA inputs do not reconcile to the revenue counterfactual.')
}

# Calculate the BIG4 aggregate from summed amounts, not averaged margins.
profitability_big4 <- profitability_airport |>
  group_by(scenario, year) |>
  summarise(
    across(c(actual_total_revenue_cad_2025_m,
             actual_aero_plus_aif_cad_2025_m,
             actual_ebitda_cad_2025_m,
             scenario_total_revenue_cad_2025_m,
             scenario_ebitda_cad_2025_m,
             additional_revenue_cad_2025_m), sum),
    .groups = 'drop'
  ) |>
  mutate(airport = 'BIG4')

profitability_amounts <- bind_rows(
  profitability_airport |>
    select(scenario, airport, year,
           actual_total_revenue_cad_2025_m,
           actual_aero_plus_aif_cad_2025_m,
           actual_ebitda_cad_2025_m,
           scenario_total_revenue_cad_2025_m,
           scenario_ebitda_cad_2025_m,
           additional_revenue_cad_2025_m),
  profitability_big4
)

# Compute margins after constructing annual amounts or three-year pooled
# totals; averaging annual margin percentages would produce different values.
add_ebitda_margins <- function(data) {
  data |>
    mutate(
      actual_ebitda_margin_pct = 100 * actual_ebitda_cad_2025_m /
        actual_total_revenue_cad_2025_m,
      scenario_ebitda_margin_pct = 100 * scenario_ebitda_cad_2025_m /
        scenario_total_revenue_cad_2025_m,
      ebitda_margin_change_pp = scenario_ebitda_margin_pct -
        actual_ebitda_margin_pct
    )
}

profitability_annual <- profitability_amounts |>
  add_ebitda_margins() |>
  arrange(match(scenario, scenario_info$scenario), airport, year)

profitability_pooled <- profitability_amounts |>
  group_by(scenario, airport) |>
  summarise(
    across(-year, sum),
    years = n_distinct(year),
    .groups = 'drop'
  ) |>
  add_ebitda_margins() |>
  arrange(match(scenario, scenario_info$scenario), airport)

# Check full annual/pooled coverage, finite figures, positive scenario
# revenues, and a dollar-for-dollar EBITDA change equal to added revenue.
if (nrow(profitability_annual) != 3L * 5L * length(profitability_years) ||
    anyDuplicated(profitability_annual[c('scenario', 'airport', 'year')]) ||
    nrow(profitability_pooled) != 3L * 5L ||
    any(profitability_pooled$years != length(profitability_years)) ||
    any(!is.finite(as.matrix(profitability_annual |> select(where(is.numeric))))) ||
    any(!is.finite(as.matrix(profitability_pooled |> select(where(is.numeric))))) ||
    any(profitability_pooled$scenario_total_revenue_cad_2025_m <= 0) ||
    any(abs((profitability_annual$scenario_ebitda_cad_2025_m -
              profitability_annual$actual_ebitda_cad_2025_m) -
              profitability_annual$additional_revenue_cad_2025_m) > 1e-7)) {
  stop('Fixed-cost EBITDA sensitivity failed validation.')
}

# The incremental revenue in the EBITDA sensitivity must match the
# original revenue counterfactual for every scenario, airport and year.
profit_revenue_check <- profitability_annual |>
  select(scenario, airport, year, additional_revenue_cad_2025_m) |>
  left_join(annual |>
              filter(year %in% profitability_years) |>
              select(scenario, airport, year,
                     revenue_model_additional_m = additional_revenue_cad_2025_m),
            by = c('scenario', 'airport', 'year'))
if (nrow(profit_revenue_check) != nrow(profitability_annual) ||
    anyNA(profit_revenue_check$revenue_model_additional_m) ||
    any(abs(profit_revenue_check$additional_revenue_cad_2025_m -
              profit_revenue_check$revenue_model_additional_m) > 1e-7)) {
  stop('EBITDA sensitivity and revenue scenarios do not reconcile.')
}

# OUTPUT -------------------------------------------------------------------
readr::write_csv(trajectories,
                 file.path(out_dir, 'scenario_growth_paths.csv'))
readr::write_csv(actual_growth,
                 file.path(out_dir, 'actual_growth_canada_australia.csv'))
readr::write_csv(annual,
                 file.path(out_dir, 'counterfactual_annual.csv'))
readr::write_csv(summary,
                 file.path(out_dir, 'counterfactual_summary.csv'))
readr::write_csv(profitability_annual,
                 file.path(out_dir, 'ebitda_annual_2023_2025.csv'))

# Spreadsheet-friendly headline summary: 2025 levels alongside cumulative totals.
# The main CSV summary stays unchanged; the Excel summary is a shorter view.
summary_excel <- summary |>
  left_join(
    annual |>
      filter(year == end_year) |>
      select(
        scenario, airport,
        actual_revenue_cad_2025_m,
        scenario_revenue_cad_2025_m,
        additional_revenue_cad_2025_m,
        actual_pp_cad_2025,
        scenario_pp_cad_2025,
        difference_pp_cad_2025
      ),
    by = c('scenario', 'airport')
  ) |>
  transmute(
    `Scenario` = label,
    `Airport` = airport,
    `2025 actual revenue ($m)` = actual_revenue_cad_2025_m,
    `2025 counterfactual revenue ($m)` = scenario_revenue_cad_2025_m,
    `2025 additional revenue ($m)` = additional_revenue_cad_2025_m,
    `2025 actual revenue/passenger ($)` = actual_pp_cad_2025,
    `2025 counterfactual revenue/passenger ($)` = scenario_pp_cad_2025,
    `2025 additional revenue/passenger ($)` = difference_pp_cad_2025,
    `2025 additional revenue (%)` = 100 * additional_revenue_cad_2025_m / actual_revenue_cad_2025_m,
    `2016-2025 actual revenue ($m)` = actual_revenue_2016_2025_m,
    `2016-2025 counterfactual revenue ($m)` = scenario_revenue_2016_2025_m,
    `2016-2025 additional revenue ($m)` = additional_revenue_2016_2025_m
  )

# Confirm the 2025 headline join preserves every summary row
# and introduces no missing spreadsheet values.
if (nrow(summary_excel) != nrow(summary) || anyNA(summary_excel)) {
  stop('Excel summary is incomplete; check the 2025 annual join.')
}

# NOTES FOR EXCEL WORKBOOK ------------------------------------------------
# Original working notes embedded here verbatim (including preliminary figures)
# to avoid manually pasting the sheet into each newly generated workbook.
# Update these strings when revising the research notes; they are not
# recalculated by the counterfactual.
notes_excel <- tibble(
  Note = c(
    "Australia's competition monitor (ACCC) did a report this year (2026) on the determinants of airfares: https://www.accc.gov.au/system/files/accc-submission-productivity-commission-inquiry-into-the-determinants-into-regional-airfares.pdf",
    "They pointed to airport charges as a key factor and connect it to the monopoly power of the big AUS airports to raise their prices (page 24)",
    "It was found that real aero revenues per passenger grew by 38.3% on average from 2015 to 2025",
    "They also said the current price monitoring regime was not sufficient to prevent the abuse of airport market power",
    NA_character_,
    "Both sets of airport have comparable revenue per-passenger levels after the AIF is included, so it might be better to use growth rates instead",
    "Over the same time, Canada's Big 4 had much lower per-passenger revenue growth (it actually fell at CGY and VAN), so they not really using their market power to maximize charge revenue",
    "What if Canada's Big 4 had followed a similar path as Australia's major private, market-power abusing airports?",
    "We can take the historical path of Australia's airports post-privitization and apply it as a counter-factual scenario to Canada's Big 4 over the last 10 years using actual data.",
    "Everything is based on real change in per-passenger aeronautical revenue in 2025 CAD",
    NA_character_,
    "Three counterfactual scenarios",
    "1. Baseline: The calculated average real growth rate of the AUS big 4 was 38.4% from 2015 to 2025 (same as the ACCC). This can be the baseline scenario, a slow but steady increase over time, nothing extreme.",
    "2. Policy cap, then shock: Uses the growth path of AUS airports post-privitization (1997-2007), where prices were capped for 5 years (CPI-X), then suddenly shot up when the cap lifted, using average growth in the post-shock years. Results in about 58% revenue growth",
    "3. No cap, sudden shock: There is no post-priv cap on price growth and the price shock is immediate, using average growth for the post-shock years. Results in about 80% revenue growth."
  )
)

# Simple workbook for spreadsheet users.
writexl::write_xlsx(
  list(
    Summary = summary_excel,
    Annual = annual,
    Actual_growth = actual_growth,
    Actual_growth_2025 = actual_growth_2025,
    Growth_paths = trajectories,
    EBITDA_annual = profitability_annual,
    EBITDA_pooled = profitability_pooled,
    Notes = notes_excel
  ),
  file.path(out_dir, 'airport_counterfactual.xlsx')
)

print(summary |>
        filter(airport == 'BIG4') |>
        select(scenario, real_growth_since_2015_pct,
               additional_revenue_2025_m, additional_revenue_2016_2025_m),
      n = Inf)
message(sprintf('Post-shock growth: %.2f%% annually (ACCC 2008-2019).', 100 * post_shock_growth))
message('Saved 5 analytical CSVs and the counterfactual workbook to: ', out_dir)
