# Canadian airport aeronautical-revenue counterfactuals | V17 (November 2026)
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
#
# Inputs:
#   output/airport_comparison/aero_revenue_growth_2015_2025.csv
#   output/accc_legacy_vector_selected.csv (includes Sydney 1998)
#   output/accc_airport_financial_analysis_2007_2025.xlsx (per_passenger sheet)
# Main outputs: five CSVs + airport-level comparison charts + one Excel workbook in output/airport_counterfactual/
# Separate no-COVID ETS sensitivity and backtest CSVs + workbook:
#   output/airport_counterfactual/ets_sensitivity/
# Note: Australia has June-ending fiscal years; Canada has calendar years.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(readxl)
})

# PATHS AND LOCATIONS ------------------------------------------------------
start_year <- 2015L
end_year <- 2025L
years <- start_year:end_year
can_airports <- c('YYC', 'YUL', 'YVR', 'YYZ')
aus_airports <- c('BNE', 'MEL', 'PER', 'SYD')

input_path <- here('output', 'airport_comparison',
                   'aero_revenue_growth_2015_2025.csv')
historical_path <- here('output', 'accc_legacy_vector_selected.csv')
out_dir <- here('output', 'airport_counterfactual')
long_run_path <- here('output', 'accc_airport_financial_analysis_2007_2025.xlsx')

if (!file.exists(long_run_path)) stop('Missing ACCC financial workbook: ', long_run_path)
if (!file.exists(input_path)) stop('Missing comparison input: ', input_path)
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

# Keep essential source checks; don't silently use incomplete airport panels.
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
         adjustment %in% c('ansett_adjusted', 'adjusted'),
         year %in% 1998:2007) |>
  transmute(airport, year = as.integer(year),
            nominal_aud_pp = as.numeric(value)) |>
  left_join(historical_cpi, by = 'year') |>
  mutate(real_yield = nominal_aud_pp / cpi)

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
    additional_revenue_ex_2020_22_m =
      sum(additional_revenue_cad_2025_m[!year %in% 2020:2022]),
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

if (any(summary$years != end_year - start_year)) {
  stop('Incomplete cumulative Canadian airport series.')
}

# OUTPUT -------------------------------------------------------------------
readr::write_csv(trajectories,
                 file.path(out_dir, 'scenario_growth_paths.csv'))
readr::write_csv(actual_growth,
                 file.path(out_dir, 'actual_growth_canada_australia.csv'))
readr::write_csv(actual_growth_2025,
                 file.path(out_dir, 'actual_growth_2025.csv'))
readr::write_csv(annual,
                 file.path(out_dir, 'counterfactual_annual.csv'))
readr::write_csv(summary,
                 file.path(out_dir, 'counterfactual_summary.csv'))

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

if (nrow(summary_excel) != nrow(summary) || anyNA(summary_excel)) {
  stop('Excel summary is incomplete; check the 2025 annual join.')
}

# Simple workbook for spreadsheet users; no ETS forecasts in the headline file.
writexl::write_xlsx(
  list(
    Summary = summary_excel,
    Annual = annual,
    Actual_growth = actual_growth,
    Actual_growth_2025 = actual_growth_2025,
    Growth_paths = trajectories
  ),
  file.path(out_dir, 'airport_counterfactual.xlsx')
)

print(summary |>
        filter(airport == 'BIG4') |>
        select(scenario, real_growth_since_2015_pct,
               additional_revenue_2025_m, additional_revenue_2016_2025_m),
      n = Inf)
message(sprintf('Post-shock growth: %.2f%% annually (ACCC 2008-2019).', 100 * post_shock_growth))
# Observed growth paths, including every airport and the two aggregates.
# Facet by country so the aggregate and four airport trajectories are readable.
actual_growth_plot <- actual_growth |>
  mutate(airport = factor(airport, levels = c(can_airports, aus_airports,
                                              'BIG4', 'AUS4'))) |>
  ggplot(aes(year, index_2015, colour = airport, group = airport)) +
  geom_hline(yintercept = 100, linetype = 'dashed', colour = 'grey65') +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ country) +
  labs(title = 'Actual real aeronautical revenue per passenger',
       subtitle = 'Individual airports and unweighted four-airport averages; 2015 = 100',
       x = NULL, y = '2015 = 100', colour = 'Airport') +
  theme_minimal()

ggsave(file.path(out_dir, 'actual_growth_canada_australia.png'),
       actual_growth_plot, width = 11, height = 5, dpi = 200)
print(actual_growth_plot)

# Level comparison complements the indices; levels use the source's 2025
# CAD conversion and are not a like-for-like comparison of fee schedules.
actual_levels_plot <- actual_growth |>
  ggplot(aes(year, actual_pp_cad_2025, colour = airport, group = airport)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ country) +
  labs(title = 'Actual real aeronautical revenue per passenger: levels',
       subtitle = 'Canada includes AIF; Australian values converted to 2025 CAD',
       x = NULL, y = '2025 CAD per passenger', colour = 'Airport') +
  theme_minimal()

ggsave(file.path(out_dir, 'actual_levels_canada_australia.png'),
       actual_levels_plot, width = 11, height = 5, dpi = 200)
print(actual_levels_plot)
message('Saved 5 headline CSVs, observed-growth and level charts, and 1 Excel workbook to: ', out_dir)


# NO-PANDEMIC PASSENGER DEMAND | ETS SENSITIVITY ONLY ---------------------
# This is not a fourth pricing scenario. The Australian price paths above
# and the main pricing counterfactual CSVs remain unchanged. Only Canadian passenger volumes
# for 2020-2025 are replaced with no-pandemic ETS forecasts.
# Keep all sensitivity outputs in a separate directory.

ets_out_dir <- file.path(out_dir, 'ets_sensitivity')
dir.create(ets_out_dir, recursive = TRUE, showWarnings = FALSE)

# Historical total enplaned + deplaned passengers, 2008-2019 (StatCan).
traffic_raw <- cansim::get_cansim('23-10-0253-01', factors = FALSE)

traffic_hist <- traffic_raw |>
  mutate(
    airport = case_when(
      str_detect(GEO, regex('^Calgary International', ignore_case = TRUE)) ~ 'YYC',
      str_detect(GEO, regex('^Montr.al.*Trudeau', ignore_case = TRUE)) ~ 'YUL',
      str_detect(GEO, regex('^Vancouver International', ignore_case = TRUE)) ~ 'YVR',
      str_detect(GEO, regex('^Toronto.*Pearson', ignore_case = TRUE)) ~ 'YYZ',
      TRUE ~ NA_character_
    ),
    year = as.integer(REF_DATE)
  ) |>
  filter(
    !is.na(airport),
    year %in% 2008:2019,
    `Air passenger traffic` == 'Total, passengers enplaned and deplaned',
    !is.na(VALUE)
  ) |>
  transmute(airport, year, passengers = as.numeric(VALUE)) |>
  arrange(airport, year)

if (nrow(traffic_hist) != length(can_airports) * 12L ||
    anyDuplicated(traffic_hist[c('airport', 'year')]) ||
    !setequal(traffic_hist$airport, can_airports) ||
    !setequal(traffic_hist$year, 2008:2019) ||
    any(!is.finite(traffic_hist$passengers)) ||
    any(traffic_hist$passengers <= 0)) {
  stop('Incomplete StatCan total-passenger panel for 2008-2019; check airport labels and categories.')
}

# Fit a damped, additive-trend ETS model to log passenger volumes for each
# airport. Exponentiating the forecasts produces positive passenger counts.
passenger_forecasts <- traffic_hist |>
  group_by(airport) |>
  group_modify(\(d, key) {
    d <- arrange(d, year)
    fit <- forecast::ets(
      ts(log(d$passengers), start = 2008, frequency = 1),
      model = 'AAN', damped = TRUE
    )
    tibble(
      year = 2020:2025,
      statcan_forecast_passengers = exp(as.numeric(
        forecast::forecast(fit, h = 6)$mean
      ))
    )
  }) |>
  ungroup() |>
  left_join(
    traffic_hist |>
      filter(year == 2019) |>
      transmute(airport, statcan_2019_passengers = passengers),
    by = 'airport'
  ) |>
  left_join(
    can |>
      filter(year == 2019) |>
      transmute(airport, canadian_model_2019_passengers = passengers),
    by = 'airport'
  ) |>
  mutate(
    # Rebase forecast changes to the Canadian model's actual 2019 volumes.
    # This prevents differing passenger definitions from affecting the level.
    passengers_no_covid_ets = canadian_model_2019_passengers *
      statcan_forecast_passengers / statcan_2019_passengers
  ) |>
  arrange(airport, year)

if (nrow(passenger_forecasts) != length(can_airports) * 6L ||
    anyNA(passenger_forecasts) ||
    any(passenger_forecasts$passengers_no_covid_ets <= 0)) {
  stop('Invalid ETS passenger forecasts.')
}

# Apply the SAME scenario-minus-actual per-passenger differences to the
# projected passenger volumes. This isolates the volume effect; it does not
# treat ordinary revenue from additional passengers as a pricing benefit.
ets_annual_airport <- annual_airport |>
  left_join(
    passenger_forecasts |>
      select(airport, year, passengers_no_covid_ets),
    by = c('airport', 'year')
  ) |>
  mutate(
    passengers_no_covid_ets = coalesce(passengers_no_covid_ets, passengers),
    additional_revenue_no_covid_ets_m =
      difference_pp_cad_2025 * passengers_no_covid_ets / 1e6
  ) |>
  select(
    scenario, airport, year, passengers_actual = passengers,
    passengers_no_covid_ets, difference_pp_cad_2025,
    additional_revenue_actual_traffic_m = additional_revenue_cad_2025_m,
    additional_revenue_no_covid_ets_m
  )

# Include BIG4 totals, as in the headline annual and summary exports.
ets_annual_big4 <- ets_annual_airport |>
  group_by(scenario, year) |>
  summarise(
    passengers_actual = sum(passengers_actual),
    passengers_no_covid_ets = sum(passengers_no_covid_ets),
    additional_revenue_actual_traffic_m = sum(additional_revenue_actual_traffic_m),
    additional_revenue_no_covid_ets_m = sum(additional_revenue_no_covid_ets_m),
    .groups = 'drop'
  ) |>
  mutate(
    airport = 'BIG4',
    difference_pp_cad_2025 = NA_real_
  )

ets_annual <- bind_rows(ets_annual_airport, ets_annual_big4) |>
  arrange(match(scenario, scenario_info$scenario), airport, year)

ets_summary <- ets_annual |>
  filter(year > start_year) |>
  group_by(scenario, airport) |>
  summarise(
    additional_revenue_actual_traffic_2016_2025_m =
      sum(additional_revenue_actual_traffic_m),
    additional_revenue_no_covid_ets_2016_2025_m =
      sum(additional_revenue_no_covid_ets_m),
    additional_revenue_no_covid_ets_2025_m =
      additional_revenue_no_covid_ets_m[year == end_year],
    .groups = 'drop'
  ) |>
  mutate(
    ets_minus_actual_traffic_m =
      additional_revenue_no_covid_ets_2016_2025_m -
      additional_revenue_actual_traffic_2016_2025_m
  ) |>
  arrange(match(scenario, scenario_info$scenario), airport)

# Two-year diagnostic backtest: fit 2008-2017, forecast 2018-2019.
# The shorter earlier sample is insufficient for a damped ETS fit.
ets_backtest <- traffic_hist |>
  group_by(airport) |>
  group_modify(\(d, key) {
    train <- d |> filter(year <= 2017) |> arrange(year)
    test <- d |> filter(year %in% 2018:2019) |> arrange(year)
    fit <- forecast::ets(
      ts(log(train$passengers), start = 2008, frequency = 1),
      model = 'AAN', damped = TRUE
    )
    tibble(
      year = test$year,
      actual = test$passengers,
      predicted = exp(as.numeric(forecast::forecast(fit, h = 2)$mean))
    )
  }) |>
  ungroup()

ets_backtest_big4 <- ets_backtest |>
  group_by(year) |>
  summarise(actual = sum(actual), predicted = sum(predicted), .groups = 'drop') |>
  mutate(airport = 'BIG4')

ets_backtest <- bind_rows(ets_backtest, ets_backtest_big4) |>
  mutate(
    error_pct = 100 * (predicted / actual - 1),
    abs_error_pct = abs(error_pct)
  ) |>
  arrange(airport, year)

ets_backtest_accuracy <- ets_backtest |>
  group_by(airport) |>
  summarise(
    mape = mean(abs_error_pct),
    bias_pct = mean(error_pct),
    error_2019_pct = error_pct[year == 2019],
    .groups = 'drop'
  )

# SEPARATE ETS OUTPUTS -----------------------------------------------------
readr::write_csv(passenger_forecasts,
                 file.path(ets_out_dir, 'passenger_forecasts.csv'))
readr::write_csv(ets_annual,
                 file.path(ets_out_dir, 'counterfactual_annual.csv'))
readr::write_csv(ets_summary,
                 file.path(ets_out_dir, 'counterfactual_summary.csv'))
readr::write_csv(ets_backtest,
                 file.path(ets_out_dir, 'backtest_annual.csv'))
readr::write_csv(ets_backtest_accuracy,
                 file.path(ets_out_dir, 'backtest_accuracy.csv'))

# Separate workbook: no ETS fields or forecasts enter the headline workbook.
writexl::write_xlsx(
  list(
    Summary = ets_summary,
    Annual = ets_annual,
    Passenger_forecasts = passenger_forecasts,
    Backtest_accuracy = ets_backtest_accuracy,
    Backtest_annual = ets_backtest
  ),
  file.path(ets_out_dir, 'ets_sensitivity.xlsx')
)

print(ets_summary |>
        filter(airport == 'BIG4') |>
        select(scenario, additional_revenue_actual_traffic_2016_2025_m,
               additional_revenue_no_covid_ets_2016_2025_m,
               ets_minus_actual_traffic_m), n = Inf)
message('Saved ETS sensitivity CSVs and separate Excel workbook to: ', ets_out_dir)

# Keep a compact passenger trajectory plot for inspecting the sensitivity.
ets_plot <- bind_rows(
  can |>
    select(airport, year, passengers) |>
    mutate(series = 'Actual'),
  passenger_forecasts |>
    transmute(airport, year, passengers = passengers_no_covid_ets,
              series = 'No-pandemic ETS')
) |>
  ggplot(aes(year, passengers / 1e6, colour = series)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ airport, scales = 'free_y') +
  labs(title = 'Canadian airport passengers: actual vs no-pandemic ETS',
       x = NULL, y = 'Passengers (millions)', colour = NULL) +
  theme_minimal()
print(ets_plot)
