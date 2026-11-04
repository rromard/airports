# No-pandemic passenger demand | ETS sensitivity for airport counterfactual
# -------------------------------------------------------------------------
# Run AFTER airport_revenue_counterfactual.R. This is a passenger-volume
# sensitivity, NOT a new pricing scenario: use unchanged per-passenger pricing
# differences from the main counterfactual, replacing 2020-25 passenger
# volumes with ETS projections fitted to 2008-19 StatCan airport traffic.
# Forecast levels are rebased to 2019 passengers in the main analysis.
# Outputs: one workbook (summary, annual, forecasts, backtest) in
#   output/airport_counterfactual/ets_sensitivity/
# Only the workbook is exported; the constituent tables remain R objects.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

start_year <- 2015L
end_year <- 2025L
can_airports <- c('YYC', 'YUL', 'YVR', 'YYZ')
scenario_order <- c('GRADUAL', 'CAP_THEN_SHOCK', 'IMMEDIATE_SHOCK')
out_dir <- here('output', 'airport_counterfactual')
ets_out_dir <- file.path(out_dir, 'ets_sensitivity')
annual_path <- file.path(out_dir, 'counterfactual_annual.csv')
# Require the main scenario output before running this independent sensitivity.
if (!file.exists(annual_path)) stop('Run the main counterfactual first: ', annual_path)
dir.create(ets_out_dir, recursive = TRUE, showWarnings = FALSE)

# Read pricing differences and observed passenger volumes from the headline
# annual results; one scenario is sufficient to recover actual traffic.
annual <- readr::read_csv(annual_path, show_col_types = FALSE)
required <- c('scenario', 'airport', 'year', 'passengers',
              'difference_pp_cad_2025', 'additional_revenue_cad_2025_m')
# Verify the annual file supplies traffic, pricing differences and
# revenue differences needed to isolate the passenger-volume effect.
if (!all(required %in% names(annual))) {
  stop('Missing columns in main counterfactual: ',
       paste(setdiff(required, names(annual)), collapse = ', '))
}
annual_airport <- annual |>
  filter(airport %in% can_airports, scenario %in% scenario_order)
# Require a complete, unique 2015–2025 panel for every scenario and
# Canadian airport; reconcile extra revenue to price difference × traffic.
if (nrow(annual_airport) != length(scenario_order) * length(can_airports) *
      (end_year - start_year + 1L) ||
    anyDuplicated(annual_airport[c('scenario', 'airport', 'year')]) ||
    anyNA(annual_airport[required]) ||
    any(annual_airport$passengers <= 0) ||
    !setequal(annual_airport$year, start_year:end_year) ||
    any(abs(annual_airport$additional_revenue_cad_2025_m -
              annual_airport$difference_pp_cad_2025 *
              annual_airport$passengers / 1e6) > 1e-4)) {
  stop('Main counterfactual airport-year panel is incomplete or inconsistent.')
}
can <- annual_airport |>
  filter(scenario == 'GRADUAL') |>
  select(airport, year, passengers) |>
  arrange(airport, year)

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

# Check the StatCan training sample has twelve positive annual
# passenger observations (2008–2019) for each of the four airports.
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

# Require six positive, nonmissing 2020–2025 passenger forecasts
# per airport after rebasing to the main model's 2019 traffic.
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
  arrange(match(scenario, scenario_order), airport, year)

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
  arrange(match(scenario, scenario_order), airport)

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

# ETS OUTPUT ---------------------------------------------------------------
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
message('Saved ETS sensitivity workbook to: ', ets_out_dir)

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
