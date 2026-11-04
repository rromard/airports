# Airport revenue counterfactual | exploratory plots ----------------------
# Run after airport_revenue_counterfactual.R.
# Reads headline CSVs and pooled EBITDA from the main counterfactual.
# Saves charts to output/airport_counterfactual/plots/.

library(tidyverse)
library(here)

in_dir <- here('output', 'airport_counterfactual')
plot_dir <- file.path(in_dir, 'plots')
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

actual <- read_csv(file.path(in_dir, 'actual_growth_canada_australia.csv'), show_col_types = FALSE)
annual <- read_csv(file.path(in_dir, 'counterfactual_annual.csv'), show_col_types = FALSE)
summary <- read_csv(file.path(in_dir, 'counterfactual_summary.csv'), show_col_types = FALSE)
paths <- read_csv(file.path(in_dir, 'scenario_growth_paths.csv'), show_col_types = FALSE)
ebitda <- readxl::read_excel(file.path(in_dir, 'airport_counterfactual.xlsx'),
                            sheet = 'EBITDA_pooled')

scenario_order <- c('GRADUAL', 'CAP_THEN_SHOCK', 'IMMEDIATE_SHOCK')
scenario_names <- c(
  GRADUAL = 'Australian growth',
  CAP_THEN_SHOCK = 'Cap, then shock',
  IMMEDIATE_SHOCK = 'Immediate shock'
)
airport_order <- c('BIG4', 'YYC', 'YUL', 'YVR', 'YYZ')

annual <- annual |>
  mutate(scenario = factor(scenario, levels = scenario_order, labels = scenario_names),
         airport = factor(airport, levels = airport_order))
summary <- summary |>
  mutate(scenario = factor(scenario, levels = scenario_order, labels = scenario_names),
         airport = factor(airport, levels = airport_order))
paths <- paths |>
  mutate(scenario = factor(scenario, levels = scenario_order, labels = scenario_names))
ebitda <- ebitda |>
  mutate(scenario = factor(scenario, levels = scenario_order, labels = scenario_names),
         airport = factor(airport, levels = airport_order))

# Minimal exploratory style: units and time coverage appear in subtitles.
base_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = 'plot',
        legend.position = 'bottom',
        axis.title = element_blank(),
        strip.text = element_text(face = 'bold'))

save_plot <- function(plot, filename, width = 11, height = 7) {
  ggsave(file.path(plot_dir, filename), plot = plot,
         width = width, height = height, dpi = 250, bg = 'white')
  print(plot)
}

# 1. OBSERVED GROWTH: real aeronautical (+ AIF for Canada) revenue/pax
# Each airport indexed independently; AUS4/BIG4 indices are unweighted means.
p1 <- actual |>
  mutate(airport = factor(airport,
                          levels = c('BIG4', 'YYC', 'YUL', 'YVR', 'YYZ',
                                     'AUS4', 'BNE', 'MEL', 'PER', 'SYD'))) |>
  ggplot(aes(year, index_2015, group = airport)) +
  geom_hline(yintercept = 100, linetype = 'dashed', colour = 'grey65') +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ country + airport, ncol = 5) +
  scale_x_continuous(breaks = c(2015, 2020, 2025)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  labs(title = 'Actual revenue per passenger growth',
       subtitle = '2015 = 100') +
  base_theme
save_plot(p1, '01_actual_growth_by_airport.png', width = 13, height = 6.7)

# 2–3. SUMMARY: additional counterfactual revenue by airport and scenario
# Free y scales keep the individual airports legible alongside BIG4.
plot_summary <- function(metric, title, filename) {
  p <- ggplot(summary, aes(scenario, .data[[metric]], fill = scenario)) +
    geom_hline(yintercept = 0, colour = 'grey60', linewidth = 0.3) +
    geom_col(width = 0.7) +
    facet_wrap(~ airport, scales = 'free_y', ncol = 3) +
    scale_y_continuous(labels = scales::label_number(big.mark = ',', accuracy = 1)) +
    labs(title = title, subtitle = 'Difference from actual · millions of 2025 CAD',
         fill = NULL) +
    base_theme +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  save_plot(p, filename, width = 11, height = 7.2)
}
plot_summary('additional_revenue_2025_m',
             'Revenue difference in 2025',
             '02_additional_revenue_2025.png')
plot_summary('additional_revenue_2016_2025_m',
             'Cumulative revenue difference, 2016–2025',
             '03_additional_revenue_cumulative.png')

# 4–5. ANNUAL AND CUMULATIVE additional revenue through time.
annual_trends <- annual |>
  arrange(scenario, airport, year) |>
  group_by(scenario, airport) |>
  mutate(cumulative_additional_m = cumsum(if_else(
    year >= 2016L, additional_revenue_cad_2025_m, 0
  ))) |>
  ungroup()

plot_trend <- function(metric, title, filename) {
  p <- ggplot(annual_trends,
              aes(year, .data[[metric]], colour = scenario, group = scenario)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = 'grey60') +
    geom_line(linewidth = 1) +
    facet_wrap(~ airport, scales = 'free_y', ncol = 3) +
    scale_x_continuous(breaks = c(2015, 2020, 2025)) +
    scale_y_continuous(labels = scales::label_number(big.mark = ',', accuracy = 1)) +
    labs(title = title, subtitle = 'Difference from actual · millions of 2025 CAD',
         colour = NULL) +
    base_theme
  save_plot(p, filename, width = 11, height = 7.2)
}
plot_trend('additional_revenue_cad_2025_m',
           'Annual revenue difference',
           '04_annual_additional_revenue_trend.png')
plot_trend('cumulative_additional_m',
           'Cumulative revenue difference',
           '05_cumulative_additional_revenue_trend.png')

# 6. Australian scenario paths: show observed versus extrapolated portions.
# Classify each connecting segment by its END year, so the extrapolation
# begins at the last observed point rather than leaving a visual gap.
path_segments <- paths |>
  arrange(scenario, year) |>
  group_by(scenario) |>
  mutate(
    year_start = lag(year),
    index_start = lag(100 * growth_multiple_2015),
    index_end = 100 * growth_multiple_2015,
    status = if_else(source_status == 'observed', 'Observed', 'Extrapolated')
  ) |>
  ungroup() |>
  filter(!is.na(year_start))

p6 <- ggplot(path_segments,
             aes(x = year_start, xend = year, y = index_start, yend = index_end,
                 colour = scenario, linetype = status)) +
  geom_segment(linewidth = 1) +
  geom_point() +
  scale_linetype_manual(values = c(Observed = 'solid', Extrapolated = 'dashed')) +
  scale_x_continuous(breaks = c(2015, 2020, 2025)) +
  labs(title = 'Australian growth scenarios', subtitle = '2015 = 100',
       colour = NULL, linetype = NULL) +
  base_theme
save_plot(p6, '06_counterfactual_growth_paths.png', width = 11, height = 6.5)

# 7. Put the counterfactual paths beside ACTUAL Canadian per-passenger growth
# for each airport (including BIG4). Counterfactual indices use the same 2015
# airport-specific baseline as the model; BIG4 uses passenger-weighted 2015
# baseline. BIG4 actual is recalculated as passenger-weighted real
# revenue/pax relative to 2015, matching its counterfactual index definition.
actual_can <- actual |>
  filter(country == 'Canada') |>
  group_by(airport) |>
  mutate(index_consistent = if_else(
    airport == 'BIG4',
    100 * actual_pp_cad_2025 / actual_pp_cad_2025[year == 2015],
    index_2015
  )) |>
  ungroup() |>
  transmute(airport, year, series = 'Actual', index = index_consistent)

scenario_can <- annual |>
  transmute(airport = as.character(airport), year,
            series = as.character(scenario),
            index = 100 * scenario_pp_cad_2025 / baseline_pp_cad_2025)

p7_data <- bind_rows(actual_can, scenario_can) |>
  mutate(airport = factor(airport, levels = airport_order),
         series = factor(series, levels = c('Actual', unname(scenario_names))))

p7 <- ggplot(p7_data, aes(year, index, colour = series)) +
  geom_hline(yintercept = 100, linetype = 'dashed', colour = 'grey65') +
  geom_line(linewidth = 0.95) +
  geom_point() +
  facet_wrap(~ airport, ncol = 3) +
  scale_x_continuous(breaks = c(2015, 2020, 2025)) +
  labs(title = 'Canadian revenue growth: actual vs. scenarios',
       subtitle = 'Aeronautical + AIF per passenger · 2015 = 100',
       colour = NULL) +
  base_theme
save_plot(p7, '07_actual_vs_counterfactual_growth_by_airport.png',
          width = 11, height = 7.2)


# 8. DIRECT CANADA–AUSTRALIA GROWTH COMPARISON.
# Every Canadian airport is compared with the SAME AUS4 average, making
# the international growth gap easier to read than the ten-facet chart.
# Both BIG4 and AUS4 are the unweighted averages of four airport indices.
aus_benchmark <- actual |>
  filter(country == 'Australia', airport == 'AUS4') |>
  transmute(year, australian_index = index_2015)

p8_data <- actual |>
  filter(country == 'Canada', airport %in% airport_order) |>
  select(airport, year, canadian_index = index_2015) |>
  left_join(aus_benchmark, by = 'year') |>
  pivot_longer(c(canadian_index, australian_index),
               names_to = 'series', values_to = 'index') |>
  mutate(
    airport = factor(airport, levels = airport_order),
    series = factor(series,
                    levels = c('canadian_index', 'australian_index'),
                    labels = c('Canadian airport', 'Australian four-airport average'))
  )

p8 <- ggplot(p8_data, aes(year, index, colour = series, linetype = series)) +
  geom_hline(yintercept = 100, linetype = 'dashed', colour = 'grey75') +
  geom_line(linewidth = 1.05) +
  facet_wrap(~ airport, ncol = 3) +
  scale_x_continuous(breaks = c(2015, 2020, 2025)) +
  labs(title = 'Canada vs. Australia: revenue growth',
       subtitle = 'Per passenger · 2015 = 100',
       colour = NULL, linetype = NULL) +
  base_theme
save_plot(p8, '08_canada_vs_australian_average_growth.png', width = 11, height = 7.2)

# 9. OBSERVED PER-PASSENGER REVENUE LEVELS IN BOTH COUNTRIES.
# Uses the 2025-CAD conversions in the model input; levels are not
# definitions-of-fees matched across countries.
p9 <- actual |>
  mutate(airport = factor(airport,
    levels = c('BIG4', 'YYC', 'YUL', 'YVR', 'YYZ',
               'AUS4', 'BNE', 'MEL', 'PER', 'SYD'))) |>
  ggplot(aes(year, actual_pp_cad_2025, group = airport)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ country + airport, ncol = 5, scales = 'free_y') +
  scale_x_continuous(breaks = c(2015, 2020, 2025)) +
  scale_y_continuous(labels = scales::label_dollar(accuracy = 1)) +
  labs(title = 'Actual revenue per passenger',
       subtitle = '2025 CAD per passenger') +
  base_theme
save_plot(p9, '09_actual_per_passenger_levels.png', width = 13, height = 6.7)

# 10. 2025 ACTUAL VS COUNTERFACTUAL PER-PASSENGER LEVELS.
# Actual is plotted once per airport rather than repeated across scenarios.
pp_2025 <- bind_rows(
  annual |>
    filter(year == 2025, scenario == levels(scenario)[1]) |>
    transmute(airport, series = 'Actual', pp = actual_pp_cad_2025),
  annual |>
    filter(year == 2025) |>
    transmute(airport, series = as.character(scenario),
              pp = scenario_pp_cad_2025)
) |>
  mutate(series = factor(series, levels = c('Actual', unname(scenario_names))))

p10 <- ggplot(pp_2025, aes(series, pp, fill = series)) +
  geom_col(width = 0.72) +
  facet_wrap(~ airport, scales = 'free_y', ncol = 3) +
  scale_y_continuous(labels = scales::label_dollar(accuracy = 1)) +
  labs(title = 'Revenue per passenger in 2025',
       subtitle = 'Actual and scenarios · 2025 CAD per passenger', fill = NULL) +
  base_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
save_plot(p10, '10_per_passenger_2025_actual_vs_scenarios.png', width = 11, height = 7.2)

# 11. 2025 ADDITIONAL REVENUE PER PASSENGER, rather than total millions.
p11 <- annual |>
  filter(year == 2025) |>
  ggplot(aes(scenario, difference_pp_cad_2025, fill = scenario)) +
  geom_hline(yintercept = 0, colour = 'grey65', linewidth = 0.35) +
  geom_col(width = 0.72) +
  facet_wrap(~ airport, scales = 'free_y', ncol = 3) +
  scale_y_continuous(labels = scales::label_dollar(accuracy = 1)) +
  labs(title = 'Revenue difference per passenger in 2025',
       subtitle = '2025 CAD per passenger', fill = NULL) +
  base_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
save_plot(p11, '11_additional_revenue_per_passenger_2025.png', width = 11, height = 7.2)

# 12. ACTUAL VERSUS COUNTERFACTUAL PER-PASSENGER LEVELS THROUGH TIME.
# Unlike chart 7, this shows 2025-CAD values, not indices.
pp_paths <- bind_rows(
  annual |>
    filter(scenario == levels(scenario)[1]) |>
    transmute(airport, year, series = 'Actual', pp = actual_pp_cad_2025),
  annual |>
    transmute(airport, year, series = as.character(scenario),
              pp = scenario_pp_cad_2025)
) |>
  mutate(series = factor(series, levels = c('Actual', unname(scenario_names))))

p12 <- ggplot(pp_paths, aes(year, pp, colour = series)) +
  geom_line(linewidth = 0.95) +
  facet_wrap(~ airport, scales = 'free_y', ncol = 3) +
  scale_x_continuous(breaks = c(2015, 2020, 2025)) +
  scale_y_continuous(labels = scales::label_dollar(accuracy = 1)) +
  labs(title = 'Revenue per passenger: actual vs. scenarios',
       subtitle = '2025 CAD per passenger', colour = NULL) +
  base_theme
save_plot(p12, '12_per_passenger_actual_vs_scenarios_trend.png', width = 11, height = 7.2)

# 13. YEAR-BY-YEAR PER-PASSENGER GAP: counterfactual less actual.
# Per-passenger differences are not cumulative; summing them across years
# would not yield a meaningful cumulative dollar-per-passenger measure.
p13 <- annual |>
  ggplot(aes(year, difference_pp_cad_2025, colour = scenario)) +
  geom_hline(yintercept = 0, colour = 'grey65', linewidth = 0.35) +
  geom_line(linewidth = 1) +
  facet_wrap(~ airport, scales = 'free_y', ncol = 3) +
  scale_x_continuous(breaks = c(2015, 2020, 2025)) +
  scale_y_continuous(labels = scales::label_dollar(accuracy = 1)) +
  labs(title = 'Annual revenue difference per passenger',
       subtitle = '2025 CAD per passenger', colour = NULL) +
  base_theme
save_plot(p13, '13_additional_revenue_per_passenger_trend.png', width = 11, height = 7.2)

# 14–15. POOLED EBITDA MARGINS, 2023–2025 ---------------------------------
# Read the pooled figures directly from the main workbook. These margins
# are based on summed revenues and EBITDA, not averages of annual margins.
ebitda_levels <- bind_rows(
  ebitda |>
    filter(scenario == levels(scenario)[1]) |>
    transmute(airport, series = 'Actual', margin = actual_ebitda_margin_pct),
  ebitda |>
    transmute(airport, series = as.character(scenario),
              margin = scenario_ebitda_margin_pct)
) |>
  mutate(series = factor(series, levels = c('Actual', unname(scenario_names))))

p14 <- ggplot(ebitda_levels, aes(series, margin, fill = series)) +
  geom_col(width = 0.65) +
  facet_wrap(~ airport, ncol = 3) +
  scale_y_continuous(labels = scales::label_number(suffix = '%', accuracy = 1)) +
  labs(title = 'EBITDA margins',
       subtitle = '2023–2025 pooled · % of total revenue', colour = NULL) +
  base_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
save_plot(p14, '14_pooled_ebitda_margins.png', width = 11, height = 7.2)

p15 <- ggplot(ebitda,
              aes(scenario, ebitda_margin_change_pp, fill = scenario)) +
  geom_hline(yintercept = 0, colour = 'grey65', linewidth = 0.35) +
  geom_col(width = 0.72) +
  facet_wrap(~ airport, ncol = 3) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(title = 'Change in EBITDA margin',
       subtitle = '2023–2025 pooled · percentage points', fill = NULL) +
  base_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
save_plot(p15, '15_pooled_ebitda_margin_change.png', width = 11, height = 7.2)

message('Saved 15 PNG charts to: ', plot_dir)
