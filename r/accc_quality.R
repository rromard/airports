# ACCC airport quality-of-service: exploratory parsing and diagnostics
# Source: ACCC Airport Monitoring Report 2024-25 supplementary database
# BNE, MEL, PER, SYD; FY2007-08 through FY2024-25
#
# These are SELECTED series in the supplementary workbook, not all components
# of the ACCC's published overall quality ratings. Do not reconstruct that
# headline score by averaging the source columns.
#
# Packages: tidyverse, readxl, here

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
})

# EDIT INPUT PATH IF NEEDED -------------------------------------------------
input <- here('data', 'australia',
              'accc-airport-monitoring-report-2024-25-supplementary-database.xlsx')
if (!file.exists(input)) {
  input <- here('accc-airport-monitoring-report-2024-25-supplementary-database.xlsx')
}
if (!file.exists(input)) stop('Cannot find ACCC supplementary workbook: ', input)

out_dir <- here('output', 'accc_quality')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
airports <- c('BNE', 'MEL', 'PER', 'SYD')

# PARSER --------------------------------------------------------------------
# The four "total and aero" worksheets have 18 FY columns B:S.
# Their quality sections each have three blocks: objective measures,
# surveys of airlines, and surveys of passengers. We retain the original
# row number for later auditing and preserve explicit missing years.

parse_quality_sheet <- function(airport) {
  sheet <- paste(airport, 'total and aero')
  x <- read_excel(input, sheet = sheet, col_names = FALSE, col_types = 'text')

  header <- as.character(unlist(x[1, ], use.names = FALSE))
  fy_cols <- which(str_detect(header, '^20[0-9]{2}[–-][0-9]{2}$'))
  if (length(fy_cols) != 18L) {
    stop('Expected 18 financial-year columns in ', sheet,
         '; found ', length(fy_cols))
  }
  year_end <- 2000L + as.integer(str_sub(header[fy_cols], -2L))
  if (!identical(year_end, 2008:2025)) stop('Unexpected FY headers in ', sheet)

  labels <- str_squish(as.character(x[[1]]))
  find_one <- function(text) {
    pos <- which(str_to_lower(labels) == str_to_lower(text))
    if (length(pos) != 1L) stop('Expected exactly one "', text, '" in ', sheet)
    pos
  }
  start <- find_one('Total airport and aeronautical quality data')
  airline_start <- find_one('Surveys of airlines')
  passenger_start <- find_one('Surveys of passengers')
  stopifnot(start < airline_start, airline_start < passenger_start)

  values <- x[, fy_cols, drop = FALSE]
  names(values) <- paste0('year_', year_end)
  to_number <- function(z) suppressWarnings(as.numeric(str_replace_all(z, ',', '')))
  has_value <- rowSums(!is.na(as.matrix(map_dfc(values, to_number)))) > 0

  meta <- tibble(
    source_row = seq_len(nrow(x)),
    label = labels,
    has_value = has_value
  ) |>
    mutate(
      section = case_when(
        source_row > start & source_row < airline_start ~ 'objective',
        source_row > airline_start & source_row < passenger_start ~ 'airline',
        source_row > passenger_start ~ 'passenger',
        TRUE ~ 'outside'
      ),
      is_terminal = section != 'airline' &
        str_detect(coalesce(label, ''),
                   regex(paste0('^', airport, '[ -].*terminal'), ignore_case = TRUE)),
      terminal = if_else(is_terminal, label, NA_character_),
      facility = if_else(
        section == 'airline' & label %in% c(
          'Runway', 'Taxiways', 'Aprons', 'Aircraft parking',
          'Ground handling', 'Management responsiveness'),
        label, NA_character_
      ),
      domain = if_else(section %in% c('objective', 'passenger') &
                         !has_value & !is_terminal & !is.na(label),
                       label, NA_character_)
    ) |>
    filter(section != 'outside') |>
    group_by(section) |>
    fill(terminal, facility, domain, .direction = 'down') |>
    ungroup()

  bind_cols(meta, values[meta$source_row, , drop = FALSE]) |>
    pivot_longer(starts_with('year_'), names_to = 'year_col',
                 values_to = 'raw_value') |>
    mutate(
      airport = airport,
      source_sheet = sheet,
      year_end = as.integer(str_remove(year_col, '^year_')),
      fyear = paste0(year_end - 1L, '-', str_sub(year_end, -2L)),
      value = to_number(raw_value),
      metric = if_else(section == 'airline',
                       paste(facility, label, sep = ' / '), label),
      # Distinguishes repeated questions across different terminals.
      item_id = paste(airport, section, coalesce(terminal, ''),
                      coalesce(facility, ''), label, sep = '::'),
      # ACCC did not conduct its standard quality monitoring FY2019-20
      # through FY2021-22. Retain raw workbook entries but FLAG the break.
      monitoring_gap = year_end %in% 2020:2022
    ) |>
    group_by(source_row) |>
    filter(any(!is.na(value))) |>
    ungroup() |>
    filter(!is.na(label)) |>
    select(airport, year_end, fyear, section, terminal, domain, facility,
           metric, item_id, value, monitoring_gap, source_sheet, source_row)
}

quality <- map_dfr(airports, parse_quality_sheet) |>
  arrange(airport, section, item_id, year_end)

if (anyDuplicated(quality[c('airport', 'section', 'item_id', 'year_end')])) {
  stop('Duplicate item-year keys: inspect terminal/facility headings.')
}

bad_ratings <- quality |>
  filter(section %in% c('passenger', 'airline'),
         !is.na(value), !between(value, 1, 5))
if (nrow(bad_ratings)) {
  print(bad_ratings, n = 20)
  stop('Survey ratings outside the expected 1-to-5 range.')
}

write_csv(quality, file.path(out_dir, 'quality_long.csv'))

# AUDIT: coverage and the interrupted monitoring period ---------------------
coverage <- quality |>
  group_by(airport, section, year_end, fyear, monitoring_gap) |>
  summarise(n_series = n_distinct(item_id),
            n_reported = sum(!is.na(value)),
            .groups = 'drop')
write_csv(coverage, file.path(out_dir, 'quality_coverage.csv'))
print(coverage |> filter(year_end %in% c(2016, 2019, 2020:2023, 2025)), n = Inf)

# MATCHED-ITEM change: FY2015-16 -> FY2024-25 -------------------------------
# Pair each exact terminal/indicator with itself. No changing-composition bias
# between the two endpoints; each included series gets equal weight.
paired <- quality |>
  filter(section %in% c('passenger', 'airline'),
         year_end %in% c(2016L, 2025L)) |>
  select(airport, section, terminal, domain, facility, metric,
         item_id, year_end, value) |>
  pivot_wider(names_from = year_end, values_from = value,
              names_prefix = 'rating_') |>
  filter(!is.na(rating_2016), !is.na(rating_2025)) |>
  mutate(change = rating_2025 - rating_2016)

write_csv(paired, file.path(out_dir, 'quality_paired_items_2016_2025.csv'))

paired_summary <- paired |>
  group_by(airport, section) |>
  summarise(n_matched_items = n(),
            mean_2016 = mean(rating_2016),
            mean_2025 = mean(rating_2025),
            mean_change = mean(change),
            share_declining = mean(change < 0),
            .groups = 'drop')
write_csv(paired_summary,
          file.path(out_dir, 'quality_matched_summary_2016_2025.csv'))
print(paired_summary, n = Inf)

# FIGURE 1: survey trends, limited to the same endpoint-matched questions --
matched_keys <- paired |> distinct(airport, section, item_id)
series <- quality |>
  inner_join(matched_keys, by = c('airport', 'section', 'item_id')) |>
  filter(!monitoring_gap, !is.na(value)) |>
  group_by(airport, section, year_end) |>
  summarise(mean_rating = mean(value), n_items = n(), .groups = 'drop')

p1 <- ggplot(series, aes(year_end, mean_rating, colour = section)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  facet_wrap(~airport, ncol = 2) +
  scale_x_continuous(breaks = c(2008, 2012, 2016, 2019, 2023, 2025)) +
  coord_cartesian(ylim = c(2.5, 5)) +
  labs(
    title = 'Quality ratings at four Australian airports',
    subtitle = 'Unweighted averages of selected endpoint-matched survey indicators; not ACCC overall ratings',
    x = 'Financial year ending', y = 'Mean rating (1–5)', colour = NULL,
    caption = 'FY2019-20 to FY2021-22 omitted because regular ACCC quality monitoring was suspended.'
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = 'bottom')

ggsave(file.path(out_dir, 'survey_ratings_2008_2025.png'), p1,
       width = 11, height = 7, dpi = 180)

# FIGURE 2: identify which particular passenger indicators changed ----------
p2 <- paired |>
  filter(section == 'passenger') |>
  ggplot(aes(change)) +
  geom_histogram(binwidth = 0.2, boundary = 0, colour = 'white') +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  facet_wrap(~airport, ncol = 2) +
  labs(title = 'Distribution of changes in passenger survey items',
       subtitle = 'FY2024-25 minus FY2015-16; matched question and terminal',
       x = 'Change in 1–5 rating (points)', y = 'Number of indicators') +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, 'passenger_indicator_changes.png'), p2,
       width = 10, height = 7, dpi = 180)

# Optional next-pass raw indicators of capacity / crowding pressure ---------
# These are NOT survey scores. The sign of a change is not automatically
# interpretable as 'better' or 'worse'; passenger volumes also matter.
pressure <- quality |>
  filter(section == 'objective',
         str_detect(str_to_lower(metric),
                    'passengers per (seat|security clearance system|check-in desk|square metre of lounge area)'))
write_csv(pressure, file.path(out_dir, 'capacity_pressure_selected.csv'))

message('ACCC quality EDA files written to: ', out_dir)

# OPTIONAL: join the quality trend to the existing V8 Australia revenue series.
# This is DESCRIPTIVE only: all these airports were already privatized by the
# start of the service-quality panel, and no causality is identified here.
financial_path <- here('output', 'airport_comparison',
                       'aero_revenue_growth_2015_2025.csv')
if (file.exists(financial_path)) {
  financial <- read_csv(financial_path, show_col_types = FALSE) |>
    filter(country == 'Australia', variable == 'aero_revenue') |>
    transmute(airport, year_end = as.integer(year),
              aero_revenue_per_passenger_cad_2025 = per_passenger_cad_2025)

  financial_quality <- series |>
    filter(section == 'passenger', year_end >= 2015L) |>
    left_join(financial, by = c('airport', 'year_end'),
              relationship = 'many-to-one') |>
    arrange(airport, year_end)
  write_csv(financial_quality,
            file.path(out_dir, 'quality_vs_aero_revenue_per_passenger.csv'))
}

