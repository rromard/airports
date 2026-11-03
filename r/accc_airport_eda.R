# ACCC airport monitoring database: quick proposal-oriented EDA
#
# Project layout: r/ (scripts), data/australia/ (source), output/ (results).
# In an interactive project session:
#   source(here::here("r", "accc_airport_eda.R"))
#   eda <- run_accc_eda()
#
# Or from the project root: Rscript r/accc_airport_eda.R
# Optional shell arguments override workbook, parser, output directory (in order).
# Installs required: tidyverse and tidyxl (used by the existing parser).
#
# This is a descriptive screen, not an estimate of privatization's effects.
# Prices, margins, investment and survey scores have different denominators.
# The workbook has no flight punctuality/cancellation series or separate AIF.

library(tidyverse)
library(here)

accc_metric_dictionary <- function() {
  tribble(
    ~metric, ~label, ~unit,
    "passengers_m", "Passengers", "millions",
    "aero_revenue_per_passenger", "Aeronautical revenue per passenger", "A$ per passenger",
    "aero_revenue_m", "Aeronautical revenue", "A$ millions",
    "total_revenue_m", "Total airport revenue", "A$ millions",
    "aero_margin", "Aeronautical operating margin", "percent",
    "total_margin", "Total airport operating margin", "percent",
    "aero_additions_m", "Aeronautical asset additions", "A$ millions",
    "total_additions_m", "Total airport asset additions", "A$ millions",
    "parking_revenue_m", "Car parking revenue", "A$ millions",
    "parking_revenue_per_vehicle", "Car parking revenue per vehicle", "A$ per vehicle",
    "parking_margin", "Car parking operating margin", "percent"
  )
}

# Match the original workbook's labels. Sydney has parallel landfill-inclusive
# and landfill-exclusive series: select the latter once, to prevent double count.
prepare_accc_financial <- function(accc) {
  airport <- accc$airport_financial |>
    filter(!is.na(value)) |>
    mutate(
      clean_measure = measure |>
        str_to_lower() |>
        str_squish() |>
        str_remove("\\*$"),
      metric = case_when(
        clean_measure == "passengers ('000)" ~ "passengers_m",
        str_detect(clean_measure, "^aeronautical revenue per passenger \\(\\$\\)") ~ "aero_revenue_per_passenger",
        str_detect(clean_measure, "^aeronautical revenue \\(\\$m\\)") &
          !str_detect(clean_measure, "including landfill") ~ "aero_revenue_m",
        str_detect(clean_measure, "^total airport revenue \\(\\$m\\)") ~ "total_revenue_m",
        str_detect(clean_measure, "^aeronautical profit margin \\(%\\)") &
          !str_detect(clean_measure, "including landfill") ~ "aero_margin",
        str_detect(clean_measure, "^total airport profit margin \\(%\\)") ~ "total_margin",
        clean_measure == "aeronautical asset additions ($m)" ~ "aero_additions_m",
        clean_measure == "total airport asset additions ($m)" ~ "total_additions_m",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(metric)) |>
    mutate(value = if_else(metric == "passengers_m", value / 1000, value))

  parking <- accc$carpark_financial |>
    filter(section == "car_parking", !is.na(value)) |>
    mutate(
      clean_measure = str_to_lower(str_squish(measure)),
      metric = case_when(
        clean_measure == "car parking revenue ($million)" ~ "parking_revenue_m",
        clean_measure == "revenue per vehicle ($)" ~ "parking_revenue_per_vehicle",
        clean_measure == "car parking profit margin (%)" ~ "parking_margin",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(metric))

  out <- bind_rows(airport, parking) |>
    left_join(accc_metric_dictionary(), by = "metric") |>
    select(airport, year, metric, label, unit, value, measure, sheet, source_row) |>
    arrange(metric, airport, year)

  duplicates <- out |>
    count(airport, year, metric) |>
    filter(n != 1L)
  if (nrow(duplicates)) {
    stop("Metric mapping produced duplicate airport-year-metric records; inspect labels and Sydney landfill choices.")
  }
  out
}

prepare_accc_quality <- function(accc) {
  # Preserve terminals: an unweighted mean would change as terminals enter the
  # data, and the workbook does not supply terminal survey sample sizes.
  accc$airport_quality |>
    filter(
      source == "passenger_survey",
      str_to_lower(str_squish(measure)) == "quality of security search process",
      !is.na(terminal),
      !is.na(value)
    ) |>
    transmute(airport, terminal, year, value, measure, sheet, source_row) |>
    arrange(airport, terminal, year)
}

make_accc_comparisons <- function(financial,
                                  baseline_year = "2018-19",
                                  latest_year = "2024-25") {
  financial |>
    filter(year %in% c(baseline_year, latest_year)) |>
    select(airport, metric, label, unit, year, value) |>
    pivot_wider(names_from = year, values_from = value) |>
    mutate(
      before = .data[[baseline_year]],
      after = .data[[latest_year]],
      change = after - before,
      percent_change = if_else(before != 0, 100 * change / before, NA_real_),
      change_unit = if_else(unit == "percent", "percentage points", unit)
    ) |>
    select(airport, metric, label, unit, before, after, change, change_unit,
           percent_change) |>
    arrange(metric, airport)
}

plot_accc_trends <- function(financial, quality, output_dir) {
  theme_set(theme_minimal(base_size = 11))
  save_plot <- function(plot, filename, height = 7) {
    ggsave(file.path(output_dir, filename), plot,
           width = 10, height = height, units = "in")
  }

  aero <- financial |>
    filter(metric %in% c("aero_revenue_per_passenger", "aero_margin")) |>
    mutate(label = factor(label, levels = c(
      "Aeronautical revenue per passenger", "Aeronautical operating margin"
    )))
  save_plot(
    ggplot(aero, aes(year, value, colour = airport, group = airport)) +
      geom_line(linewidth = 0.7) + geom_point(size = 1.1) +
      facet_wrap(~label, scales = "free_y", ncol = 1) +
      labs(title = "Airport charges and aeronautical operating margins",
           subtitle = "Revenue per passenger (A$) and margin (%); panels have separate scales",
           x = "Financial year", y = NULL, colour = NULL) +
      scale_x_discrete(breaks = function(x) x[seq_along(x) %% 3 == 1]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)),
    "01_aeronautical_charges_margins.pdf"
  )

  investment <- financial |>
    filter(metric %in% c("aero_additions_m", "total_additions_m")) |>
    mutate(label = factor(label, levels = c(
      "Aeronautical asset additions", "Total airport asset additions"
    )))
  save_plot(
    ggplot(investment, aes(year, value, colour = airport, group = airport)) +
      geom_line(linewidth = 0.7) + geom_point(size = 1.1) +
      facet_wrap(~label, scales = "free_y", ncol = 1) +
      labs(title = "Annual additions to airport assets",
           subtitle = "Reported A$ millions; annual additions are volatile, not a capital stock",
           x = "Financial year", y = NULL, colour = NULL) +
      scale_x_discrete(breaks = function(x) x[seq_along(x) %% 3 == 1]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)),
    "02_investment.pdf"
  )

  parking <- financial |>
    filter(metric %in% c("parking_revenue_per_vehicle", "parking_margin")) |>
    mutate(label = factor(label, levels = c(
      "Car parking revenue per vehicle", "Car parking operating margin"
    )))
  save_plot(
    ggplot(parking, aes(year, value, colour = airport, group = airport)) +
      geom_line(linewidth = 0.7) + geom_point(size = 1.1) +
      facet_wrap(~label, scales = "free_y", ncol = 1) +
      labs(title = "Car parking: yield and operating margin",
           subtitle = "Workbook parking money series are in real 2023–24 A$; vehicle mix may change",
           x = "Financial year", y = NULL, colour = NULL) +
      scale_x_discrete(breaks = function(x) x[seq_along(x) %% 3 == 1]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)),
    "03_car_parking.pdf"
  )

  if (nrow(quality)) {
    save_plot(
      ggplot(quality, aes(year, value, colour = terminal, group = terminal)) +
        geom_line(linewidth = 0.7) + geom_point(size = 1) +
        facet_wrap(~airport, ncol = 2) +
        labs(title = "Passenger rating of the security search process",
             subtitle = "Each terminal remains separate; coverage differs by airport and year",
             x = "Financial year", y = "Reported survey score", colour = "Terminal") +
        scale_x_discrete(breaks = function(x) x[seq_along(x) %% 4 == 1]) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)),
      "04_passenger_security_ratings.pdf", height = 8
    )
  }
  invisible(NULL)
}

run_accc_eda <- function(
                         workbook = here::here("data", "australia", "accc-airport-monitoring-report-2024-25-supplementary-database.xlsx"),
                         parser_path = here::here("r", "parse_accc_airports.R"),
                         output_dir = here::here("output"),
                         accc = NULL) {
  if (is.null(accc)) {
    if (!file.exists(workbook)) stop("Workbook not found: ", workbook)
    if (!file.exists(parser_path)) stop("Parser not found: ", parser_path)
    source(parser_path, local = TRUE)
    accc <- parse_accc_airports(workbook)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  
  
  financial <- prepare_accc_financial(accc)
  quality <- prepare_accc_quality(accc)
  comparisons <- make_accc_comparisons(financial)
  coverage <- financial |>
    group_by(metric, label, airport) |>
    summarise(first_year = min(year), last_year = max(year),
              n_years = n_distinct(year), .groups = "drop")
  latest <- financial |>
    filter(year == "2024-25") |>
    select(airport, metric, value) |>
    pivot_wider(names_from = metric, values_from = value) |>
    arrange(airport)

  write_csv(financial, file.path(output_dir, "financial_selected.csv"))
  write_csv(quality, file.path(output_dir, "security_ratings_by_terminal.csv"))
  write_csv(comparisons, file.path(output_dir, "2018-19_vs_2024-25.csv"))
  write_csv(latest, file.path(output_dir, "2024-25_snapshot.csv"))
  write_csv(coverage, file.path(output_dir, "series_coverage.csv"))
  plot_accc_trends(financial, quality, output_dir)

  message("Saved proposal screen to: ", normalizePath(output_dir))
  print(latest, width = Inf)
  invisible(list(financial = financial, quality = quality,
                 comparisons = comparisons, latest = latest, coverage = coverage))
}

# RStudio can evaluate a sourced script at frame zero. Only auto-run when this
# file itself was supplied to Rscript via --file; Source should define helpers.
accc_direct_script <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) == 1L &&
    basename(sub("^--file=", "", file_arg)) == "accc_airport_eda.R"
}

if (!interactive() && accc_direct_script()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 3L) stop("Expected at most three paths: workbook, parser, output directory.")
  overrides <- as.list(args)
  names(overrides) <- c("workbook", "parser_path", "output_dir")[seq_along(args)]
  do.call(run_accc_eda, overrides)
}
