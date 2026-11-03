# ACCC airport monitoring supplementary database parser
# Designed for: accc-airport-monitoring-report-2024-25-supplementary-database.xlsx

library(tidyverse)
library(tidyxl)
library(here)

# -----------------------------------------------------------------------------
# Basic cleaning / workbook helpers
# -----------------------------------------------------------------------------

year_rx <- "^\\d{4}[–-]\\d{2}$"
missing_tokens <- c("NA", "N/A", "#N/A", "-")

normalize_year <- function(x) {
  x |>
    str_squish() |>
    str_replace_all("–", "-")
}

clean_value_text <- function(x) {
  x <- str_squish(x)

  case_when(
    is.na(x) ~ NA_character_,
    x == "" ~ NA_character_,
    str_detect(x, year_rx) ~ NA_character_,
    str_to_upper(x) %in% missing_tokens ~ NA_character_,
    TRUE ~ x
  )
}

normalize_terminal <- function(x) {
  x <- str_squish(x)

  if_else(
    str_detect(x, "^\\d+$"),
    paste0("T", x),
    x
  )
}

clean_terminal_heading <- function(x, airport) {
  x |>
    str_remove(regex(paste0("^", airport, "\\s*[-–]?\\s*"), ignore_case = TRUE)) |>
    str_squish()
}

read_accc_cells <- function(path) {
  xlsx_cells(path) |>
    filter(!str_detect(sheet, regex(" charts$", ignore_case = TRUE))) |>
    mutate(
      airport = str_extract(sheet, "^[A-Z]{3}"),
      sheet_type = case_when(
        str_detect(sheet, "total and aero$") ~ "total_aero",
        str_detect(sheet, "Car park and Landside$") ~ "carpark_landside",
        TRUE ~ "other"
      ),
      chr = character |> str_squish() |> na_if(""),
      year_label = if_else(
        str_detect(coalesce(chr, ""), year_rx),
        normalize_year(chr),
        NA_character_
      )
    )
}

make_row_labels <- function(cells) {
  cells |>
    filter(col == 1, !is.na(chr)) |>
    transmute(
      sheet,
      airport,
      row,
      label = chr
    ) |>
    distinct()
}

sheet_last_row <- function(cells, sheet_name) {
  max(cells$row[cells$sheet == sheet_name], na.rm = TRUE)
}

find_section_row <- function(labels,
                             sheet_name,
                             pattern,
                             after_row = -Inf,
                             default = NA_integer_) {
  rows <- labels |>
    filter(
      sheet == sheet_name,
      row > after_row,
      str_detect(label, regex(pattern, ignore_case = TRUE))
    ) |>
    pull(row)

  if (length(rows) == 0) default else min(rows)
}

make_global_year_lookup <- function(cells) {
  cells |>
    filter(row == 1, !is.na(year_label)) |>
    transmute(sheet, col, year = year_label) |>
    distinct()
}

make_local_year_lookup <- function(cells,
                                   sheet_name,
                                   from_row,
                                   to_row,
                                   min_col = 2) {
  out <- cells |>
    filter(
      sheet == sheet_name,
      row >= from_row,
      row <= to_row,
      col >= min_col,
      !is.na(year_label)
    ) |>
    transmute(sheet, col, year = year_label) |>
    distinct()

  conflicts <- out |>
    count(sheet, col) |>
    filter(n > 1)

  if (nrow(conflicts) > 0) {
    stop(
      "Conflicting year labels found in ", sheet_name,
      " between rows ", from_row, " and ", to_row, "."
    )
  }

  out
}

# Pull the actual observations that sit in columns identified as years.
# value is numeric; value_text preserves unusual text-valued observations.
extract_year_values <- function(cells,
                                year_lookup,
                                sheet_name,
                                from_row,
                                to_row,
                                keep_text = FALSE) {
  out <- cells |>
    filter(
      sheet == sheet_name,
      row >= from_row,
      row <= to_row
    ) |>
    inner_join(
      year_lookup |> filter(sheet == sheet_name),
      by = c("sheet", "col")
    ) |>
    mutate(
      value = numeric,
      value_text = clean_value_text(chr)
    )

  if (!keep_text) {
    out <- out |> mutate(value_text = NA_character_)
  }

  out |>
    filter(!is.na(value) | !is.na(value_text)) |>
    transmute(
      sheet,
      airport,
      source_row = row,
      col,
      year,
      value,
      value_text
    )
}

# Detect whether a row has any non-blank cell in a year column. This is used to
# distinguish headings from measure rows, even where a measure contains only NA.
rows_with_year_entries <- function(cells,
                                   year_lookup,
                                   sheet_name,
                                   from_row,
                                   to_row) {
  cells |>
    filter(
      sheet == sheet_name,
      row >= from_row,
      row <= to_row
    ) |>
    inner_join(
      year_lookup |> filter(sheet == sheet_name),
      by = c("sheet", "col")
    ) |>
    filter(!is.na(numeric) | !is.na(chr)) |>
    distinct(source_row = row) |>
    mutate(has_year_entry = TRUE)
}

# Convert columns A:D in the metadata-style tables to one row per source row.
make_meta_table <- function(cells,
                            sheet_name,
                            from_row,
                            to_row) {
  field_lookup <- tibble(
    col = 1:4,
    field = c(
      "reported_airport",
      "market",
      "terminal_raw",
      "indicator"
    )
  )

  cells |>
    filter(
      sheet == sheet_name,
      row >= from_row,
      row <= to_row,
      col %in% 1:4
    ) |>
    mutate(
      meta_value = coalesce(
        chr,
        if_else(!is.na(numeric), as.character(numeric), NA_character_)
      )
    ) |>
    filter(!is.na(meta_value)) |>
    select(source_row = row, col, meta_value) |>
    inner_join(field_lookup, by = "col") |>
    select(-col) |>
    pivot_wider(
      names_from = field,
      values_from = meta_value
    )
}

find_metadata_header_row <- function(cells,
                                     sheet_name,
                                     after_row,
                                     d_pattern,
                                     before_row = Inf) {
  last_row <- sheet_last_row(cells, sheet_name)
  upper <- min(before_row - 1, last_row)

  if (upper <= after_row) return(NA_integer_)

  meta <- make_meta_table(
    cells,
    sheet_name,
    from_row = after_row + 1,
    to_row = upper
  )

  rows <- meta |>
    filter(
      str_to_lower(reported_airport) == "airport",
      str_detect(indicator, regex(d_pattern, ignore_case = TRUE))
    ) |>
    pull(source_row)

  if (length(rows) == 0) NA_integer_ else min(rows)
}

# -----------------------------------------------------------------------------
# Total-airport financial / operational data
# -----------------------------------------------------------------------------

parse_airport_financial_sheet <- function(cells,
                                          labels,
                                          global_years,
                                          sheet_name) {
  staff_start <- find_section_row(
    labels,
    sheet_name,
    "^Report of staff numbers$"
  )

  quality_start <- find_section_row(
    labels,
    sheet_name,
    "^Total airport and aeronautical quality data$"
  )

  possible_ends <- c(staff_start, quality_start)
  possible_ends <- possible_ends[!is.na(possible_ends)]

  end_row <- if (length(possible_ends) == 0) {
    sheet_last_row(cells, sheet_name)
  } else {
    min(possible_ends) - 1
  }

  extract_year_values(
    cells,
    global_years,
    sheet_name,
    from_row = 2,
    to_row = end_row,
    keep_text = TRUE
  ) |>
    left_join(
      labels |>
        filter(sheet == sheet_name) |>
        select(source_row = row, measure = label),
      by = "source_row"
    ) |>
    filter(!is.na(measure)) |>
    select(
      airport,
      sheet,
      source_row,
      measure,
      year,
      value,
      value_text
    )
}

# -----------------------------------------------------------------------------
# Total-airport quality data
# -----------------------------------------------------------------------------

parse_airport_quality_sheet <- function(cells,
                                        labels,
                                        global_years,
                                        sheet_name) {
  airport_code <- str_extract(sheet_name, "^[A-Z]{3}")

  quality_start <- find_section_row(
    labels,
    sheet_name,
    "^Total airport and aeronautical quality data$"
  )

  airline_start <- find_section_row(
    labels,
    sheet_name,
    "^Surveys of airlines$",
    after_row = quality_start
  )

  passenger_start <- find_section_row(
    labels,
    sheet_name,
    "^Surveys of passengers$",
    after_row = quality_start
  )

  end_row <- sheet_last_row(cells, sheet_name)

  row_state <- tibble(
    source_row = seq.int(quality_start + 1, end_row)
  ) |>
    left_join(
      labels |>
        filter(sheet == sheet_name) |>
        select(source_row = row, label),
      by = "source_row"
    ) |>
    left_join(
      rows_with_year_entries(
        cells,
        global_years,
        sheet_name,
        quality_start + 1,
        end_row
      ),
      by = "source_row"
    ) |>
    mutate(
      has_year_entry = replace_na(has_year_entry, FALSE),
      source = case_when(
        !is.na(passenger_start) & source_row > passenger_start ~ "passenger_survey",
        !is.na(airline_start) & source_row > airline_start ~ "airline_survey",
        TRUE ~ "objective"
      ),
      is_source_heading = str_detect(
        coalesce(label, ""),
        regex("^Surveys of (airlines|passengers)$", ignore_case = TRUE)
      ),
      is_terminal_heading = !has_year_entry &
        str_detect(coalesce(label, ""), regex("terminal", ignore_case = TRUE)),
      terminal = if_else(
        is_terminal_heading,
        clean_terminal_heading(label, airport_code),
        NA_character_
      )
    ) |>
    group_by(source) |>
    fill(terminal, .direction = "down") |>
    ungroup() |>
    mutate(
      category = case_when(
        !has_year_entry &
          !is_terminal_heading &
          !is_source_heading &
          !is.na(label) ~ label,
        TRUE ~ NA_character_
      )
    ) |>
    group_by(source, terminal) |>
    fill(category, .direction = "down") |>
    ungroup()

  extract_year_values(
    cells,
    global_years,
    sheet_name,
    from_row = quality_start + 1,
    to_row = end_row,
    keep_text = FALSE
  ) |>
    left_join(
      row_state |>
        select(source_row, source, terminal, category, measure = label),
      by = "source_row"
    ) |>
    filter(!is.na(measure), !is.na(value)) |>
    select(
      airport,
      sheet,
      source_row,
      source,
      terminal,
      category,
      measure,
      year,
      value
    )
}

# -----------------------------------------------------------------------------
# Car-parking and landside financial data
# -----------------------------------------------------------------------------

parse_carpark_financial_sheet <- function(cells,
                                          labels,
                                          global_years,
                                          sheet_name) {
  carpark_start <- find_section_row(
    labels,
    sheet_name,
    "^Car parking operational"
  )

  landside_start <- find_section_row(
    labels,
    sheet_name,
    "^Landside operational"
  )

  objective_start <- find_section_row(
    labels,
    sheet_name,
    "^Car parking objective indicators$"
  )

  values <- extract_year_values(
    cells,
    global_years,
    sheet_name,
    from_row = 2,
    to_row = objective_start - 1,
    keep_text = TRUE
  ) |>
    left_join(
      labels |>
        filter(sheet == sheet_name) |>
        select(source_row = row, measure = label),
      by = "source_row"
    ) |>
    filter(!is.na(measure)) |>
    mutate(
      section = case_when(
        source_row < carpark_start ~ "online_booking",
        source_row < landside_start ~ "car_parking",
        TRUE ~ "landside"
      )
    )

  # Perth has two distinct online-booking precincts; the other airports do not.
  online_heading_row <- labels |>
    filter(
      sheet == sheet_name,
      row >= 2,
      row < carpark_start
    ) |>
    summarise(row = min(row)) |>
    pull(row)

  online_state <- tibble(
    source_row = seq.int(2, carpark_start - 1)
  ) |>
    left_join(
      labels |>
        filter(sheet == sheet_name) |>
        select(source_row = row, label),
      by = "source_row"
    ) |>
    mutate(
      is_value_row = source_row %in% values$source_row,
      subgroup = case_when(
        !is_value_row &
          source_row != online_heading_row &
          !is.na(label) ~ label,
        TRUE ~ NA_character_
      )
    ) |>
    fill(subgroup, .direction = "down") |>
    select(source_row, subgroup)

  values |>
    left_join(online_state, by = "source_row") |>
    mutate(
      subgroup = if_else(section == "online_booking", subgroup, NA_character_)
    ) |>
    select(
      airport,
      sheet,
      source_row,
      section,
      subgroup,
      measure,
      year,
      value,
      value_text
    )
}

# -----------------------------------------------------------------------------
# Car-parking objective indicators
# -----------------------------------------------------------------------------

parse_carpark_objective_sheet <- function(cells,
                                          labels,
                                          global_years,
                                          sheet_name) {
  objective_start <- find_section_row(
    labels,
    sheet_name,
    "^Car parking objective indicators$"
  )

  ratings_start <- find_section_row(
    labels,
    sheet_name,
    "^Car parking passenger ratings"
  )

  values <- extract_year_values(
    cells,
    global_years,
    sheet_name,
    from_row = objective_start + 1,
    to_row = ratings_start - 1,
    keep_text = FALSE
  ) |>
    filter(!is.na(value))

  skip_heading_rx <- paste(
    "^Objective indicator(?: and precinct)?$",
    "airport car parking objective indicators$",
    sep = "|"
  )

  row_state <- tibble(
    source_row = seq.int(objective_start + 1, ratings_start - 1)
  ) |>
    left_join(
      labels |>
        filter(sheet == sheet_name) |>
        select(source_row = row, label),
      by = "source_row"
    ) |>
    mutate(
      is_value_row = source_row %in% values$source_row,
      precinct = case_when(
        !is_value_row &
          !is.na(label) &
          !str_detect(label, regex(skip_heading_rx, ignore_case = TRUE)) ~ label,
        TRUE ~ NA_character_
      )
    ) |>
    fill(precinct, .direction = "down")

  values |>
    left_join(
      row_state |>
        select(source_row, precinct, measure = label),
      by = "source_row"
    ) |>
    filter(!is.na(measure)) |>
    select(
      airport,
      sheet,
      source_row,
      precinct,
      measure,
      year,
      value
    )
}

# -----------------------------------------------------------------------------
# Character ratings tables (car park and landside)
# -----------------------------------------------------------------------------

parse_rating_table <- function(cells,
                               sheet_name,
                               header_row,
                               end_row,
                               table_name) {
  local_years <- make_local_year_lookup(
    cells,
    sheet_name,
    from_row = header_row,
    to_row = header_row,
    min_col = 5
  )

  values <- extract_year_values(
    cells,
    local_years,
    sheet_name,
    from_row = header_row + 1,
    to_row = end_row,
    keep_text = TRUE
  ) |>
    mutate(
      rating = coalesce(value_text, as.character(value))
    ) |>
    filter(!is.na(rating))

  meta <- make_meta_table(
    cells,
    sheet_name,
    from_row = header_row + 1,
    to_row = end_row
  )

  values |>
    left_join(meta, by = "source_row") |>
    filter(!is.na(indicator)) |>
    mutate(
      terminal = normalize_terminal(terminal_raw),
      market = str_squish(market),
      table = table_name
    ) |>
    select(
      airport,
      sheet,
      source_row,
      table,
      market,
      terminal,
      indicator,
      year,
      rating
    )
}

parse_carpark_ratings_sheet <- function(cells,
                                        labels,
                                        sheet_name) {
  ratings_start <- find_section_row(
    labels,
    sheet_name,
    "^Car parking passenger ratings"
  )

  landside_objective_start <- find_section_row(
    labels,
    sheet_name,
    "^Landside objective"
  )

  header_row <- find_metadata_header_row(
    cells,
    sheet_name,
    after_row = ratings_start,
    d_pattern = "quality indicator",
    before_row = landside_objective_start
  )

  parse_rating_table(
    cells,
    sheet_name,
    header_row = header_row,
    end_row = landside_objective_start - 1,
    table_name = "carpark"
  )
}

# -----------------------------------------------------------------------------
# Landside objective indicators
# -----------------------------------------------------------------------------

parse_landside_objective_sheet <- function(cells,
                                           labels,
                                           sheet_name) {
  objective_start <- find_section_row(
    labels,
    sheet_name,
    "^Landside objective"
  )

  objective_header <- find_metadata_header_row(
    cells,
    sheet_name,
    after_row = objective_start,
    d_pattern = "landslide objective indicators"
  )

  # Melbourne does not have a separate "Landside passenger surveys" title.
  # The next A:D header with "Quality indicator" is therefore the reliable end.
  ratings_header <- find_metadata_header_row(
    cells,
    sheet_name,
    after_row = objective_start,
    d_pattern = "quality indicator"
  )

  local_years <- make_local_year_lookup(
    cells,
    sheet_name,
    from_row = objective_header,
    to_row = ratings_header - 1,
    min_col = 5
  )

  values <- extract_year_values(
    cells,
    local_years,
    sheet_name,
    from_row = objective_header + 1,
    to_row = ratings_header - 1,
    keep_text = FALSE
  ) |>
    filter(!is.na(value))

  meta <- make_meta_table(
    cells,
    sheet_name,
    from_row = objective_header + 1,
    to_row = ratings_header - 1
  ) |>
    mutate(
      metadata_source_row = source_row,
      repaired = FALSE
    )

  # In MEL and SYD the first objective row contains the year labels instead of
  # its observations, while a later row contains those observations with blank
  # A:D metadata. Repair that geometry without hard-coding airport or row number.
  year_rows <- cells |>
    filter(
      sheet == sheet_name,
      row > objective_header,
      row < ratings_header,
      col >= 5,
      !is.na(year_label)
    ) |>
    distinct(source_row = row) |>
    pull(source_row)

  orphan_rows <- setdiff(
    unique(values$source_row),
    meta |>
      filter(!is.na(indicator)) |>
      pull(source_row)
  )

  donor_meta <- meta |>
    filter(
      source_row %in% year_rows,
      !is.na(indicator)
    )

  if (length(orphan_rows) == 1 && nrow(donor_meta) == 1) {
    donor_row <- donor_meta$source_row[[1]]

    repaired_meta <- donor_meta |>
      mutate(
        source_row = orphan_rows[[1]],
        metadata_source_row = donor_row,
        repaired = TRUE
      )

    meta <- bind_rows(meta, repaired_meta)
  } else if (length(orphan_rows) > 0) {
    warning(
      sheet_name,
      ": found orphan landside-objective value row(s) that could not be ",
      "repaired automatically: ",
      paste(orphan_rows, collapse = ", ")
    )
  }

  values |>
    left_join(meta, by = "source_row") |>
    filter(!is.na(indicator)) |>
    mutate(
      terminal = normalize_terminal(terminal_raw),
      market = str_squish(market)
    ) |>
    select(
      airport,
      sheet,
      source_row,
      metadata_source_row,
      repaired,
      market,
      terminal,
      indicator,
      year,
      value
    )
}

parse_landside_ratings_sheet <- function(cells,
                                         labels,
                                         sheet_name) {
  objective_start <- find_section_row(
    labels,
    sheet_name,
    "^Landside objective"
  )

  header_row <- find_metadata_header_row(
    cells,
    sheet_name,
    after_row = objective_start,
    d_pattern = "quality indicator"
  )

  parse_rating_table(
    cells,
    sheet_name,
    header_row = header_row,
    end_row = sheet_last_row(cells, sheet_name),
    table_name = "landside"
  )
}

# -----------------------------------------------------------------------------
# Diagnostics
# -----------------------------------------------------------------------------

build_parse_diagnostics <- function(parsed) {
  output_names <- c(
    "airport_financial",
    "airport_quality",
    "carpark_financial",
    "carpark_objective",
    "carpark_ratings",
    "landside_objective",
    "landside_ratings"
  )

  summary <- map_dfr(
    output_names,
    \(nm) {
      x <- parsed[[nm]]

      tibble(
        table = nm,
        rows = nrow(x),
        airports = n_distinct(x$airport),
        first_year = if ("year" %in% names(x) && nrow(x) > 0) min(x$year) else NA_character_,
        last_year = if ("year" %in% names(x) && nrow(x) > 0) max(x$year) else NA_character_
      )
    }
  )

  repairs <- parsed$landside_objective |>
    filter(repaired) |>
    distinct(
      airport,
      sheet,
      source_row,
      metadata_source_row,
      market,
      terminal,
      indicator
    )

  text_values <- bind_rows(
    parsed$airport_financial |>
      filter(!is.na(value_text)) |>
      mutate(table = "airport_financial"),
    parsed$carpark_financial |>
      filter(!is.na(value_text)) |>
      mutate(table = "carpark_financial")
  ) |>
    select(table, airport, sheet, source_row, year, value_text, everything())

  list(
    summary = summary,
    repaired_rows = repairs,
    text_valued_observations = text_values
  )
}

# Optional validation against the 2024-25 workbook used to develop this parser.
validate_accc_2024_25 <- function(parsed) {
  expected <- tribble(
    ~table,                ~rows,
    "airport_financial",    3189,
    "airport_quality",      4639,
    "carpark_financial",    2626,
    "carpark_objective",     500,
    "carpark_ratings",       113,
    "landside_objective",     94,
    "landside_ratings",       93
  )

  observed <- parsed$diagnostics$summary |>
    select(table, observed = rows)

  expected |>
    left_join(observed, by = "table") |>
    mutate(ok = rows == observed)
}

# -----------------------------------------------------------------------------
# Main wrapper
# -----------------------------------------------------------------------------

parse_accc_airports <- function(path, keep_raw = FALSE) {
  cells <- read_accc_cells(path)
  labels <- make_row_labels(cells)
  global_years <- make_global_year_lookup(cells)

  total_sheets <- cells |>
    distinct(sheet, sheet_type) |>
    filter(sheet_type == "total_aero") |>
    pull(sheet)

  carpark_sheets <- cells |>
    distinct(sheet, sheet_type) |>
    filter(sheet_type == "carpark_landside") |>
    pull(sheet)

  parsed <- list(
    airport_financial = map_dfr(
      total_sheets,
      \(s) parse_airport_financial_sheet(cells, labels, global_years, s)
    ),

    airport_quality = map_dfr(
      total_sheets,
      \(s) parse_airport_quality_sheet(cells, labels, global_years, s)
    ),

    carpark_financial = map_dfr(
      carpark_sheets,
      \(s) parse_carpark_financial_sheet(cells, labels, global_years, s)
    ),

    carpark_objective = map_dfr(
      carpark_sheets,
      \(s) parse_carpark_objective_sheet(cells, labels, global_years, s)
    ),

    carpark_ratings = map_dfr(
      carpark_sheets,
      \(s) parse_carpark_ratings_sheet(cells, labels, s)
    ),

    landside_objective = map_dfr(
      carpark_sheets,
      \(s) parse_landside_objective_sheet(cells, labels, s)
    ),

    landside_ratings = map_dfr(
      carpark_sheets,
      \(s) parse_landside_ratings_sheet(cells, labels, s)
    )
  )

  #parsed$diagnostics <- build_parse_diagnostics(parsed)

  if (keep_raw) {
    parsed$cells <- cells
    parsed$labels <- labels
    parsed$global_years <- global_years
  }

  parsed
}

# parsed$diagnostics <- build_parse_diagnostics(parsed)

# Main call and export ---------------------------------------------------------------------
accc <- parse_accc_airports(
  here::here("data", "australia", "accc-airport-monitoring-report-2024-25-supplementary-database.xlsx")
)

write_rds(accc, here("data", "australia", "accc_monitor_data_2005_2025.rds"))
writexl::write_xlsx(accc, here("data", "australia", "accc_monitor_data_2005_2025.xlsx"))

# Prepare a financial subset
aero_fin  <- accc$airport_financial
parking_fin  <- accc$carpark_financial

fin_variables <- c(
  "Average staff equivalents - Aeronautical services",
  "Average staff equivalents - Non-aeronautical services",
  "Passengers ('000)",
  "Domestic Passengers ('000)",
  "Aeronautical Revenue per passenger ($)",
  "Aeronautical Expenses per passenger ($)",
  "Aeronautical Operating profit per passenger ($)",
  "Aeronautical revenue ($m)",
  "Aeronautical operating expenses ($m)",
  "Aeronautical operating profit ($m)",
  "Aeronautical profit margin (%)",
  "Total airport revenue ($m)",
  "Total airport operating expenses ($m)",
  "Total airport operating profit ($m)",
  "Total airport profit margin (%)",
  "Aeronautical total non-current assets ($m)",
  "Total airport total non-current assets ($m)",
  "Rate of return (EBITA) on tangible non-current assets for aeronautical services (%)"
)

aero_profit <- aero_fin |> 
  filter(
    str_detect(measure, "including landfill", negate = TRUE) 
  ) |> 
  mutate(
    measure = str_remove_all(measure, "\\*$| - excluding landfill") 
  ) |> 
  filter(measure %in% fin_variables) |> 
  select(airport, measure, year, value)

park_variables <- c(
"Car parking revenue ($million)",
"Car parking operating expenses ($million)",
"Car parking operating profit ($million)",
"Car parking profit margin (%)",
"Revenue per vehicle ($)",
"Operating profit per vehicle ($)"
)

parking_profit <- parking_fin |>
  mutate(
    measure = recode(
      measure,
      "Car parking operating margin ($million)" =
        "Car parking operating profit ($million)",
      "Operating margin per vehicle ($)" =
        "Operating profit per vehicle ($)"
    )
  ) |>
  filter(measure %in% park_variables) |>
  select(airport, measure, year, value)

landside_profit <- parking_fin |>
  filter(measure == "Total landside access revenue ($ millions)") |>
  transmute(
    airport, year,
    measure = "Landside access revenue ($m)",
    value
  )

# Passenger traffic: one observation per airport and financial year.
# ACCC source unit is thousands of passengers. Export persons for consistent
# denominators alongside Statistics Canada's Canadian passenger series.
aus_passengers <- aero_fin |>
  filter(measure == "Passengers ('000)") |>
  transmute(
    airport, year,
    passengers = value * 1000,
    passengers_thousands = value,
    source = "ACCC supplementary database"
  ) |>
  distinct(airport, year, .keep_all = TRUE) |>
  arrange(airport, year)

readr::write_csv(
  aus_passengers,
  here("data", "australia", "accc_airport_passengers_2007_2025.csv")
)

# Revenue, expenses and profits ---------------------------------------

fin_lookup <- c(
  "Total airport revenue ($m)" = "total_revenue",
  "Total airport operating expenses ($m)" = "total_expenses",
  "Total airport operating profit ($m)" = "total_profit",
  "Aeronautical revenue ($m)" = "aero_revenue",
  "Aeronautical operating expenses ($m)" = "aero_expenses",
  "Aeronautical operating profit ($m)" = "aero_profit",
  "Car parking revenue ($million)" = "parking_revenue",
  "Car parking operating expenses ($million)" = "parking_expenses",
  "Car parking operating profit ($million)" = "parking_profit",
  "Landside access revenue ($m)" = "landside_revenue"
)

rev_exp_prof_export <- bind_rows(aero_profit, parking_profit, landside_profit) |>
  filter(measure %in% names(fin_lookup)) |>
  mutate(variable = unname(fin_lookup[measure])) |>
  select(airport, year, variable, value) |>
  pivot_wider(names_from = variable, values_from = value) |>
  mutate(
    parking_transport_revenue = parking_revenue + landside_revenue,
    # Revenue residual excludes landside; expense/profit residuals still include it.
    # Do not calculate a margin for this residual segment.
    other_revenue = total_revenue - aero_revenue - parking_transport_revenue,
    other_expenses = total_expenses - aero_expenses - parking_expenses,
    other_profit = total_profit - aero_profit - parking_profit
  ) |>
  select(
    airport, year,
    starts_with("total_"),
    starts_with("aero_"),
    starts_with("parking_"),
    landside_revenue,
    starts_with("other_")
  ) |>
  arrange(airport, year)

# Per-unit measures and margins ---------------------------------------

unit_lookup <- c(
  "Aeronautical Revenue per passenger ($)" = "aero_revenue_per_passenger",
  "Aeronautical Expenses per passenger ($)" = "aero_expenses_per_passenger",
  "Aeronautical Operating profit per passenger ($)" = "aero_profit_per_passenger",
  "Revenue per vehicle ($)" = "parking_revenue_per_vehicle",
  "Operating profit per vehicle ($)" = "parking_profit_per_vehicle"
)

unit_values <- bind_rows(aero_profit, parking_profit) |>
  filter(measure %in% names(unit_lookup)) |>
  mutate(variable = unname(unit_lookup[measure])) |>
  select(airport, year, variable, value) |>
  pivot_wider(names_from = variable, values_from = value)

passengers <- aero_profit |>
  filter(measure == "Passengers ('000)") |>
  transmute(airport, year, passengers = value)

per_unit_margin_export <- rev_exp_prof_export |>
  left_join(passengers, by = c("airport", "year")) |>
  left_join(unit_values, by = c("airport", "year")) |>
  mutate(
    # Per-passenger figures ($)
    across(
      c(
        total_revenue, total_expenses, total_profit,
        parking_revenue, parking_expenses, parking_profit,
        landside_revenue, parking_transport_revenue,
        other_revenue, other_expenses, other_profit
      ),
      ~ if_else(passengers > 0, .x / passengers * 1000, NA_real_),
      .names = "{.col}_per_passenger"
    ),
    
    # Operating profit margins (%)
    total_op_margin = 100 * total_profit / total_revenue,
    aero_op_margin = 100 * aero_profit / aero_revenue,
    parking_op_margin = 100 * parking_profit / parking_revenue
  ) |>
  select(
    airport, year,
    starts_with("total_") & ends_with("_per_passenger"),
    starts_with("aero_") & ends_with("_per_passenger"),
    starts_with("parking_") & ends_with("_per_passenger"),
    landside_revenue_per_passenger,
    ends_with("_per_vehicle"),
    starts_with("other_") & ends_with("_per_passenger"),
    ends_with("_margin")
  ) |>
  arrange(airport, year)

# EBITA rates of return -----------------------------------------------

return_lookup <- c(
  "Rate of return (EBITA) on tangible non-current assets for aeronautical services (%)" =
    "aero_ebita_roa",
  
  "Rate of return (EBITA) on tangible non-current assets for aeronautical services (%) - excluding landfill" =
    "aero_ebita_roa",
  
  "Rate of return (EBITA) on tangible non-current assets for total airport services (%) - excluding inter-company" =
    "total_ebita_roa"
)

ebita_values <- aero_fin |>
  mutate(measure = str_remove(measure, "\\*$")) |>
  filter(measure %in% names(return_lookup)) |>
  mutate(
    variable = unname(return_lookup[measure]),
    landfill_adjusted = str_detect(measure, "excluding landfill")
  ) |>
  arrange(airport, year, landfill_adjusted) |>
  distinct(airport, year, variable, .keep_all = TRUE) |>
  select(airport, year, variable, value) |>
  pivot_wider(names_from = variable, values_from = value)

per_unit_margin_export <- per_unit_margin_export |>
  left_join(ebita_values, by = c("airport", "year"))

# Separate per-unit figures and margins ----------------------------

per_unit_export <- per_unit_margin_export |>
  select(
    airport, year,
    ends_with("_per_passenger"),
    ends_with("_per_vehicle")
  )

margins_export <- per_unit_margin_export |>
  select(
    airport, year,
    ends_with("_margin"),
    ends_with("_ebita_roa")
  )

# Diagnostics -------------------------------------------------------------
# Underlying per-passenger calculations

# Keep all revenue categories, including landside and the combined parking rate.
# Expenses/profit for the residual "other" segment are not comparable to its
# landside-excluding revenue; they are therefore omitted from segment diagnostics.
passenger_calcs <- rev_exp_prof_export |>
  left_join(passengers, by = c("airport", "year")) |>
  pivot_longer(
    cols = matches("^(total|aero|parking)_(revenue|expenses|profit)$|^(landside|parking_transport|other)_revenue$"),
    names_to = "variable",
    values_to = "amount_m"
  ) |>
  mutate(per_passenger = if_else(passengers > 0, amount_m / passengers * 1000, NA_real_)) |>
  select(airport, year, variable, amount_m, passengers, per_passenger)

# Valid segment margins: total airport, aeronautical services and car parking.
margin_calcs <- rev_exp_prof_export |>
  select(airport, year, matches("^(total|aero|parking)_(revenue|expenses|profit)$")) |>
  pivot_longer(
    cols = -c(airport, year),
    names_to = c("category", ".value"),
    names_pattern = "^(total|aero|parking)_(revenue|expenses|profit)$"
  ) |>
  mutate(margin = 100 * profit / revenue) |>
  select(airport, year, category, revenue, profit, margin)

# Export ------------------------------------------------------------------

writexl::write_xlsx(
  list(
    financial = rev_exp_prof_export,
    margins = margins_export,
    per_passenger = per_unit_export,
    passenger_calcs = passenger_calcs,
    margin_calcs = margin_calcs
  ),
  here("output", "accc_airport_financial_analysis_2007_2025.xlsx")
)
