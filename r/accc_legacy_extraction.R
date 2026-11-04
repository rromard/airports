# ACCC legacy airport data: 2006-07 report -------------------------------------
# Purpose:
#   Reconstruct historical Australian airport financial observations from
#   comparative charts and monitoring tables in the ACCC 2006-07 report.
#
# Coverage:
#   - Brisbane, Melbourne, Perth and Sydney.
#   - Comparative charts: up to 1997-98 through 2006-07.
#   - Financial monitoring tables: 2002-03 through 2006-07.
#   - Sydney 1997-98 revenue per passenger supplemented from FAC records.
#
# Main calculations:
#   - Extract passengers, aeronautical revenue, expenditure and operating
#     profit from PDF charts using the accompanying Python script.
#   - Extract revenue, expenditure and operating profit by service category
#     from historical ACCC monitoring tables.
#   - Calculate unadjusted aeronautical revenue per passenger.
#   - Check financial-table arithmetic and compare overlapping revenue
#     measures, noting differences in reporting scope.
#
# Main outputs (output/):
#   - accc_legacy_vector_selected.csv
#       Historical chart series used by the counterfactual script.
#   - accc_legacy_financial_tables.csv
#       Revenue, expenditure and profit by service category.
#   - accc_legacy_unadjusted_revenue_per_pax.csv
#       Derived historical revenue per passenger.
#   - diagnostics/accc_legacy_*.csv
#       Source extraction and reconciliation audit files.
#
# Financial years use their ending year (2003 = 2002-03).
# Chart-derived observations are approximate, not audited tabular values.

xfun::pkg_attach("tidyverse", "here", "pdftools")

# Paths and shared metadata -----------------------------------------------------
source_pdf <- here("data", "australia", "legacy_reports",
                   "Airport monitoring report 2006-07.pdf")
python_script <- here("r", "extract_comparative_charts.py")
output_dir <- here("output")
diagnostic_dir <- file.path(output_dir, "diagnostics")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(source_pdf), file.exists(python_script))
airports <- c("BNE", "MEL", "PER", "SYD")

chart_manifest <- tribble(
  ~pdf_page, ~chart, ~measure,                          ~adjustment,         ~units,
  32,        "2.1",  "passengers",                      "not_applicable",   "passengers",
  33,        "2.2",  "aero_revenue_per_pax",            "ansett_adjusted",  "AUD per passenger",
  35,        "2.4",  "aero_revenue_total",              "reported",         "AUD thousands",
  38,        "2.7",  "aero_expenditure_per_pax",        "reported",         "AUD per passenger",
  39,        "2.8",  "aero_operating_profit_per_pax",   "reported",         "AUD per passenger"
)

table_manifest <- tribble(
  ~airport, ~table_page, ~source_table,
  "BNE",    90,          "4.2",
  "MEL",    170,         "7.2",
  "PER",    203,         "8.2",
  "SYD",    238,         "9.2"
)

targets <- tribble(
  ~service_category,            ~label_pattern, ~accc_category,
  "aeronautical_total",         "^\\s*Total aeronautical\\b", "aeronautical",
  "check_in",                   "^\\s*Check-in counters\\b", "aeronautical_related",
  "parking",                    "^\\s*Public and staff car", "aeronautical_related",
  "landside_access",            "^\\s*(Landside vehicle access|Taxi holding and feeder)", "aeronautical_related",
  "aeronautical_related_total", "^\\s*Total aeronautical(?:-related|-\\s*$)", "aeronautical_related"
)

fyear_label <- function(year) paste0(year - 1, "-", str_sub(as.character(year), -2))

# 1. Vector chart extraction (Python) ------------------------------------------
vector_all_csv <- file.path(diagnostic_dir, "accc_legacy_vector_all_airports.csv")

# 'system2' is synchronous. A nonzero exit status stops the pipeline so that
# an old CSV is not mistakenly imported after a failed Python run.
python_output <- suppressWarnings(system2(
  "python",
  args = c(shQuote(python_script), shQuote(source_pdf), shQuote(vector_all_csv)),
  stdout = TRUE, stderr = TRUE
))
status <- attr(python_output, "status")
if (!is.null(status) && status != 0L) {
  stop("Python chart extraction failed:\n", paste(python_output, collapse = "\n"))
}

vector_all <- readr::read_csv(vector_all_csv, show_col_types = FALSE)
accc_vector <- vector_all |>
  filter(airport %in% airports) |>
  mutate(fyear = fyear_label(year),
         source = "ACCC Airport Monitoring Report 2006-07",
         extraction_method = "vector_geometry")

# Validate key uniqueness and chart metadata, without filling absent bars.
stopifnot(!anyDuplicated(accc_vector[c("airport", "year", "measure")]))
stopifnot(all(accc_vector$pdf_page %in% chart_manifest$pdf_page))

# Add in a missing year of historical data from SYD from the Federal Airport Corp report from 1997-1998
# https://nla.gov.au/nla.obj-2312918333/view?partId=nla.obj-2318237485
syd98 <- tibble(
  airport = "SYD",
  year = 1998L,
  fyear  = fyear_label(1998),
  chart = 2.2,
  pdf_page = 31,
  measure = "aero_revenue_per_pax",
  adjustment = "adjusted",
  value = 93.5e6 / 21552661,
  extraction_method = "retrieved from FAC report"
)

accc_vector <- accc_vector |> 
  bind_rows(syd98) |> 
  arrange(airport, chart, year)

readr::write_csv(accc_vector,
                 file.path(output_dir, "accc_legacy_vector_selected.csv"))
readr::write_csv(chart_manifest,
                 file.path(diagnostic_dir, "accc_legacy_chart_manifest.csv"))

# Derived *reported* (not Ansett-adjusted) revenue per passenger.
# Chart 2.4 revenue is in AUD thousands; Chart 2.1 passengers are persons.
aero_revenue_unadjusted <- accc_vector |>
  filter(measure %in% c("passengers", "aero_revenue_total")) |>
  select(airport, year, measure, value) |>
  pivot_wider(names_from = measure, values_from = value) |>
  mutate(
    fyear = fyear_label(year),
    aero_revenue_unadjusted_per_pax =
      if_else(!is.na(passengers) & passengers > 0,
              aero_revenue_total * 1000 / passengers, NA_real_)
  ) |>
  relocate(airport, year, fyear)

readr::write_csv(aero_revenue_unadjusted,
                 file.path(output_dir, "accc_legacy_unadjusted_revenue_per_pax.csv"))

# 2. Numerical monitoring-table extraction (pdftools) --------------------------
pages <- pdftools::pdf_text(source_pdf)

extract_row <- function(page, label_pattern) {
  lines <- stringr::str_split(page, "\n")[[1]]
  i <- which(str_detect(lines, regex(label_pattern)))[1]
  if (is.na(i)) return(NA_character_)
  candidates <- lines[i:min(i + 4L, length(lines))]
  # Some labels wrap onto the line carrying the values.
  numeric_lines <- candidates[str_detect(candidates, "\\d")]
  if (length(numeric_lines) == 0L) return(NA_character_)
  numeric_lines[[1]]
}

financial_rows <- tidyr::crossing(table_manifest, targets) |>
  mutate(raw_row = purrr::map2_chr(
    table_page, label_pattern, ~ extract_row(pages[[.x]], .y)
  )) |>
  select(airport, table_page, source_table, service_category,
         accc_category, raw_row)

if (anyNA(financial_rows$raw_row)) {
  print(financial_rows |> filter(is.na(raw_row)))
  stop("One or more financial-table rows could not be located")
}
readr::write_csv(financial_rows,
                 file.path(diagnostic_dir, "accc_legacy_table_raw_rows.csv"))

parse_financial_row <- function(row, airport, service_category, accc_category) {
  row <- str_remove_all(row, "\\([a-z]\\)")
  tokens <- str_extract_all(
    row, "\\(\\d{1,3}(?: \\d{3})?\\)|\\d{1,3} \\d{3}|\\d{4,6}|\\d{1,3}"
  )[[1]]
  values <- tokens |>
    str_replace_all("^\\((.*)\\)$", "-\\1") |>
    str_remove_all(" ") |>
    as.numeric()
  if (length(values) != 15L) {
    stop(airport, " / ", service_category,
         ": expected 15 financial values, found ", length(values),
         "\nSource: ", row)
  }
  tibble(
    airport = airport, service_category = service_category,
    accc_category = accc_category,
    year = rep(2003:2007, 3),
    measure = rep(c("revenue", "expenditure", "operating_profit"), each = 5),
    value = values
  )
}

financial_data <- financial_rows |>
  mutate(parsed = pmap(
    list(raw_row, airport, service_category, accc_category),
    parse_financial_row
  )) |>
  select(table_page, source_table, parsed) |>
  unnest(parsed) |>
  mutate(fyear = fyear_label(year), units = "AUD thousands",
         source = "ACCC Airport Monitoring Report 2006-07",
         extraction_method = "pdf_text_table") |>
  relocate(airport, year, fyear, service_category, accc_category, measure)

readr::write_csv(financial_data,
                 file.path(output_dir, "accc_legacy_financial_tables.csv"))

# 3. Diagnostics and overlapping-year comparison -------------------------------
table_arithmetic <- financial_data |>
  select(airport, year, service_category, measure, value) |>
  pivot_wider(names_from = measure, values_from = value) |>
  mutate(difference_aud_thousands = revenue - expenditure - operating_profit)
readr::write_csv(table_arithmetic,
                 file.path(diagnostic_dir, "accc_legacy_table_arithmetic_checks.csv"))

# Comparison is diagnostic ONLY: Chart 2.4 uses the broader regulatory-account
# aeronautical allocation; the table's aeronautical_total is narrower.
revenue_comparison <- accc_vector |>
  filter(measure == "aero_revenue_total") |>
  select(airport, year, chart_revenue = value) |>
  inner_join(
    financial_data |>
      filter(service_category == "aeronautical_total", measure == "revenue") |>
      select(airport, year, table_revenue = value),
    by = c("airport", "year")
  ) |>
  mutate(difference_aud_thousands = chart_revenue - table_revenue,
         pct_difference = 100 * difference_aud_thousands / table_revenue,
         comparison_note = "Different definitions: regulatory-account chart vs narrow monitoring-table aeronautical services")
readr::write_csv(revenue_comparison,
                 file.path(diagnostic_dir, "accc_legacy_revenue_scope_comparison.csv"))

message("ACCC legacy extraction complete. Analysis data: ", output_dir,
        "; diagnostics: ", diagnostic_dir)
message("Chart records (selected airports): ", nrow(accc_vector),
        "; table financial records: ", nrow(financial_data))

