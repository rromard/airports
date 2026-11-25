# Interactive Datawrapper chart preparation
# -----------------------------------------
# Run AFTER r/prepare_datawrapper_exports.R has created the source tables.
# This script only loads the prepared tables and sets up a workspace for
# hand-written chart transformations. It does NOT regenerate upstream data,
# write charts automatically, or modify any analytical source tables.
#
# From the repository root:
#   source(here::here("r", "datawrapper_chart_workspace.R"))
#
# Typical interactive workflow:
#   names(dw)                  # available source datasets
#   dw_catalog                 # source, units, caveats and status
#   View(dw$financials_2025)   # inspect source
#   # chart <- dw$financials_2025 |> filter(...) |> ...
#   # write_chart(chart, "my_chart")
#
# Outputs: output/datawrapper_charts/ (only when write_chart() is called)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(here)
})

dw_source_dir <- here::here("output", "presentation_data")
dw_chart_dir <- here::here("output", "datawrapper_charts")
dw_catalog_path <- file.path(dw_source_dir, "00_data_catalog.csv")

if (!file.exists(dw_catalog_path)) {
  stop(
    "Missing Datawrapper source catalog: ", dw_catalog_path,
    "\nRun r/prepare_datawrapper_exports.R first.",
    call. = FALSE
  )
}

dw_catalog <- readr::read_csv(dw_catalog_path, show_col_types = FALSE)
required_catalog_cols <- c("file", "theme", "status", "source", "interpretation")
missing_catalog_cols <- setdiff(required_catalog_cols, names(dw_catalog))
if (length(missing_catalog_cols)) {
  stop("Catalog missing columns: ", paste(missing_catalog_cols, collapse = ", "))
}

# Friendly names for the standard exports. Optional sources remain absent
# from 'dw' if they have not been produced; inspect dw_catalog for reasons.
dw_names <- c(
  "00a_country_context_2025.csv" = "country_context",
  "00b_domestic_airline_capacity_2022.csv" = "airline_shares",
  "00c_top_two_airline_share_2022.csv" = "airline_top_two",
  "00d_policy_context.csv" = "policy_context",
  "01_airport_scale_2025.csv" = "airport_scale",
  "01b_big4_aggregate_2025.csv" = "big4_scale",
  "02_actual_aeronautical_yield_2015_2025.csv" = "aero_yield",
  "03_current_financials_2025.csv" = "financials_2025",
  "04_australia_tax_cash_accounting_2016_2025.csv" = "australia_tax",
  "04b_australia_ato_entities_2013_2025.csv" = "ato_entities",
  "04c_australia_tax_periods.csv" = "australia_tax_periods",
  "05_acquisition_assumptions.csv" = "acquisition_assumptions",
  "06_acquisition_scenarios_2025.csv" = "acquisition_scenarios",
  "07_profitability_targets_2025.csv" = "profitability_targets",
  "08_revenue_impacts_2025.csv" = "revenue_impacts",
  "09_australia_observed_real_yield_1998_2007.csv" = "historical_real_yield",
  "09_legacy_cap_then_shock_paths.csv" = "growth_paths",
  "10_australia_quality_series.csv" = "quality"
)

ready_files <- dw_catalog |>
  filter(status == "ready", !is.na(file), nzchar(file)) |>
  pull(file)

if (anyDuplicated(ready_files)) {
  stop("Duplicate ready filenames in presentation catalog.")
}

missing_files <- ready_files[!file.exists(file.path(dw_source_dir, ready_files))]
if (length(missing_files)) {
  stop(
    "Catalog lists missing source files:\n",
    paste0("  - ", missing_files, collapse = "\n"),
    "\nRerun r/prepare_datawrapper_exports.R.",
    call. = FALSE
  )
}

# Load every catalogued ready CSV (including any future new exports).
# Preserve unknown filenames as sensible names rather than dropping them.
make_dw_name <- function(filename) {
  if (filename %in% names(dw_names)) return(unname(dw_names[[filename]]))
  gsub("[^a-z0-9]+", "_", sub("\\.csv$", "", tolower(filename)))
}

dw <- lapply(file.path(dw_source_dir, ready_files), function(path) {
  readr::read_csv(path, show_col_types = FALSE)
})
names(dw) <- make.unique(vapply(ready_files, make_dw_name, character(1)))

# A convenient inventory for deciding which charts to build next.
dw_inventory <- tibble::tibble(
  dataset = names(dw),
  file = ready_files,
  rows = vapply(dw, nrow, integer(1)),
  columns = vapply(dw, ncol, integer(1))
) |>
  left_join(dw_catalog |> select(file, theme, interpretation), by = "file")

# Source data should remain unchanged. Only write intentionally prepared
# chart tables; keep chart files in their own directory.
write_chart <- function(data, name, overwrite = FALSE) {
  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  if (!nrow(data)) stop("Refusing to export an empty chart.")
  if (anyDuplicated(names(data))) stop("Duplicate chart column names.")
  if (length(name) != 1L || is.na(name) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", name)) {
    stop("Use a simple chart name: letters, numbers, underscores or hyphens.")
  }
  dir.create(dw_chart_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dw_chart_dir, paste0(name, ".csv"))
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("Chart already exists: ", path,
         "\nUse overwrite = TRUE if intentional.")
  }
  readr::write_csv(data, path, na = "")
  message("Saved chart CSV: ", path)
  invisible(path)
}

# Quick lookup of the source and methodological notes for a loaded table.
dw_notes <- function(dataset) {
  match_row <- dw_inventory |> filter(.data$dataset == dataset)
  if (nrow(match_row) != 1L) stop("Unknown dataset: ", dataset)
  dw_catalog |> filter(file == match_row$file[[1L]]) |>
    select(theme, source, interpretation)
}

# -------------------------------------------------------------------------
# INTERACTIVE CHART WORKSPACE
# -------------------------------------------------------------------------
# Define chart objects here, inspect them, then call write_chart() yourself.
# Do not overwrite objects in 'dw' if you want to retain the source tables.
#
# EXAMPLE: domestic airline concentration (same-week, comparable measure)
#
# airline_chart <- dw$airline_shares |>
#   select(country, airline_group, domestic_seat_capacity_share_pct)
# View(airline_chart)
# write_chart(airline_chart, "airline_concentration_2022")
#
# EXAMPLE: Australian tax ratios (explicitly different denominators)
#
# tax_rate_chart <- dw$australia_tax |>
#   select(airport, year_end, cash_etr, ato_payable_rate)
# write_chart(tax_rate_chart, "australian_tax_rates")
#
# EXAMPLE: broad corporate income tax gap, nominal AUD.
# Prefer cumulative periods to reduce annual timing effects.
# tax_gap_chart <- dw$australia_tax_periods |>
#   select(airport, period, analysis_ebt, expected_tax_on_accounting_profit,
#          analysis_cash_tax_paid, tax_gap, n_cash_assumed)
# write_chart(tax_gap_chart, "australian_tax_gap")
#
# Sydney is an illustrative adjusted gap, not observed parent-level tax.
# Sydney observed cash tax stays missing; n_cash_assumed identifies scenario years.
#
# EXAMPLE: financial comparison
#
# margin_chart <- dw$financials_2025 |>
#   filter(metric == "ebitda_margin_pct") |>
#   select(country, airport, value)
# write_chart(margin_chart, "ebitda_margins_2025")
#
# Useful commands:
# names(dw)
# dw_inventory
# dw_notes("australia_tax")
# View(dw$revenue_impacts)
# -------------------------------------------------------------------------

message(
  "Loaded ", length(dw), " Datawrapper source tables into 'dw'.\n",
  "Inspect 'dw_inventory' or 'dw_catalog'; call write_chart() when ready."
)
