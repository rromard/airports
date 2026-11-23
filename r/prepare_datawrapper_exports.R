# Datawrapper presentation exports | independent of workbook formatting
# Run after the production pipeline:
#   source(here::here("r", "prepare_datawrapper_exports.R"))
# Does NOT rebuild model inputs or alter acquisition assumptions.
# Exports are tidy CSVs; all currency comparisons already use 2025 CAD.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(here)
})
root <- here::here()
out <- here::here("output", "presentation_data")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
can_airports <- c("YYC", "YUL", "YVR", "YYZ")
aus_airports <- c("BNE", "MEL", "PER", "SYD")
all_airports <- c(can_airports, aus_airports)
base_comparison <- here::here("output", "airport_comparison")
base_bridge <- file.path(base_comparison, "profitability_bridge")
base_acquisition <- here::here("output", "airport_purchase_counterfactual")
base_legacy <- here::here("output", "airport_counterfactual")
manifest <- tibble::tibble(file = character(), theme = character(),
  status = character(), source = character(), interpretation = character())

input_csv <- function(path, required = TRUE) {
  if (!file.exists(path)) {
    if (required) stop("Missing input: ", path,
      "\nRun the regular production pipeline first.", call. = FALSE)
    return(NULL)
  }
  readr::read_csv(path, show_col_types = FALSE)
}
require_columns <- function(d, cols, name) {
  missing <- setdiff(cols, names(d))
  if (length(missing)) stop(name, " missing columns: ",
    paste(missing, collapse = ", "), call. = FALSE)
}
export <- function(d, name, theme, source, interpretation) {
  if (!nrow(d)) stop("No data for export: ", name, call. = FALSE)
  if (anyDuplicated(names(d))) stop("Duplicate column names: ", name)
  readr::write_csv(d, file.path(out, name), na = "")
  manifest <<- dplyr::bind_rows(manifest,
    tibble::tibble(file = name, theme = theme, status = "ready",
      source = source, interpretation = interpretation))
}
pending <- function(theme, source, interpretation) {
  manifest <<- dplyr::bind_rows(manifest,
    tibble::tibble(file = "", theme = theme, status = "external source needed",
      source = source, interpretation = interpretation))
}
check_keys <- function(d, cols, label) {
  require_columns(d, cols, label)
  if (anyNA(d[cols]) || anyDuplicated(d[cols]))
    stop("Missing/duplicate keys: ", label, call. = FALSE)
}

annual_abs <- input_csv(file.path(base_comparison,
  "comparison_annual_absolute_2023_2025.csv"))
annual_pp <- input_csv(file.path(base_comparison,
  "comparison_annual_pp_2023_2025.csv"))
annual_keys <- c("country", "airport", "year")
check_keys(annual_abs, annual_keys, "annual absolute")
check_keys(annual_pp, annual_keys, "annual per-passenger")
if (!identical(annual_abs[annual_keys], annual_pp[annual_keys]))
  stop("Annual comparison files have inconsistent row keys.", call. = FALSE)
if (!setequal(annual_abs$airport, all_airports))
  stop("Unexpected airport population in annual comparison.", call. = FALSE)

# 1. COMPARABILITY: size and revenue composition, same financial comparison
# basis as the published workbook. FY 2024-25 Australia versus CY 2025 Canada.
scale <- annual_abs |>
  filter(year == 2025L) |>
  select(all_of(annual_keys), total_revenue, total_aero,
    commercial_revenue, operating_expenses, ebitda) |>
  left_join(annual_pp |>
    filter(year == 2025L) |>
    select(all_of(annual_keys), revenue_per_passenger = total_revenue),
    by = annual_keys) |>
  mutate(passengers_m = total_revenue / revenue_per_passenger,
         aeronautical_share_pct = 100 * total_aero / total_revenue,
         ebitda_margin_pct = 100 * ebitda / total_revenue,
         reporting_basis = if_else(country == "Australia",
           "Financial year ending June 2025", "Calendar year 2025")) |>
  arrange(country, airport)
# The two per-passenger and aggregate files both express CAD; ratio is
# passengers / 1e6, i.e., millions of passengers.
if (nrow(scale) != 8L || any(!is.finite(scale$passengers_m)) ||
    any(scale$passengers_m <= 0))
  stop("Invalid 2025 comparison scale.", call. = FALSE)
export(scale, "01_airport_scale_2025.csv", "Canada–Australia comparability",
  "Comparison annual absolute and per-passenger CSVs",
  "Eight airports; passenger volume millions. Australian fiscal years end June; Canadian years end December. Not proof of identical airport markets.")

# BIG4 country aggregates: sum levels first, then recompute weighted
# shares/margins. Do not add percentage margins or average airport-level shares.
scale_big4 <- scale |>
  group_by(country) |>
  summarise(
    airport_group = "BIG4",
    passengers_m = sum(passengers_m),
    total_revenue = sum(total_revenue),
    total_aero = sum(total_aero),
    commercial_revenue = sum(commercial_revenue),
    operating_expenses = sum(operating_expenses),
    ebitda = sum(ebitda),
    .groups = "drop"
  ) |>
  mutate(
    aeronautical_share_pct = 100 * total_aero / total_revenue,
    ebitda_margin_pct = 100 * ebitda / total_revenue,
    revenue_per_passenger = total_revenue / passengers_m,
    reporting_basis = if_else(country == "Australia",
      "Financial year ending June 2025", "Calendar year 2025"))
if (nrow(scale_big4) != 2L || any(!is.finite(scale_big4$passengers_m)))
  stop("Invalid BIG4 comparison aggregate.", call. = FALSE)
export(scale_big4, "01b_big4_aggregate_2025.csv",
  "Canada–Australia comparability",
  "Aggregated from 2025 annual absolute and per-passenger comparisons",
  "Four-airport country totals; shares and margins computed from sums. Financial years differ across countries.")

# 2. ACTUAL 2015-25 REAL AERONAUTICAL YIELDS, with 2015 index.
# Canadian aeronautical series INCLUDES AIF; ACCC Australian series is
# aeronautical revenue. These are revenue proxies, not posted airport fees.
aero <- input_csv(file.path(base_comparison,
  "aero_revenue_growth_2015_2025.csv"))
require_columns(aero, c("country", "airport", "year", "variable",
  "per_passenger_cad_2025", "real_pp_index_2015"), "aero series")
check_keys(aero, c("country", "airport", "year"), "aero series")
export(aero |>
  mutate(series = if_else(country == "Canada",
    "Aeronautical revenue incl. AIF", "Aeronautical revenue"),
    year_basis = if_else(country == "Australia",
      "June-ending financial year", "Calendar year")) |>
  arrange(country, airport, year),
  "02_actual_aeronautical_yield_2015_2025.csv",
  "Australian post-privatization revenue trends",
  "Comparison aeronautical revenue growth CSV",
  "Real 2025 CAD per passenger; 2015=100 indexes. Not a causal privatization estimate or a direct posted-fee series.")

# 3. CURRENT FINANCIALS: keep consistent measures and units in a long table.
financial_measures <- c(
  total_revenue = "Revenue",
  total_aero = "Aeronautical revenue",
  commercial_revenue = "Commercial revenue",
  operating_expenses = "Operating expenses",
  ebitda = "EBITDA",
  ebit = "EBIT",
  profit_before_tax = "Pre-tax profit",
  net_income = "Net income",
  ebitda_margin_pct = "EBITDA margin",
  ebit_margin_pct = "EBIT margin",
  net_profit_margin_pct = "Net profit margin")
financial <- annual_abs |>
  filter(year == 2025L) |>
  select(all_of(annual_keys), all_of(names(financial_measures))) |>
  pivot_longer(all_of(names(financial_measures)),
    names_to = "metric", values_to = "value") |>
  mutate(metric_label = unname(financial_measures[metric]),
    unit = if_else(grepl("_pct$", metric), "percent", "2025 CAD millions"),
    reporting_basis = if_else(country == "Australia",
      "FY 2024-25", "CY 2025")) |>
  select(country, airport, year, reporting_basis, metric,
    metric_label, value, unit)
export(financial, "03_current_financials_2025.csv",
  "Current Canadian–Australian airport financials",
  "Annual absolute financial comparison CSV",
  "Operating margin is EBITDA/revenue, not after-capital net profit. Australia FY versus Canada CY; source-specific accounting limitations remain.")

# 4. AUSTRALIAN ACCOUNTING TAX: avoid representing expenses as cash paid.
tax <- annual_abs |>
  filter(country == "Australia", year %in% 2023:2025) |>
  select(country, airport, year, profit_before_tax,
    accounting_income_tax_expense, net_income, total_revenue) |>
  mutate(effective_accounting_tax_rate_pct = if_else(
    is.finite(profit_before_tax) & profit_before_tax > 0 &
      is.finite(accounting_income_tax_expense),
    100 * accounting_income_tax_expense / profit_before_tax, NA_real_),
    reporting_basis = "Mixed FY reporting; see model source notes")
export(tax, "04_australia_accounting_tax_2023_2025.csv",
  "Australian airport taxation",
  "Financial comparison: regulatory accounts; Sydney SCACH consolidated below-operating results",
  "Accounting income-tax EXPENSE, not cash corporate tax paid. Effective rates can be distorted by deferred tax, losses and differing corporate/reporting entities.")
pending("Australian airport taxation",
  "ATO corporate tax transparency / company financial statements",
  "To investigate cash tax paid, taxable income, deductions, related-party finance and corporate entity scope, prepare a separately sourced entity-year crosswalk; do not infer avoidance from accounting expenses alone.")

# 5. FINANCING CASES: assumptions and matching 2025 outcomes.
assumptions <- input_csv(file.path(base_acquisition, "scenario_assumptions.csv"))
export(assumptions, "05_acquisition_assumptions.csv",
  "Three acquisition scenarios",
  "Acquisition model scenario_assumptions.csv",
  "Model inputs for low-cost, central and high-cost acquisitions; assumptions are illustrations, not market quotes.")
scenarios <- input_csv(file.path(base_acquisition, "scenario_comparison.csv"))
require_columns(scenarios,
  c("airport", "period", "metric", "low_cost", "central", "high_cost"),
  "acquisition scenarios")
export(scenarios |>
  filter(period == "2025") |>
  pivot_longer(c(low_cost, central, high_cost),
    names_to = "financing", values_to = "value") |>
  mutate(financing = recode(financing,
    low_cost = "Low-cost", central = "Central", high_cost = "High-cost")),
  "06_acquisition_scenarios_2025.csv",
  "Three acquisition scenarios",
  "Acquisition model scenario_comparison.csv",
  "2025 acquisition sensitivity; verify units by metric in source model. Does not contain a prediction of actual sale price.")

# 6. PROFITABILITY TARGETS: 2025 x airport x scenario x benchmark.
bridge <- input_csv(file.path(base_bridge,
  "profitability_bridge_scenarios_long.csv"))
require_columns(bridge, c("airport", "year_label", "scenario_name",
  "profitability_objective", "benchmark_mode",
  "acquisition_required_revenue_m", "acquisition_required_revenue_per_pax_cad",
  "benchmark_required_revenue_m", "additional_revenue_gap_m"),
  "profitability benchmark export")
target <- bridge |>
  filter(year_label == "2025") |>
  select(airport, year_label, scenario_name, threshold_id,
    profitability_objective, benchmark_mode, passengers,
    acquisition_required_revenue_m,
    acquisition_required_revenue_per_pax_cad,
    benchmark_required_revenue_m, benchmark_required_revenue_per_pax_cad,
    additional_revenue_gap_m, additional_revenue_gap_per_pax_cad,
    observed_revenue_m, counterfactual_revenue_m,
    observed_ebitda_margin_pct, counterfactual_ebitda_margin_pct,
    achieved_net_margin, target_net_margin)
check_keys(target, c("airport", "year_label", "scenario_name",
  "profitability_objective", "benchmark_mode"), "2025 profitability targets")
export(target, "07_profitability_targets_2025.csv",
  "Final profitability thresholds",
  "Profitability bridge scenarios long CSV",
  "2025 values include each airport and BIG4; independently derived pooled and year-matched Australian benchmarks. All dollar figures 2025 CAD; totals in millions.")

# 7. ALLOCATION OF ADDITIONAL REVENUE: existing well-validated long panel.
impacts <- input_csv(file.path(base_bridge, "revenue_impacts_2025_long.csv"))
require_columns(impacts, c("airport", "period", "financing",
  "profitability_objective", "allocation_method", "revenue_category",
  "observed_category_m", "additional_category_m",
  "counterfactual_category_m", "increase_pct"), "revenue impacts")
if (nrow(impacts) != 1080L) stop("Expected 1080 revenue-impact category records.")
export(impacts, "08_revenue_impacts_2025.csv",
  "Final profitability thresholds and revenue allocation",
  "Revenue impacts 2025 long CSV",
  "2025; category uplift is NOT a fee/passenger-incidence forecast. Total and three components must not be added together.")

# 8. OBSERVED HISTORICAL ACCC PRICE-CAP PERIOD.
# The legacy input contains ACCC Figure 2.2 and 1998 FAC Sydney values.
# It is NOMINAL AUD, not deflated: export as traceable observations but do not
# join the levels directly to modern 2025-CAD airport results.
historical_observed_path <- here::here("output",
  "accc_legacy_vector_selected.csv")
historical_observed <- input_csv(historical_observed_path, required = FALSE)
if (is.null(historical_observed)) {
  pending("Australia observed 1998–2007 path",
    "ACCC Figure 2.2 / FAC historical digitization",
    "Optional source: output/accc_legacy_vector_selected.csv. Original nominal AUD observations require fiscal-year CPI conversion before displaying a real growth index.")
} else {
  require_columns(historical_observed,
    c("airport", "year", "chart", "measure", "adjustment", "value"),
    "observed historical ACCC source")
  observed_yield <- historical_observed |>
    filter(airport %in% aus_airports, year %in% 1998:2007,
      as.character(chart) == "2.2",
      measure == "aero_revenue_per_pax",
      adjustment %in% c("ansett_adjusted", "adjusted") |
        (airport == "SYD" & year == 1998L &
           adjustment == "FAC_reported")) |>
    transmute(airport, australian_fy_end = year,
      nominal_aud_per_passenger = as.numeric(value),
      adjustment,
      regulatory_period = if_else(year <= 2002L,
        "Price-cap period", "Price monitoring after cap removal"))
  check_keys(observed_yield,
    c("airport", "australian_fy_end"), "observed ACCC history")
  if (nrow(observed_yield) != 40L)
    stop("Expected 40 historical Australian airport-year records.")
  export(observed_yield, "09_observed_nominal_australia_1998_2007.csv",
    "Australia observed 1998–2007 path",
    "ACCC 2006-07 Figure 2.2 and FAC Sydney 1998; original digitization",
    "NOMINAL AUD per passenger, not real growth. Price caps ceased in 2002. Sydney 1998 FAC figure is not directly Ansett-adjusted; do not infer a causal shock.")
}

# 9. OPTIONAL HISTORY: historical cap-then-shock and immediate shock paths.
# This older model is OFF by default in run_pipeline.R and should be run
# separately before creating these exports.
legacy_path <- file.path(base_legacy, "scenario_growth_paths.csv")
legacy <- input_csv(legacy_path, required = FALSE)
if (is.null(legacy)) {
  pending("Australia cap-then-shock path",
    "Legacy airport_revenue_counterfactual.R output",
    "Optional: run the separate historical-growth counterfactual, then rerun these exports. Do not confuse modelled Canadian paths with observed Australian history.")
} else {
  require_columns(legacy, c("scenario"), "historical growth paths")
  export(legacy, "09_legacy_cap_then_shock_paths.csv",
    "Australia cap-then-shock path",
    "Legacy airport_revenue_counterfactual.R scenario_growth_paths.csv",
    "ILLUSTRATIVE paths mapped to Canadian years; check observed vs extrapolated flags. Not observed Australian financial data.")
}

# 9. OPTIONAL QUALITY: preserve exact ACCC survey series and classification.
quality_path <- here::here("data", "australia",
  "accc_monitor_data_2005_2025.rds")
if (file.exists(quality_path)) {
  parsed <- readRDS(quality_path)
  if (!is.list(parsed) || is.null(parsed$airport_quality))
    stop("ACCC RDS lacks airport_quality.")
  quality <- parsed$airport_quality
  require_columns(quality, c("airport", "year", "source", "terminal",
    "category", "measure", "value"), "ACCC quality")
  export(quality |>
    filter(airport %in% aus_airports) |>
    arrange(airport, year, source, terminal, category, measure),
    "10_australia_quality_series.csv",
    "Australia quality and revenue",
    "ACCC supplementary database airport_quality extract",
    "Raw measure-specific quality series. Do NOT pool airline and passenger ratings, terminals or categories without a defensible common indicator.")
} else {
  pending("Australia quality and revenue",
    "ACCC supplementary database airport_quality extract",
    "Optional: run parse_accc_airports.R. Choose a consistent overall-quality measure before joining quality ratings to revenue or drawing averages.")
}
pending("Canada–Australia comparison validation",
  "Statistics Canada / BITRE passenger throughput; airport annual reports",
  "Current output provides traffic scale and revenue mix. Additional traffic mix (international, domestic, transfers), cargo, catchment and airline market structure require separately sourced harmonized definitions.")

readr::write_csv(manifest, file.path(out, "00_data_catalog.csv"))
message("Datawrapper export directory: ", out)
message("Ready: ", sum(manifest$status == "ready"),
  "; source gaps / optional stages: ",
  sum(manifest$status != "ready"))
