# Datawrapper presentation exports | independent of workbook formatting
# Run after the production pipeline:
#   source(here::here("r", "prepare_datawrapper_exports.R"))
# Does NOT rebuild model inputs or alter acquisition assumptions.
# Exports are tidy CSVs; monetary comparisons use real 2025 CAD.
# Original nominal tax amounts and historical source yields are retained for audit.
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

# COUNTRY-LEVEL COMPARABILITY: fixed-source, publishable context tables.
# Maintain bibliographic URLs and qualification notes in data/comparability/.
# These country and airline figures DO NOT share the airport-financial
# reporting conventions; keep years and definitions next to each observation.
context_dir <- here::here("data", "comparability")
country_context <- input_csv(file.path(context_dir, "country_context_2025.csv"))
require_columns(country_context, c("country", "population_2025",
  "land_area_km2", "population_density_2025_per_km2",
  "gdp_2025_usd_trillion_rounded", "gdp_per_capita_2025_usd",
  "source_population", "source_land_area", "source_gdp"),
  "country-level context")
check_keys(country_context, "country", "country context")
if (!setequal(country_context$country, c("Canada", "Australia")) ||
    any(abs(country_context$population_2025 / country_context$land_area_km2 -
      country_context$population_density_2025_per_km2) > 0.0001))
  stop("Invalid population/land-area/density comparability inputs.")
export(country_context, "00a_country_context_2025.csv",
  "Canada–Australia country comparability",
  "World Bank population, land area and 2025 national accounts; URLs recorded in rows",
  "Land area (not total including lakes); midyear population; nominal 2025 GDP in USD. GDP in trillions is rounded to 0.1 and is not a precision estimate.")

airlines <- input_csv(file.path(context_dir,
  "domestic_airline_capacity_2022.csv"))
require_columns(airlines, c("country", "airline_group",
  "domestic_seat_capacity_share_pct", "year", "measurement_period", "source"),
  "harmonized airline shares")
check_keys(airlines, c("country", "airline_group"), "airline shares")
airline_totals <- airlines |>
  group_by(country) |>
  summarise(total = sum(domestic_seat_capacity_share_pct),
    .groups = "drop")
if (nrow(airlines) != 6L || nrow(airline_totals) != 2L ||
    any(abs(airline_totals$total - 100) > 0.01) ||
    any(airlines$year != 2022L))
  stop("Airline domestic seat-capacity comparisons fail checks.")
export(airlines, "00b_domestic_airline_capacity_2022.csv",
  "Canada–Australia airline market structure",
  "ACCC June 2022 airline competition report, Figure 11",
  "Same scheduled domestic seat capacity measure in both countries for week of 27 June 2022. Shares are grouped by two major firms vs all other carriers. Not 2025 market shares.")
airline_top2 <- airlines |>
  filter(airline_group != "Other carriers") |>
  group_by(country) |>
  summarise(two_largest_groups_share_pct =
    sum(domestic_seat_capacity_share_pct), year = first(year),
    .groups = "drop")
export(airline_top2, "00c_top_two_airline_share_2022.csv",
  "Canada–Australia airline market structure",
  "ACCC June 2022 airline competition report, Figure 11",
  "Harmonized two-largest-airline-group shares of domestic scheduled seat capacity for a single week in 2022. Not directly comparable to more recent Australian passenger-share measures.")

# Updated domestic seat capacity: matched CAPA/OAG week in 2024, plus
# Transport Canada's 2025 annual figures as a separate context observation.
# Retain source rounding; do not normalize the reported shares to 100.
# Consolidate Qantas and Jetstar by ownership, not by brand. "Other"
# represents multiple carriers, never a single firm in concentration ratios.
airlines_recent <- input_csv(file.path(context_dir,
  "domestic_airline_capacity_2024_2025.csv"))
require_columns(airlines_recent, c("country", "airline", "airline_group",
  "named_group_rank", "domestic_seat_capacity_share_pct", "year",
  "measurement_period", "source", "notes"), "recent domestic airline shares")
check_keys(airlines_recent, c("country", "year", "airline"), "recent airline brands")
recent_totals <- airlines_recent |>
  group_by(country, year) |>
  summarise(total = sum(domestic_seat_capacity_share_pct), .groups = "drop")
if (nrow(airlines_recent) != 19L || nrow(recent_totals) != 3L ||
    anyNA(airlines_recent$domestic_seat_capacity_share_pct) ||
    any(airlines_recent$domestic_seat_capacity_share_pct < 0) ||
    any(abs(recent_totals$total - 100) > 1.01))
  stop("Invalid recent domestic seat capacity shares.")
export(airlines_recent, "00e_domestic_airline_capacity_brands_2024_2025.csv",
  "Domestic airline market structure, reported brands",
  "CAPA/OAG 26 August 2024 charts and Transport Canada 2025 annual report",
  "2024 same scheduled domestic seat-capacity week for both countries; 2025 Canada annual only. Published rounding retained; 2024 totals 99.9%, 2025 Canada 99%. Qantas and Jetstar are separate brands but one group.")
airline_groups_recent <- airlines_recent |>
  group_by(country, year, measurement_period, source, airline_group,
    named_group_rank) |>
  summarise(domestic_seat_capacity_share_pct =
    sum(domestic_seat_capacity_share_pct), .groups = "drop") |>
  arrange(year, country, named_group_rank)
check_keys(airline_groups_recent, c("country", "year", "airline_group"),
  "recent airline group shares")
export(airline_groups_recent, "00f_domestic_airline_capacity_groups_2024_2025.csv",
  "Domestic airline market structure, ownership groups",
  "CAPA/OAG 26 August 2024 charts and Transport Canada 2025 annual report",
  "Qantas Airways plus Jetstar consolidated; other carriers retained as an aggregate residual, not a firm. Compare matched August 2024 observations across countries; Canadian 2025 annual observation is supplementary.")
airline_concentration_recent <- airline_groups_recent |>
  group_by(country, year, measurement_period, source) |>
  summarise(
    top_two_share_pct = sum(domestic_seat_capacity_share_pct[
      !is.na(named_group_rank) & named_group_rank <= 2L]),
    top_four_named_share_pct = sum(domestic_seat_capacity_share_pct[
      !is.na(named_group_rank) & named_group_rank <= 4L]),
    reported_total_pct = sum(domestic_seat_capacity_share_pct),
    .groups = "drop")
export(airline_concentration_recent, "00g_domestic_airline_concentration_2024_2025.csv",
  "Domestic airline concentration",
  "Grouped CAPA/OAG and Transport Canada scheduled domestic seat shares",
  "Sum of two or four largest individually named airline groups in the source; not normalized. Other carriers are not a firm. Top-four named share may not be exact CR4 if an undisclosed carrier exceeds a named smaller carrier. Matched 2024 week is preferred for cross-country comparison.")

policy <- input_csv(file.path(context_dir, "policy_context.csv"))
require_columns(policy, c("topic", "country", "statement",
  "source", "interpretation"), "policy context")
check_keys(policy, c("topic", "country"), "policy context")
export(policy, "00d_policy_context.csv",
  "Canada–Australia policy and institutional context",
  "Transport Canada, Australian Productivity Commission, ACCC and parliamentary sources; URLs in rows",
  "Qualitative source notes; selected institutional analogies and essential differences, not quantified variables.")

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
  interest_financing_costs = "Financing costs",
  profit_before_tax = "Pre-tax profit",
  net_income = "Net income",
  cash_capital_investment = "Cash capital investment",
  shareholder_payouts = "Shareholder payouts",
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
    reporting_basis = case_when(
      airport == "SYD" & metric %in% c("interest_financing_costs", "profit_before_tax", "net_income", "net_profit_margin_pct", "cash_capital_investment", "shareholder_payouts") ~ "CY 2025 (SCACH)",
      country == "Australia" ~ "FY 2024-25", TRUE ~ "CY 2025")) |>
  select(country, airport, year, reporting_basis, metric,
    metric_label, value, unit)
# Supplementary parking yield: GTAA 2024 Annual Report, p. 45,
# estimates enplanements for credit metrics as half of total passengers.
# Keep total-passenger rates for additive comparisons and use estimated
# enplanements only for standalone parking/ground transportation yields.
parking_enplanement_share <- 0.5
require_columns(annual_pp, "parking_transport_revenue", "annual per-passenger")
require_columns(annual_abs, "parking_transport_revenue", "annual absolute")
parking_annual <- annual_abs |>
  select(all_of(annual_keys),
    parking_transport_revenue_m = parking_transport_revenue) |>
  left_join(annual_pp |>
    select(all_of(annual_keys),
      parking_transport_revenue_per_pax = parking_transport_revenue),
    by = annual_keys) |>
  mutate(
    reporting_basis = if_else(country == "Australia",
      sprintf("FY %d-%02d", year - 1L, year %% 100L),
      paste("CY", year)),
    parking_transport_revenue_per_est_enplaned_pax =
      parking_transport_revenue_per_pax / parking_enplanement_share)
check_keys(parking_annual, annual_keys, "annual parking yield")
export(parking_annual, "03b_parking_yield_2023_2025.csv",
  "Current Canadian–Australian airport parking and ground transportation",
  "Comparison annual absolute and per-passenger CSVs",
  "2025 CAD millions and CAD per total or estimated enplaned passenger. Enplanements estimated as 50% of total passengers; this is a revenue yield, not a price paid by parking customers. Do not add enplanement-based yields to total-passenger revenue components. Australia June-ending FY; Canada CY.")

parking_yield <- parking_annual |>
  filter(year == 2025L) |>
  transmute(country, airport, year, reporting_basis,
    metric = "parking_transport_revenue_per_enplaned",
    metric_label = "Parking and ground transportation revenue per estimated enplaned passenger",
    value = parking_transport_revenue_per_est_enplaned_pax,
    unit = "2025 CAD per estimated enplaned passenger")
financial <- bind_rows(financial, parking_yield)
export(financial, "03_current_financials_2025.csv",
  "Current Canadian–Australian airport financials",
  "Annual absolute and per-passenger financial comparison CSVs",
  "Operating margin is EBITDA/revenue, not after-capital net profit. Sydney net profit is adjusted for RPS remuneration and redemption loss; payouts include ordinary dividends and cash RPS interest. BNE/MEL/PER payouts are ACCC reported dividends. Cash investment is observed cash outlay, not depreciation; intangible assets may be bundled in Canadian inputs. Australia FY versus Canada CY; Sydney below-operating measures and cash investment use SCACH CY. Parking/ground transportation yield uses estimated enplanements (50% of total passengers), following GTAA 2024 Annual Report p. 45; not a parking price or observed enplanement count.")

# 3b. FULL HARMONIZED FINANCIAL PANEL: annual and pooled, airport and BIG4.
# Preserve the existing 2025 export for compatibility. Use the upstream
# audit for actual passenger counts and the matching margin denominators
# (particularly SCACH consolidated revenue for Sydney net income).
full_financial_measures <- c(financial_measures,
  aero_excl_aif = "Aeronautical revenue excluding AIF",
  aif_revenue = "Airport improvement fees",
  parking_transport_revenue = "Parking and ground transportation revenue",
  non_aero_minus_parking = "Non-aeronautical revenue excluding parking and ground transportation",
  labour = "Labour costs",
  purchased_services_other = "Purchased services and other costs",
  federal_ground_rent = "Federal ground rent",
  pilt = "Payments in lieu of taxes",
  depreciation_amortisation = "Depreciation and amortisation",
  asset_impairment = "Asset impairment",
  abnormal_items = "Abnormal items",
  accounting_income_tax_expense = "Accounting income tax expense")
full_metrics <- names(full_financial_measures)
margin_metrics <- full_metrics[grepl("_pct$", full_metrics)]
money_metrics <- setdiff(full_metrics, margin_metrics)
pooled_keys <- c("country", "airport")
pooled_abs <- input_csv(file.path(base_comparison,
  "comparison_pooled_absolute_2023_2025.csv"))
pooled_pp <- input_csv(file.path(base_comparison,
  "comparison_pooled_pp_2023_2025.csv"))

check_financial_pair <- function(a, p, keys, expected_n, label) {
  for (d in list(a, p)) {
    check_keys(d, keys, label)
    require_columns(d, full_metrics, label)
    if (nrow(d) != expected_n ||
        !setequal(setdiff(names(d), keys), full_metrics))
      stop("Unexpected financial coverage: ", label)
  }
  if (!identical(a[keys], p[keys]) ||
      !isTRUE(all.equal(a[margin_metrics], p[margin_metrics],
                        tolerance = 1e-10)))
    stop("Financial companion tables disagree: ", label)
}
check_financial_pair(annual_abs, annual_pp, annual_keys, 24L, "annual financials")
check_financial_pair(pooled_abs, pooled_pp, pooled_keys, 8L, "pooled financials")
if (!setequal(annual_abs$year, 2023:2025) ||
    !setequal(pooled_abs$airport, all_airports) ||
    any(table(annual_abs$airport) != 3L))
  stop("Unexpected annual or pooled financial coverage.")

comparison_audit <- readRDS(file.path(base_comparison,
  "financial_comparison_audit_2023_2025.rds"))
financial_basis <- comparison_audit$financial_source_annual
require_columns(financial_basis, c(annual_keys, "passengers",
  "adjustment_to_2025", "fx_cad_per_unit", "ebitda_ebit_revenue_basis",
  "net_income_revenue_basis"), "financial comparison audit")
check_keys(financial_basis, annual_keys, "financial comparison audit")
# Reuse the exact fixed FX and fiscal-year CPI conventions of production.
cad_per_aud_2025 <- unique(financial_basis$fx_cad_per_unit[financial_basis$country == "Australia"])
if (length(cad_per_aud_2025) != 1L || !is.finite(cad_per_aud_2025) ||
    cad_per_aud_2025 <= 0)
  stop("Expected one positive fixed Australian exchange rate in financial audit.")
presentation_aus_cpi <- comparison_audit$aus_cpi |>
  select(year, adjustment_to_2025)
check_keys(presentation_aus_cpi, "year", "presentation Australian CPI")
if (!setequal(presentation_aus_cpi$year, 2015:2025) ||
    any(!is.finite(presentation_aus_cpi$adjustment_to_2025) |
        presentation_aus_cpi$adjustment_to_2025 <= 0))
  stop("Invalid Australian CPI factors in production audit.")
basis_annual <- financial_basis |>
  transmute(country, airport, year, passengers_m = passengers / 1e6,
    operating_revenue_basis_m = ebitda_ebit_revenue_basis *
      adjustment_to_2025 * fx_cad_per_unit,
    net_revenue_basis_m = net_income_revenue_basis *
      adjustment_to_2025 * fx_cad_per_unit)
# Missing or inapplicable measures remain NA; never silently sum a partial
# set of airports or treat an unavailable accounting line as zero.
complete_sum <- function(x) {
  if (all(is.finite(x))) sum(x) else NA_real_
}
basis_pooled <- basis_annual |>
  group_by(country, airport) |>
  summarise(across(c(passengers_m, operating_revenue_basis_m,
    net_revenue_basis_m), complete_sum), .groups = "drop")

financial_panel <- function(a, p, basis, keys, period_label) {
  airport_abs <- a |>
    select(all_of(keys), all_of(full_metrics)) |>
    left_join(basis, by = keys)
  if (any(!is.finite(airport_abs$passengers_m)) ||
      any(airport_abs$passengers_m <= 0))
    stop("Missing or invalid financial passenger denominators.")
  for (nm in money_metrics) {
    if (!isTRUE(all.equal(airport_abs[[nm]] / airport_abs$passengers_m,
                         p[[nm]], tolerance = 1e-7)))
      stop("Financial passenger denominator mismatch: ", nm)
  }
  country_keys <- setdiff(keys, "airport")
  if (any((airport_abs |> count(across(all_of(country_keys))))$n != 4L))
    stop("Expected four airports per country-period.")
  big4_abs <- airport_abs |>
    group_by(across(all_of(country_keys))) |>
    summarise(across(all_of(c(money_metrics, "passengers_m",
      "operating_revenue_basis_m", "net_revenue_basis_m")), complete_sum),
      .groups = "drop") |>
    mutate(airport = "BIG4",
      ebitda_margin_pct = if_else(operating_revenue_basis_m > 0,
        100 * ebitda / operating_revenue_basis_m, NA_real_),
      ebit_margin_pct = if_else(operating_revenue_basis_m > 0,
        100 * ebit / operating_revenue_basis_m, NA_real_),
      net_profit_margin_pct = if_else(net_revenue_basis_m > 0,
        100 * net_income / net_revenue_basis_m, NA_real_))
  big4_pp <- big4_abs |>
    mutate(across(all_of(money_metrics), ~ .x / passengers_m))
  abs_all <- bind_rows(airport_abs, big4_abs)
  pp_all <- bind_rows(p |> left_join(basis, by = keys), big4_pp)
  to_long <- function(d, measures, unit_label) {
    d |>
      select(all_of(keys), passengers_m, all_of(measures)) |>
      pivot_longer(all_of(measures), names_to = "metric", values_to = "value") |>
      mutate(unit = unit_label)
  }
  result <- bind_rows(
    to_long(abs_all, money_metrics, "2025 CAD millions"),
    to_long(pp_all, money_metrics, "2025 CAD per passenger"),
    to_long(abs_all, margin_metrics, "percent"),
    to_long(pp_all, "parking_transport_revenue",
      "2025 CAD per estimated enplaned passenger") |>
      mutate(value = value / parking_enplanement_share))
  if (!"year" %in% keys) result$year <- NA_integer_
  result |>
    mutate(period = if_else(is.na(year), period_label, as.character(year)),
      aggregation = if_else(airport == "BIG4", "country BIG4", "airport"),
      metric_label = unname(full_financial_measures[metric]),
      reporting_basis = case_when(
        country == "Canada" ~ "CY",
        airport %in% c("SYD", "BIG4") & metric %in% c(
          "interest_financing_costs", "profit_before_tax",
          "accounting_income_tax_expense", "net_income", "net_profit_margin_pct",
          "cash_capital_investment", "shareholder_payouts") ~
          if_else(airport == "SYD", "CY (SCACH)", "Mixed FY / CY (SCACH)"),
        TRUE ~ "June-ending FY")) |>
    select(country, airport, aggregation, period, year, reporting_basis,
      passengers_m, metric, metric_label, unit, value)
}

full_financial <- bind_rows(
  financial_panel(annual_abs, annual_pp, basis_annual, annual_keys, "annual"),
  financial_panel(pooled_abs, pooled_pp, basis_pooled, pooled_keys, "2023-2025")) |>
  arrange(country, airport, period, metric, unit)
check_keys(full_financial, c("country", "airport", "period", "metric", "unit"),
  "full financial presentation panel")
if (nrow(full_financial) != 40L * (2L * length(money_metrics) +
    length(margin_metrics) + 1L))
  stop("Unexpected full financial presentation panel size.")
export(full_financial, "03c_full_financials_2023_2025.csv",
  "Current Canadian–Australian airport financials, full annual and pooled panel",
  "Annual and pooled comparison CSVs; financial comparison audit passenger and margin bases",
  "All published harmonized metrics; monetary amounts in real 2025 CAD. Annual and pooled 2023-2025; pooled absolute figures are three-year sums, not annual averages. BIG4 yields and margins are ratios of sums, not means of airport ratios. Missing or inapplicable inputs remain NA, including country totals without complete coverage. Sydney below-operating results and cash investment use SCACH CY; Australian BIG4 combines June FY and Sydney CY for those metrics. Per-passenger rates use the upstream passenger series, including Sydney FY passenger counts for SCACH CY metrics. Parking alternative yield estimates enplanements as half of total passengers and must not be stacked with total-passenger rates. Metrics overlap; not all are additive. Profitability is whole-airport, not segment-specific.")

# 4. AUSTRALIAN AIRPORT TAXATION: two already assembled source panels.
# Tax figures here are source AUD, not 2025 CAD. Do not equate
# ATO tax payable with ACCC cash-flow tax paid or accounting tax expense.
tax_dir <- here::here("output", "taxation")
tax_rates <- input_csv(file.path(tax_dir, "aus_airport_tax_rates.csv"),
                       required = FALSE)
if (is.null(tax_rates)) {
  pending("Australian airport cash taxation",
    "airport_tax_with_cash.R -> output/taxation/aus_airport_tax_rates.csv",
    "Run the existing airport_tax_with_cash.R analysis, then rerun presentation exports. ACCC cash-flow tax paid, ATO tax payable, and accounting tax expense are distinct concepts.")
} else {
  tax_chart_cols <- c("airport", "year_end",
    "reported_ebt", "adjusted_ebt", "ato_taxable_income",
    "adjusted_ebt_less_taxable_income",
    "reported_tax_expense", "expected_tax_on_adjusted_ebt",
    "ato_tax_payable", "reported_cash_tax_paid",
    "statutory_tax_rate", "tax_expense_to_reported_ebt",
    "tax_payable_to_taxable_income", "cash_tax_to_adjusted_ebt",
    "expected_tax_less_tax_payable", "expected_tax_less_cash_tax",
    "zero_cash_tax_assumed")
  require_columns(tax_rates, c("fyear", tax_chart_cols), "Australian airport tax rates")
  check_keys(tax_rates, c("airport", "fyear"), "Australian airport tax rates")
  export(tax_rates |>
      select(all_of(tax_chart_cols)) |>
      arrange(airport, year_end),
    "04_australia_tax_cash_accounting_2016_2025.csv",
    "Australian airport taxation",
    "airport_tax_with_cash.R: ACCC regulatory accounts + PADG and SCACH inputs + ATO tax transparency",
    "Nominal AUD. BNE/MEL use ACCC regulatory EBT and cash taxes. PER uses consolidated PADG EBT excluding signed financing valuation movements and consolidated cash taxes. Accounting ETR uses reported EBT; book-to-tax difference and payable gap use adjusted EBT. ATO nonpositive income is a zero proxy; scope and timing differences mean these are not measured deductions. SYD uses adjusted SCACH calendar-year EBT (RPS interest, RPS debt-cost amortisation and exceptional redemption loss added back); the illustrative gap assumes zero corporate cash tax, while observed Sydney cash taxes and cash ETR remain NA. Investor-level taxes are outside scope. ATO payable and taxable income are separate tax-return diagnostics. ACCC cash payments may reflect regulatory entity scope or tax-sharing arrangements; not verified ultimate-parent tax remittances.")
}
tax_periods <- input_csv(file.path(tax_dir, "aus_airport_tax_periods.csv"),
                          required = FALSE)
if (!is.null(tax_periods)) {
  period_chart_cols <- c("airport", "period",
    "reported_ebt", "adjusted_ebt", "ato_taxable_income",
    "adjusted_ebt_less_taxable_income",
    "reported_tax_expense", "expected_tax_on_adjusted_ebt",
    "ato_tax_payable", "reported_cash_tax_paid",
    "statutory_tax_rate", "tax_expense_to_reported_ebt",
    "tax_payable_to_taxable_income", "cash_tax_to_adjusted_ebt",
    "expected_tax_less_tax_payable", "expected_tax_less_cash_tax",
    "years_zero_cash_tax_assumed")
  require_columns(tax_periods, period_chart_cols, "Australian airport tax periods")
  check_keys(tax_periods, c("airport", "period"), "Australian airport tax periods")
  export(tax_periods |> select(all_of(period_chart_cols)),
    "04c_australia_tax_periods.csv",
    "Australian airport taxation",
    "airport_tax_with_cash.R: period totals and ratios",
    "Nominal AUD. Five-year ratios of sums, signed EBT with losses offsetting profits. BNE/MEL gap = 30% of ACCC regulatory EBT less reported ACCC cash taxes. PER uses adjusted PADG EBT and consolidated cash taxes. Accounting ETR uses reported EBT; book-to-tax difference and payable gap use adjusted EBT. ATO nonpositive income is a zero proxy. Sydney hypothetical gap = 30% of adjusted SCACH EBT less assumed zero corporate cash tax; actual Sydney cash taxes/ETR remain NA. No investor-level tax adjustment. Period 2015-2020 means years ending 2016-2020, including FY2019-20; SYD uses calendar years, others June fiscal years. Missing observed cash propagates unless the explicit Sydney scenario is enabled.")
}
ato_airport <- input_csv(file.path(tax_dir,
  "ato_airport_tax_2013_2025.csv"), required = FALSE)
if (is.null(ato_airport)) {
  pending("Australian tax transparency: entity detail",
    "airport_tax_with_cash.R -> output/taxation/ato_airport_tax_2013_2025.csv",
    "Run existing tax script to make entity-year ATO turnover, taxable income and tax payable available for visualizations.")
} else {
  require_columns(ato_airport,
    c("airport", "company", "abn", "fyear", "year_end",
      "total_income", "taxable_income", "tax_payable"),
    "ATO airport entities")
  check_keys(ato_airport, c("abn", "fyear"), "ATO airport entities")
  export(ato_airport, "04b_australia_ato_entities_2013_2025.csv",
    "Australian airport taxation",
    "airport_tax_with_cash.R: ATO corporate tax transparency workbooks",
    "ATO company entities do not always coincide with airport operating-company or group consolidated boundaries; SYD reporting entity changes in 2022-23. Non-positive taxable income and tax payable blanks are represented by zero with flags; zero taxable income is not a measured loss.")
}

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

# 5b. Standalone valuation and financing bridge, independent of profit targets.
# Annual source amounts are nominal; pooled amounts are already in 2025 CAD.
# Stock amounts are not divided by years_in_period. Pooled EBITDA and rent
# flows are annualized for valuation, and incremental interest for display.
acquisition_source <- input_csv(file.path(base_acquisition,
  "acquisition_scenarios.csv"))
acquisition_money <- c("ebitda_m", "ground_rent_m", "rent_adjusted_ebitda_m",
  "valuation_ebitda_m", "estimated_sale_value_m", "gross_debt_m", "cash_m",
  "equity_purchase_consideration_m", "new_acquisition_debt_m",
  "buyer_equity_contribution_m", "total_gross_debt_m",
  "net_financing_cost_m", "additional_acquisition_interest_m")
acquisition_keys <- c("airport", "year_label", "scenario_id")
acquisition_parameters <- c("purchase_multiple", "acquisition_debt_share",
  "acquisition_interest_rate")
require_columns(acquisition_source, c(acquisition_keys, "year", "period",
  "years_in_period", "cpi_factor", "passengers",
  acquisition_parameters, acquisition_money,
  "actual_total_gross_debt_to_sale_value"), "standalone acquisition source")
require_columns(assumptions, c("scenario_id", "scenario_name",
  acquisition_parameters), "acquisition assumptions")
check_keys(assumptions, "scenario_id", "acquisition assumptions")
check_keys(acquisition_source, acquisition_keys, "standalone acquisition source")
acquisition <- acquisition_source |>
  select(all_of(acquisition_keys), year, period, years_in_period,
    cpi_factor, passengers, all_of(acquisition_parameters),
    all_of(acquisition_money), actual_total_gross_debt_to_sale_value) |>
  left_join(assumptions |>
    select(scenario_id, scenario_name, all_of(acquisition_parameters)),
    by = c("scenario_id", acquisition_parameters)) |>
  mutate(year_label = as.character(year_label))
if (nrow(acquisition) != 60L ||
    !setequal(acquisition$airport, c(can_airports, "BIG4")) ||
    !setequal(acquisition$year_label, c("2023", "2024", "2025", "2023-25")) ||
    !setequal(acquisition$scenario_name, c("Low-cost", "Central", "High-cost")) ||
    anyNA(acquisition[c(acquisition_keys, "scenario_name", acquisition_money,
      acquisition_parameters, "cpi_factor", "years_in_period", "passengers",
      "actual_total_gross_debt_to_sale_value")]) ||
    any(acquisition$cpi_factor <= 0 | acquisition$passengers <= 0) ||
    any(!acquisition$period %in% c("Annual", "Pooled")) ||
    any(acquisition$years_in_period != if_else(acquisition$period == "Pooled", 3L, 1L)) ||
    any(acquisition$period == "Pooled" & abs(acquisition$cpi_factor - 1) > 1e-8))
  stop("Invalid standalone acquisition coverage or conversion inputs.")
acquisition <- acquisition |>
  mutate(across(all_of(acquisition_money), ~ .x * cpi_factor),
    observed_annual_ebitda_m = ebitda_m / years_in_period,
    annual_ground_rent_saving_m = ground_rent_m / years_in_period,
    existing_net_debt_m = gross_debt_m - cash_m,
    annual_existing_net_financing_cost_m = net_financing_cost_m / years_in_period,
    annual_additional_acquisition_interest_m =
      additional_acquisition_interest_m / years_in_period,
    annual_combined_net_financing_cost_m =
      annual_existing_net_financing_cost_m + annual_additional_acquisition_interest_m,
    additional_interest_per_pax = additional_acquisition_interest_m * 1e6 / passengers,
    acquisition_debt_share_pct = 100 * acquisition_debt_share,
    acquisition_interest_rate_pct = 100 * acquisition_interest_rate,
    post_purchase_gross_debt_to_ev_pct = 100 * actual_total_gross_debt_to_sale_value)
acq_close <- function(a, b) {
  all(is.finite(a) & is.finite(b) &
    abs(a - b) <= 1e-7 * pmax(1, abs(a), abs(b)))
}
with(acquisition, {
  if (!acq_close(valuation_ebitda_m,
        observed_annual_ebitda_m + annual_ground_rent_saving_m) ||
      !acq_close(estimated_sale_value_m, valuation_ebitda_m * purchase_multiple) ||
      !acq_close(equity_purchase_consideration_m, estimated_sale_value_m - existing_net_debt_m) ||
      !acq_close(new_acquisition_debt_m, equity_purchase_consideration_m * acquisition_debt_share) ||
      !acq_close(equity_purchase_consideration_m, new_acquisition_debt_m + buyer_equity_contribution_m) ||
      !acq_close(total_gross_debt_m, gross_debt_m + new_acquisition_debt_m) ||
      !acq_close(annual_additional_acquisition_interest_m, new_acquisition_debt_m * acquisition_interest_rate) ||
      !acq_close(actual_total_gross_debt_to_sale_value, total_gross_debt_m / estimated_sale_value_m))
    stop("Standalone acquisition transaction or annual financing checks failed.")
})
# A wide table makes transaction bridges and financing stacks easy to select.
# Keep annual-equivalent flow measures and stock measures together, but do not
# mix three-year flows into a transaction valuation or annual interest chart.
acquisition_fields <- c(
  observed_annual_ebitda_m = "Observed annual EBITDA",
  annual_ground_rent_saving_m = "Annual federal ground rent saving",
  valuation_ebitda_m = "Annual rent-adjusted valuation EBITDA",
  estimated_sale_value_m = "Estimated enterprise value",
  gross_debt_m = "Existing gross debt",
  cash_m = "Existing cash",
  existing_net_debt_m = "Existing net debt",
  equity_purchase_consideration_m = "Equity purchase consideration",
  new_acquisition_debt_m = "New acquisition debt",
  buyer_equity_contribution_m = "Buyer equity contribution",
  total_gross_debt_m = "Post-acquisition gross debt",
  annual_existing_net_financing_cost_m = "Existing annual net financing costs",
  annual_additional_acquisition_interest_m = "Additional annual acquisition interest",
  annual_combined_net_financing_cost_m = "Combined annual net financing costs")
acquisition_wide <- acquisition |>
  select(airport, year, period, year_label, years_in_period,
    scenario_id, scenario_name, purchase_multiple, acquisition_debt_share_pct,
    acquisition_interest_rate_pct, all_of(names(acquisition_fields)),
    post_purchase_gross_debt_to_ev_pct, additional_interest_per_pax) |>
  arrange(airport, year_label, purchase_multiple)
export(acquisition_wide, "06b_acquisition_financing_2023_2025.csv",
  "Standalone acquisition valuation and financing",
  "Acquisition model acquisition_scenarios.csv and scenario_assumptions.csv",
  "All monetary amounts real 2025 CAD millions except additional_interest_per_pax (2025 CAD/passenger). Annual-equivalent EBITDA, rent and interest flows; pooled debt and cash are mean real stocks, not sums. EV = annual rent-adjusted EBITDA times multiple; equity consideration = EV less existing net debt. New debt share applies to equity consideration, not EV or total debt; existing debt remains. Rates and debt ratios are percentage points. BIG4 is the model aggregate. Illustrative financing packages, before profitability targets.")
acquisition_long <- acquisition_wide |>
  pivot_longer(all_of(c(names(acquisition_fields), "purchase_multiple",
    "acquisition_debt_share_pct", "acquisition_interest_rate_pct",
    "post_purchase_gross_debt_to_ev_pct", "additional_interest_per_pax")),
    names_to = "metric", values_to = "value") |>
  mutate(metric_label = case_when(
    metric == "purchase_multiple" ~ "EV / rent-adjusted EBITDA multiple",
    metric == "acquisition_debt_share_pct" ~ "New debt share of equity consideration",
    metric == "acquisition_interest_rate_pct" ~ "New borrowing interest rate",
    metric == "post_purchase_gross_debt_to_ev_pct" ~ "Post-acquisition gross debt / EV",
    metric == "additional_interest_per_pax" ~ "Additional acquisition interest per passenger",
    TRUE ~ unname(acquisition_fields[metric])),
    unit = case_when(metric == "purchase_multiple" ~ "multiple",
      grepl("_pct$", metric) ~ "percent",
      metric == "additional_interest_per_pax" ~ "2025 CAD per passenger",
      TRUE ~ "2025 CAD millions"))
check_keys(acquisition_long, c(acquisition_keys, "metric"), "standalone acquisition long")
export(acquisition_long, "06c_acquisition_financing_long_2023_2025.csv",
  "Standalone acquisition valuation and financing",
  "06b acquisition financing wide export",
  "Same valuation and financing measures as 06b, in metric/value/unit format. Monetary flow measures are annual-equivalent; stocks are transaction amounts. Not additive across all metrics; use only the relevant components of each bridge.")

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
  "counterfactual_category_m", "increase_pct", "observed_per_pax",
  "additional_per_pax", "counterfactual_per_pax"), "revenue impacts")
if (nrow(impacts) != 1080L) stop("Expected 1080 revenue-impact category records.")
# Retain all original total-passenger rates. The alternative denominator
# is deliberately available only for the parking/ground transport category.
is_parking <- impacts$revenue_category == "parking_transport_revenue"
if (anyNA(is_parking) || sum(is_parking) != 270L)
  stop("Expected 270 parking revenue-impact records.")
impacts <- impacts |>
  mutate(
    observed_per_est_enplaned_pax = if_else(is_parking,
      observed_per_pax / parking_enplanement_share, NA_real_),
    additional_per_est_enplaned_pax = if_else(is_parking,
      additional_per_pax / parking_enplanement_share, NA_real_),
    counterfactual_per_est_enplaned_pax = if_else(is_parking,
      counterfactual_per_pax / parking_enplanement_share, NA_real_))
export(impacts, "08_revenue_impacts_2025.csv",
  "Final profitability thresholds and revenue allocation",
  "Revenue impacts 2025 long CSV",
  "2025; original per_pax fields use total passengers. Additional per_est_enplaned_pax fields apply only to parking and ground transportation, using 50% of total passengers as estimated enplanements, in 2025 CAD. Do not stack these fields with total-passenger rates. Category uplift is NOT a fee/passenger-incidence forecast. Total and three components must not be added together.")

# 8. DIRECTLY OBSERVED REAL AUSTRALIAN HISTORICAL REVENUE PATH.
# The counterfactual script already deflates the ACCC historical nominal
# data using Australian FY CPI. Reuse that output; no duplicate CPI logic.
history_real <- input_csv(file.path(base_legacy,
  "australia_observed_real_yield_1998_2007.csv"), required = FALSE)
if (is.null(history_real)) {
  pending("Australia observed 1998–2007 real path",
    "airport_revenue_counterfactual.R historical ACCC/FAC yield + Australian CPI",
    "Run the existing legacy counterfactual script with the new real-history export, then rerun presentation exports.")
} else {
  require_columns(history_real, c("airport", "australian_fy_end",
    "real_yield", "index_1998", "source", "regulatory_period"),
    "observed Australian historical yields")
  check_keys(history_real, c("airport", "australian_fy_end"),
    "observed Australian history")
  if (nrow(history_real) != 50L ||
      any(!is.finite(history_real$index_1998)))
    stop("Expected 40 airport-year and 10 pooled historical index rows.")
  require_columns(history_real, c("nominal_aud_pp", "real_aud_pp_2025"),
    "Historical dollar yields; rerun r/airport_revenue_counterfactual.R with updated code")
  if (any(!is.finite(history_real$real_aud_pp_2025) | history_real$real_aud_pp_2025 <= 0))
    stop("Missing constant-dollar historical yield; rerun the updated historical script.")
  history_real <- history_real |>
    mutate(real_cad_pp_2025 = real_aud_pp_2025 * cad_per_aud_2025)
  export(history_real, "09_australia_observed_real_yield_1998_2007.csv",
    "Australia observed 1998–2007 real path",
    "airport_revenue_counterfactual.R: CPI-deflated ACCC Figure 2.2 and FAC Sydney 1998",
    "Real aeronautical yield index 1998=100; BIG4 index is an unweighted mean of four airport indexes. BIG4 dollar yields are separate unweighted means of four airport yields. real_yield retains the original CPI-normalized value; real_aud_pp_2025 uses the FY2025 CPI from the same historical CPI vintage; real_cad_pp_2025 applies the production fixed 2025 CAD/AUD exchange rate. Sydney 1998 FAC source differs in scope from other ACCC figures.")
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

# 9b. OBSERVED ACCC FINANCIAL HISTORY: FY 2007-08 to FY 2024-25.
# Supplementary database operating profit (after D&A), NOT EBITDA or net
# income. Source real 2024-25 AUD; convert once to 2025 CAD after calculating
# all ratios, implied vehicle counts and indices. No further CPI adjustment.
history_fin_path <- here::here("output",
  "accc_airport_financial_analysis_2007_2025.xlsx")
history_rds_path <- here::here("data", "australia",
  "accc_monitor_data_2005_2025.rds")
history_pass_path <- here::here("data", "australia",
  "accc_airport_passengers_2007_2025.csv")
history_keys <- c("airport", "year")
history_year_end <- function(x) {
  2000L + as.integer(substr(as.character(x), nchar(as.character(x)) - 1L,
    nchar(as.character(x))))
}
history_filter <- function(d) {
  d |>
    mutate(year = history_year_end(year)) |>
    filter(airport %in% aus_airports, year %in% 2008:2025)
}
history_amounts <- c("total_revenue", "total_expenses", "total_profit",
  "aero_revenue", "aero_expenses", "aero_profit",
  "parking_revenue", "parking_expenses", "parking_profit",
  "landside_revenue", "parking_transport_revenue", "other_revenue",
  "other_expenses", "other_profit")
history_fin <- readxl::read_excel(history_fin_path, sheet = "financial") |>
  history_filter()
require_columns(history_fin, c(history_keys, history_amounts),
  "historical ACCC financial workbook; rerun parse_accc_airports.R if outdated")
history_fin <- history_fin |> select(all_of(history_keys), all_of(history_amounts))
history_pass <- input_csv(history_pass_path) |> history_filter() |>
  select(all_of(history_keys), passengers)
history_parsed <- readRDS(history_rds_path)
history_parking <- history_parsed$carpark_financial
require_columns(history_parking, c("airport", "year", "section", "measure",
  "value"), "historical ACCC parking source")
history_parking <- history_parking |> history_filter() |>
  filter(section == "car_parking")
# Financial table throughput is in thousands, with fixed precinct rows in
# the 2024-25 source workbook. Missing precinct values are not zero-filled.
throughput <- history_parking |>
  filter(grepl("^Annual throughput of ", measure)) |>
  mutate(expected_precincts = case_when(airport == "BNE" ~ 4L,
    airport == "MEL" ~ 2L, airport == "PER" ~ 4L, airport == "SYD" ~ 3L))
check_keys(throughput, c(history_keys, "measure"), "parking throughput components")
throughput <- throughput |>
  group_by(airport, year) |>
  summarise(parking_vehicles_reported = if (n() == first(expected_precincts) &&
    all(is.finite(value) & value >= 0)) 1000 * sum(value) else NA_real_,
    .groups = "drop")
vehicle_lookup <- c("Revenue per vehicle ($)" = "parking_revenue_per_vehicle",
  "Operating expenses per vehicle ($)" = "parking_expenses_per_vehicle",
  "Operating profit per vehicle ($)" = "parking_profit_per_vehicle",
  "Operating margin per vehicle ($)" = "parking_profit_per_vehicle")
vehicle_rates <- history_parking |>
  filter(measure %in% names(vehicle_lookup)) |>
  transmute(airport, year, metric = unname(vehicle_lookup[measure]), value)
check_keys(vehicle_rates, c(history_keys, "metric"), "ACCC parking vehicle yields")
vehicle_rates <- vehicle_rates |>
  pivot_wider(names_from = metric, values_from = value)
history_roa <- readxl::read_excel(history_fin_path, sheet = "margins") |>
  history_filter() |>
  select(all_of(history_keys), aero_ebita_roa, total_ebita_roa)
for (d in list(history_fin, history_pass, throughput, vehicle_rates, history_roa))
  check_keys(d, history_keys, "historical ACCC source")
history_airports <- history_fin |>
  left_join(history_pass, by = history_keys) |>
  left_join(throughput, by = history_keys) |>
  left_join(vehicle_rates, by = history_keys) |>
  left_join(history_roa, by = history_keys) |>
  mutate(
    # ACCC published per-vehicle yields and reported throughput are separate
    # observations; PER in particular does not always reconcile. Recover
    # the denominator implicit in the published revenue yield for pooling.
    parking_vehicles_implied = if_else(parking_revenue_per_vehicle > 0,
      parking_revenue * 1e6 / parking_revenue_per_vehicle, NA_real_),
    commercial_revenue = total_revenue - aero_revenue,
    # Match residual expenses/profit with revenue INCLUDING landside.
    other_incl_landside_revenue = total_revenue - aero_revenue - parking_revenue)
if (nrow(history_airports) != 72L ||
    any(table(history_airports$airport) != 18L) ||
    any(!is.finite(history_airports$passengers) | history_airports$passengers <= 0))
  stop("Expected 72 ACCC airport-years with valid passenger counts.")
history_amounts <- c(history_amounts, "commercial_revenue",
  "other_incl_landside_revenue")
# Use ratios of sums, with complete coverage. For aggregate published-yield
# equivalents, use the implicit revenue-yield denominator; keep the reported
# throughput-based calculation separately labelled.
history_big4 <- history_airports |>
  group_by(year) |>
  summarise(across(all_of(c(history_amounts, "passengers",
    "parking_vehicles_reported", "parking_vehicles_implied")), complete_sum),
    .groups = "drop") |>
  mutate(airport = "BIG4",
    parking_revenue_per_vehicle = if_else(parking_vehicles_implied > 0,
      parking_revenue * 1e6 / parking_vehicles_implied, NA_real_),
    parking_expenses_per_vehicle = if_else(parking_vehicles_implied > 0,
      parking_expenses * 1e6 / parking_vehicles_implied, NA_real_),
    parking_profit_per_vehicle = if_else(parking_vehicles_implied > 0,
      parking_profit * 1e6 / parking_vehicles_implied, NA_real_),
    aero_ebita_roa = NA_real_, total_ebita_roa = NA_real_)
history_divide <- function(n, d) {
  n / replace(d, !is.finite(d) | d <= 0, NA_real_)
}
history_full <- bind_rows(history_airports, history_big4) |>
  mutate(
    financial_year = sprintf("%d-%02d", year - 1L, year %% 100L),
    across(all_of(history_amounts),
      ~ history_divide(.x * 1e6, passengers), .names = "{.col}_per_passenger"),
    parking_revenue_per_est_enplaned_pax =
      parking_revenue_per_passenger / parking_enplanement_share,
    parking_transport_revenue_per_est_enplaned_pax =
      parking_transport_revenue_per_passenger / parking_enplanement_share,
    parking_revenue_per_reported_vehicle =
      history_divide(parking_revenue * 1e6, parking_vehicles_reported),
    parking_expenses_per_reported_vehicle =
      history_divide(parking_expenses * 1e6, parking_vehicles_reported),
    parking_profit_per_reported_vehicle =
      history_divide(parking_profit * 1e6, parking_vehicles_reported),
    total_op_margin = history_divide(100 * total_profit, total_revenue),
    aero_op_margin = history_divide(100 * aero_profit, aero_revenue),
    parking_op_margin = history_divide(100 * parking_profit, parking_revenue),
    other_incl_landside_op_margin =
      history_divide(100 * other_profit, other_incl_landside_revenue)) |>
  # Rename unmatched residual components to make their scope explicit.
  rename(other_excl_landside_revenue = other_revenue,
    other_excl_landside_revenue_per_passenger = other_revenue_per_passenger,
    other_incl_landside_expenses = other_expenses,
    other_incl_landside_profit = other_profit,
    other_incl_landside_expenses_per_passenger = other_expenses_per_passenger,
    other_incl_landside_profit_per_passenger = other_profit_per_passenger) |>
  group_by(airport) |>
  mutate(across(c(ends_with("_per_passenger"), ends_with("_per_vehicle"),
    ends_with("_per_reported_vehicle"), ends_with("_per_est_enplaned_pax"),
    all_of(setdiff(history_amounts, c("other_revenue", "other_expenses",
      "other_profit"))), other_excl_landside_revenue,
    other_incl_landside_expenses, other_incl_landside_profit),
    ~ history_divide(100 * .x, .x[year == 2008L][1]),
    .names = "{.col}_index_2008")) |>
  ungroup() |>
  relocate(airport, year, financial_year, passengers,
    parking_vehicles_reported, parking_vehicles_implied) |>
  arrange(airport, year)
check_keys(history_full, history_keys, "historical ACCC financial panel")
if (nrow(history_full) != 90L)
  stop("Expected 90 ACCC historical airport and BIG4 observations.")
# Convert only monetary levels and yields, not counts, margins, ROA or indices.
history_money_cols <- c(setdiff(history_amounts, c("other_revenue", "other_expenses",
  "other_profit")), "other_excl_landside_revenue", "other_incl_landside_expenses",
  "other_incl_landside_profit")
history_full <- history_full |>
  mutate(across(c(all_of(history_money_cols), ends_with("_per_passenger"),
    ends_with("_per_vehicle"), ends_with("_per_reported_vehicle"),
    ends_with("_per_est_enplaned_pax")), ~ .x * cad_per_aud_2025),
    currency = "CAD", price_base = "2025")
export(history_full, "09b_australia_financial_history_2008_2025.csv",
  "Historical Australian airport financials and parking",
  "ACCC supplementary financial workbook, passenger CSV and parsed carpark_financial RDS",
  "FY2007-08 to FY2024-25; real 2025 CAD millions for monetary levels, CAD for yields (ACCC real FY2024-25 AUD multiplied by production fixed 2025 CAD/AUD rate), persons/vehicles for counts; percent for margins and EBITA returns; indices FY2007-08=100 (missing if baseline unavailable or nonpositive). Operating profit is ACCC operating profit after D&A, not EBITDA or net income. Parking per estimated enplaned passenger uses half of total passengers. Airport per_vehicle rates are published ACCC rates; BIG4 per_vehicle rates use implied vehicle counts from published revenue yields. Separate per_reported_vehicle rates use reported financial-table throughput summed across precincts; Perth throughput and published yields do not always reconcile. Implied vehicle counts are diagnostic estimates, not observed counts. BIG4 rates and margins are ratios of sums; no aggregate EBITA ROA without matching asset denominators. Other excl landside revenue differs in scope from other incl landside expenses/profit; the residual margin uses other incl landside revenue. Categories overlap; not all columns are additive. No extrapolation or additional CPI adjustment.")

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
  # Balanced survey panel: hold the composition of each airport's survey
  # indicators constant over time. A series is identified by airport,
  # survey source, terminal, category and exact ACCC measure label.
  # Missing terminal/category labels are legitimate (e.g. airport-wide
  # airline questions); encode as empty strings for grouping, not as
  # invented terminal or category names. Do not match renamed questions
  # or changed terminal definitions by fuzzy similarity.
  #
  # Require a finite rating in EVERY non-pandemic financial year from
  # 2007-08 through 2024-25. FY2019-20 through FY2021-22 are excluded from
  # the completeness test because COVID disrupted surveys and operations.
  # Retain any actual pandemic-year observations in the indicator export,
  # but omit these years from the summary used for historical trend charts.
  # No interpolation, imputation or zero-filling of missing ratings.
  quality_years <- setdiff(2008:2025, 2020:2022)
  quality_series_keys <- c("airport", "source", "terminal", "category", "measure")
  quality_surveys <- quality |>
    filter(airport %in% aus_airports,
      source %in% c("passenger_survey", "airline_survey")) |>
    mutate(financial_year = as.character(year),
      year = history_year_end(year),
      terminal = coalesce(as.character(terminal), ""),
      category = coalesce(as.character(category), "")) |>
    filter(year %in% 2008:2025, is.finite(value))
  # A terminal/category/question must identify one observation per year.
  # Stop if the source contains duplicate observations instead of implicitly
  # weighting that indicator more heavily or silently dropping a row.
  if (anyDuplicated(quality_surveys[c(quality_series_keys, "year")]) ||
      anyNA(quality_surveys[c(quality_series_keys, "year")]))
    stop("Duplicate or incomplete ACCC survey series keys.")
  quality_coverage <- quality_surveys |>
    group_by(across(all_of(quality_series_keys))) |>
    summarise(
      nonpandemic_years_available = n_distinct(year[year %in% quality_years]),
      first_year = min(year), last_year = max(year),
      balanced_panel = all(quality_years %in% year),
      .groups = "drop")
  quality_indicators <- quality_surveys |>
    left_join(quality_coverage, by = quality_series_keys) |>
    mutate(in_trend_period = year %in% quality_years) |>
    arrange(airport, source, terminal, category, measure, year)
  export(quality_indicators, "10b_australia_quality_survey_indicators.csv",
    "Australian quality survey indicator coverage",
    "ACCC airport_quality extract, survey observations only",
    "Original numeric survey ratings with terminal/category/measure keys and balanced-panel eligibility. Completeness requires all 15 non-pandemic FYs ending 2008-2025 (excluding 2020-2022). Pandemic observations retained and flagged. Objective indicators excluded. No imputation.")
  export(quality_coverage, "10c_australia_quality_panel_coverage.csv",
    "Australian quality survey balanced-panel audit",
    "ACCC airport_quality survey indicator series",
    "One row per airport/source/terminal/category/measure. balanced_panel identifies questions with ratings in every required non-pandemic year; first/last years include available pandemic observations. Different airports can have different balanced indicator baskets.")

  # Compute two airport-level summaries, separately for passenger and airline
  # surveys: (1) all available questions each year, which can change in
  # composition; (2) balanced questions only, with a fixed indicator basket.
  # Equal weight per terminal/category/measure observation, not per terminal,
  # category, passenger or respondent: survey sample sizes are unavailable.
  # A terminal with more questions therefore contributes more observations.
  # These are descriptive analyst composites, NOT the ACCC official overall
  # quality rating. Compare each airport's trend rather than treating different
  # airport baskets as directly comparable measures of absolute quality.
  # No BIG4 average: quality is experienced at individual airports.
  quality_summary <- function(d, method) {
    d |>
      filter(in_trend_period) |>
      group_by(airport, source, year, financial_year) |>
      summarise(mean_rating = mean(value),
        n_indicators = n(), .groups = "drop") |>
      mutate(panel = method)
  }
  quality_balanced <- quality_summary(
    filter(quality_indicators, balanced_panel), "balanced")
  quality_all <- quality_summary(quality_indicators, "all_available")
  quality_counts <- quality_balanced |>
    group_by(airport, source) |>
    summarise(n_years = n_distinct(year),
      n_indicator_counts = n_distinct(n_indicators), .groups = "drop")
  if (nrow(quality_counts) != 8L ||
      any(quality_counts$n_years != length(quality_years)) ||
      any(quality_counts$n_indicator_counts != 1L))
    stop("ACCC balanced survey panel has inconsistent coverage.")
  quality_trends <- bind_rows(quality_balanced, quality_all) |>
    arrange(airport, source, panel, year)
  check_keys(quality_trends, c("airport", "source", "panel", "year"),
    "ACCC quality survey trends")
  export(quality_trends, "10d_australia_quality_survey_trends.csv",
    "Australian airport survey quality trends",
    "ACCC survey indicators and balanced-panel coverage",
    "Airport-specific equal-indicator-weight mean ratings, separate passenger and airline surveys. Balanced panel uses fixed within-airport question/terminal/category basket across 15 non-pandemic years; all_available uses available questions each year and may have changing composition. Neither is official ACCC overall quality or respondent-weighted. No cross-airport average or causal inference.")
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
