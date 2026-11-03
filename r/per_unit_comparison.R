# Canada–Australia airport financial yields (V9, 2026-10-28)
# ------------------------------------------------------------
# V9 retains V8's long-run REVENUE + PASSENGER comparisons to 2015–2025 and retains V7's
# detailed COST/EBITDA comparison for 2023–2025 only.
#
# CANADA: 2015–22: combined audited-source extraction (CAD $000, nominal).
#         2023–25: 'Big 4 long' (CAD millions, nominal).
#         Revenue comparisons INCLUDE airport improvement fees (AIF).
# AUSTRALIA: ACCC supplementary financials (ALREADY constant FY2024–25 AUD
#            millions) and ACCC passengers; regulatory accounts are nominal.
# PRICE BASE: 2025 CAD. AU supplementary values use fixed 2025 AUD/CAD FX.
# YEARS: Canada calendar years; Australia fiscal years ending 30 June.
#
# Prerequisites: Updated ACCC parser and regulatory EBITDA extraction for V7.
# Packages: tidyverse, here, janitor, cansim, readxl, readabs.
#
# IMPORTANT SOURCE CAVEATS:
# * YYZ 2015 AIF gross-up estimated from a 4% airline collection fee. Original
#   2015 AIF stays unchanged in the source workbook; derived adjustment is
#   explicitly flagged in the diagnostics and dataset.
# * YYC 2015 is blank in the last supplied combined workbook. Add the two
#   figures found on Wayback before treating 2015 Big 4 aggregates as complete.
# * Different fiscal-year conventions mean 2020 shocks are not simultaneous.
# * ACCC supplementary revenues are NEVER deflated a second time.
# * SCACH below-operating income is calendar-year, whereas ACCC operations
#   are June-ending fiscal-year; never reconstruct a hybrid income statement.
# * Accounting income-tax EXPENSE is not tax payable or cash tax paid.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(janitor)
  library(cansim)
  library(readxl)
  library(readabs)
})

# SETTINGS ----------------------------------------------------------------
base_year       <- 2025L
start_year      <- 2015L
long_years      <- start_year:base_year
recent_years    <- 2023:2025
cad_per_aud_2025 <- 0.9009 # Bank of Canada, 2025 annual average
# https://www.bankofcanada.ca/rates/exchange/annual-average-exchange-rates/

out_dir <- here('output', 'airport_comparison')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Avoid re-fetching ABS CPI unless explicitly requested.
refresh_abs_cpi <- FALSE
# Set FALSE to build revenue-only outputs if regulatory CSVs are not ready.
run_cost_comparison <- TRUE
# Historical Toronto collection fee estimate for 2015 only (not a verified rate).
assumed_yyz_2015_collection_fee <- 0.04

find_file <- function(...) {
  candidates <- c(...)
  matches <- candidates[file.exists(candidates)]
  if (!length(matches)) {
    stop('Cannot locate input. Tried:\n', paste(candidates, collapse = '\n'),
         '\nMove the file to one of these paths or edit the settings.', call. = FALSE)
  }
  matches[1]
}

assert_unique <- function(data, cols, label) {
  dup <- data |> count(across(all_of(cols))) |> filter(n > 1)
  if (nrow(dup)) {
    print(dup, n = 20)
    stop('Duplicate keys in ', label, ': ', paste(cols, collapse = ', '), call. = FALSE)
  }
  invisible(data)
}

# SOURCES -----------------------------------------------------------------
can_recent_path <- find_file(
  here('output', 'Airport privatization.xlsx'),
  here('output', 'Airport privatization CCPA sheet.xlsx')
)
can_historical_path <- find_file(
  here('output', 'canadian_big4_aeronautical_revenue_2015_2022_combined.xlsx'),
  here('data', 'canada', 'canadian_big4_aeronautical_revenue_2015_2022_combined.xlsx'),
  here('data', 'canada', 'operator_reports',
       'canadian_big4_aeronautical_revenue_2015_2022.xlsx')
)
aus_fin_path <- find_file(
  here('output', 'accc_airport_financial_analysis_2007_2025.xlsx'),
  here('data', 'australia', 'accc_airport_financial_analysis_2007_2025.xlsx')
)
aus_pass_path <- here('data', 'australia', 'accc_airport_passengers_2007_2025.csv')

# CANADIAN PASSENGER TRAFFIC ----------------------------------------------
ports <- tribble(
  ~geo, ~airport,
  'Calgary International, Alberta', 'YYC',
  'Montréal/Pierre Elliott Trudeau International, Quebec', 'YUL',
  'Toronto/Lester B Pearson International, Ontario', 'YYZ',
  'Vancouver International, British Columbia', 'YVR'
)

pass_df <- cansim::get_cansim('23-10-0253-01') |>
  janitor::clean_names() |>
  filter(geo %in% ports$geo,
         air_passenger_traffic == 'Total, passenger sector') |>
  inner_join(ports, by = 'geo') |>
  transmute(airport,
            year = readr::parse_integer(substr(as.character(ref_date), 1L, 4L)),
            passengers = as.numeric(val_norm)) |>
  filter(year %in% long_years) |>
  distinct()
assert_unique(pass_df, c('airport', 'year'), 'StatCan passenger counts')
if (nrow(pass_df) != 4L * length(long_years) ||
    anyNA(pass_df$passengers) || any(pass_df$passengers <= 0)) {
  stop("Incomplete passenger panel for Canada's Big 4, 2015–2025.")
}

# CANADIAN CPI: annual average, original index level; convert to 2025 CAD.
can_cpi <- cansim::get_cansim('18-10-0005-01') |>
  janitor::clean_names() |>
  filter(geo == 'Canada', products_and_product_groups == 'All-items') |>
  transmute(year = as.integer(ref_date), cpi_original = as.numeric(val_norm)) |>
  filter(year %in% long_years) |>
  distinct()
assert_unique(can_cpi, 'year', 'Canadian annual CPI')
if (nrow(can_cpi) != length(long_years)) stop('Canadian annual CPI is incomplete.')
can_cpi <- can_cpi |>
  arrange(year) |>
  mutate(cpi_2025 = 100 * cpi_original / cpi_original[year == base_year],
         adjustment_to_2025 = 100 / cpi_2025)

# AUSTRALIAN CPI ----------------------------------------------------------
# ABS quarterly All-groups CPI, weighted average of eight capitals.
# Official ABS time-series ID A2325846C (historical quarterly data).
# Compute each financial-year CPI as the average of Sep, Dec, Mar and Jun
# quarters. This is NOT the June-quarter index or its annual growth rate.
# Source: https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia
# readabs: https://mattcowgill.github.io/readabs/reference/read_abs.html
abs_id <- 'A2325846C'
abs_cache_dir <- here('data', 'australia', 'abs_cache')
dir.create(abs_cache_dir, recursive = TRUE, showWarnings = FALSE)
aus_cpi_cache <- here('data', 'australia', 'abs_cpi_fy_2015_2025.csv')

if (file.exists(aus_cpi_cache) && !refresh_abs_cpi) {
  aus_cpi <- readr::read_csv(aus_cpi_cache, show_col_types = FALSE)
  message('Loaded locally cached ABS financial-year CPI: ', aus_cpi_cache)
} else {
  abs_quarters <- readabs::read_abs(
    series_id = abs_id, path = abs_cache_dir, metadata = FALSE,
    show_progress_bars = FALSE
  ) |>
    transmute(date = as.Date(date), cpi_original = as.numeric(value)) |>
    filter(!is.na(date), !is.na(cpi_original)) |>
    mutate(year = lubridate::year(date) +
                    as.integer(lubridate::month(date) >= 7L)) |>
    filter(year %in% long_years)
  # Explicit quarter checks protect against incomplete/current FY releases.
  aus_cpi <- abs_quarters |>
    group_by(year) |>
    summarise(cpi_original = mean(cpi_original),
              quarters = n_distinct(date), .groups = 'drop') |>
    arrange(year)
  if (nrow(aus_cpi) != length(long_years) ||
      any(aus_cpi$quarters != 4L)) {
    stop('ABS CPI quarterly history is incomplete for FY2014–15 to FY2024–25.')
  }
  aus_cpi <- aus_cpi |>
    mutate(cpi_2025 = 100 * cpi_original / cpi_original[year == base_year],
           adjustment_to_2025 = 100 / cpi_2025,
           abs_series_id = abs_id)
  readr::write_csv(aus_cpi, aus_cpi_cache)
}
if (!all(c('year', 'cpi_original', 'adjustment_to_2025') %in% names(aus_cpi)) ||
    !setequal(aus_cpi$year, long_years)) {
  stop('Cached Australian FY CPI is invalid or covers the wrong years. ',
       'Set refresh_abs_cpi <- TRUE to rebuild.')
}
assert_unique(aus_cpi, 'year', 'Australian FY CPI')
readr::write_csv(
  bind_rows(
    can_cpi |> transmute(country = 'Canada', year, cpi_original,
                        adjustment_to_2025),
    aus_cpi |> transmute(country = 'Australia', year, cpi_original,
                        adjustment_to_2025)
  ), file.path(out_dir, 'cpi_inputs_2015_2025.csv')
)

# CANADIAN FINANCES: historical revenue, 2015–2022 ------------------------
# Read by POSITION to avoid complications from the original '$000' headers.
# The source workbook preserves the original reported and gross-up columns.
hist <- readxl::read_excel(can_historical_path, sheet = 'Main 2015-2022')
names(hist)[1:7] <- c('airport', 'year', 'aero_k', 'aif_reported_k',
                      'grossup_k', 'total_reported_k', 'total_comparable_k')
hist <- hist |>
  mutate(
    airport = as.character(airport), year = as.integer(year),
    across(c(aero_k, aif_reported_k, grossup_k,
             total_reported_k, total_comparable_k), as.numeric),
    grossup_method = case_when(
      airport == 'YYZ' & year == 2015L & is.na(grossup_k) ~
        'estimated from 4% airline collection fee',
      grossup_k != 0 & !is.na(grossup_k) ~ 'documented restatement',
      TRUE ~ 'none'
    ),
    grossup_k = if_else(
      airport == 'YYZ' & year == 2015L & is.na(grossup_k),
      aif_reported_k / (1 - assumed_yyz_2015_collection_fee) - aif_reported_k,
      grossup_k
    ),
    aif_gross_m = (aif_reported_k + grossup_k) / 1e3,
    aero_m = aero_k / 1e3,
    aero_plus_aif_m = aero_m + aif_gross_m
  ) |>
  filter(year %in% 2015:2022)
assert_unique(hist, c('airport', 'year'), 'Canadian 2015–2022 source workbook')
if (nrow(hist) != 32L) stop('Expected 32 Canadian historical airport-year rows.')

# Check calculated comparable totals against all SOURCE-VERIFIED entries.
check_hist <- hist |>
  filter(!is.na(total_comparable_k), !is.na(aero_plus_aif_m)) |>
  mutate(difference_k = abs(1e3 * aero_plus_aif_m - total_comparable_k))
if (any(check_hist$difference_k > 0.11)) {
  print(check_hist |> filter(difference_k > 0.11) |>
          select(airport, year, difference_k))
  stop('Historic comparable totals differ from source workbook. Verify.')
}

missing_hist <- hist |>
  filter(is.na(aero_m) | is.na(aif_gross_m)) |>
  transmute(airport, year, missing_aero = is.na(aero_m),
            missing_aif = is.na(aif_gross_m))
if (nrow(missing_hist)) {
  warning('Incomplete historic revenue inputs: ',
          paste(paste(missing_hist$airport, missing_hist$year), collapse = ', '),
          '. These observations will NOT be treated as zero. ',
          'Big 4 aggregates must require four complete airports.')
}

hist_long <- bind_rows(
  hist |> transmute(airport, year, item = 'Aeronautical Revenues',
                      category = 'revenue', variable = 'aero_revenue',
                      value_m = aero_m, grossup_method),
  hist |> transmute(airport, year, item = 'Airport Improvement Fees (gross basis)',
                      category = 'revenue', variable = 'aif_revenue',
                      value_m = aif_gross_m, grossup_method)
)
readr::write_csv(
  hist |> select(airport, year, aero_k, aif_reported_k, grossup_k,
                 grossup_method, aero_plus_aif_m),
  file.path(out_dir, 'canadian_historical_revenue_audit.csv')
)

# Canadian financial detail, 2023–2025, retained from V7 ------------------
can_lookup <- c(
  'Total Revenues' = 'total_revenue',
  'Aeronautical Revenues' = 'aero_revenue',
  'Airport Improvement Fees' = 'aif_revenue',
  'Commercial (non-aeronautical) Revenues' = 'commercial_revenue',
  'Car parking and ground transportation' = 'parking_transport_revenue',
  'Landing fees' = 'landing_fees',
  'General terminal charges' = 'terminal_charges',
  'Concessions' = 'concessions',
  'Rentals' = 'rentals',
  'Other' = 'other_commercial_detail',
  'Operating Expenses' = 'operating_expenses_can',
  'Expenses' = 'expenses_can_detail',
  'Goods and services' = 'purchased_services_other',
  'Salaries, wages and benefits' = 'labour',
  'Ground rent (federal gov payments)' = 'federal_ground_rent',
  'Payment in Lieu of Muni taxes' = 'pilt',
  'Musqueam–YVR Agreement' = 'musqueam_yvr_agreement',
  'Total ground rent and PILT' = 'ground_rent_pilt_subtotal',
  'Amortization and deprecitation' = 'depreciation_amortisation',
  'Write-down or impairement of property and equipment' = 'asset_impairment',
  'Interest and financing costs net of interest income' = 'net_financing_costs',
  'Post-employment pension benefits remeasurement loss' = 'pension_remeasurement',
  'Total Expenses' = 'total_expenses_can',
  'EBITDA' = 'ebitda_can',
  'Net Income' = 'net_income_can',
  # Optional categories: absent entries remain NA (never impute zero).
  'Profit Before Tax' = 'profit_before_tax_can',
  'Earnings before tax' = 'profit_before_tax_can',
  'Income Tax Expense' = 'income_tax_expense_can',
  'Income tax expense' = 'income_tax_expense_can'
)
if (!file.exists(can_recent_path)) {
  stop('Recent Canadian finances missing: ', can_recent_path)
}
can_recent <- read_excel(can_recent_path, sheet = 'Big 4 long') |>
  transmute(
    airport = recode(as.character(airport), Pearson = 'YYZ', Calgary = 'YYC',
                     Montreal = 'YUL', Vancouver = 'YVR'),
    year = as.integer(year), item = as.character(item),
    category = as.character(category), value_m = as.numeric(value),
    variable = unname(can_lookup[as.character(item)]),
    grossup_method = 'none'
  ) |>
  filter(year %in% recent_years)

# There must be exactly one source value per airport-year-variable.
can_source <- bind_rows(
  hist_long,
  can_recent |> select(airport, year, item, category, variable,
                         value_m, grossup_method)
) |>
  filter(!is.na(variable))
assert_unique(can_source, c('airport', 'year', 'variable'),
              'Canadian mapped source categories')

can_long <- can_source |>
  left_join(pass_df, by = c('airport', 'year')) |>
  left_join(can_cpi |> select(year, adjustment_to_2025), by = 'year') |>
  mutate(country = 'Canada', period = as.character(year),
         amount_cad_2025_m = value_m * adjustment_to_2025,
         per_passenger_cad_2025 = amount_cad_2025_m * 1e6 / passengers)

# AIF-inclusive measure is ADDITIVE ONLY for this distinct derived category.
# It must not be added to total revenue again.
can_aif <- can_long |>
  filter(variable %in% c('aero_revenue', 'aif_revenue')) |>
  group_by(airport, year) |>
  summarise(
    components = n_distinct(variable[!is.na(value_m)]),
    value_m = if (components[1] == 2L) sum(value_m) else NA_real_,
    passengers = first(passengers),
    adjustment_to_2025 = first(adjustment_to_2025),
    grossup_method = if (any(str_detect(grossup_method, 'estimated'))) {
      'estimated 2015 AIF gross-up'
    } else if (any(str_detect(grossup_method, 'documented'))) {
      'documented AIF restatement'
    } else 'none',
    .groups = 'drop'
  ) |>
  filter(components == 2L) |>
  mutate(country = 'Canada', period = as.character(year),
         item = 'Aeronautical Revenues + Airport Improvement Fees',
         category = 'derived', variable = 'aero_plus_aif',
         amount_cad_2025_m = value_m * adjustment_to_2025,
         per_passenger_cad_2025 = amount_cad_2025_m * 1e6 / passengers)

if (nrow(can_aif |> filter(year %in% recent_years)) !=
    4L * length(recent_years)) {
  stop('Incomplete 2023–2025 Canadian aero + AIF records in Big 4 long.')
}

can_values <- bind_rows(
  can_long |> select(country, airport, year, period, item, category,
                     variable, value_m, passengers, amount_cad_2025_m,
                     per_passenger_cad_2025, grossup_method),
  can_aif |> select(country, airport, year, period, item, category,
                    variable, value_m, passengers, amount_cad_2025_m,
                    per_passenger_cad_2025, grossup_method)
)

# AUSTRALIAN PASSENGERS ---------------------------------------------------
# Prefer the parser CSV from V7 (persons). If unavailable, reconstruct from
# the bundled 'passenger_calcs' worksheet (thousands of passengers).
if (file.exists(aus_pass_path)) {
  aus_pass <- readr::read_csv(aus_pass_path, show_col_types = FALSE) |>
    select(airport, year, passengers) |>
    mutate(year = as.character(year), passengers = as.numeric(passengers)) |>
    distinct()
} else {
  message('ACCC passenger CSV missing; using workbook passenger_calcs fallback.')
  aus_pass <- readxl::read_excel(aus_fin_path, sheet = 'passenger_calcs') |>
    filter(category == 'total', measure == 'revenue') |>
    transmute(airport, year = as.character(year),
              passengers = as.numeric(passengers) * 1000) |>
    distinct()
}
assert_unique(aus_pass, c('airport', 'year'), 'Australian passenger source')

# AUSTRALIAN FINANCES (already in REAL 2024–25 AUD) ------------------------
aus_fin <- read_excel(aus_fin_path, sheet = 'financial') |>
  mutate(year = as.character(year))
mandatory_aus <- c('total_revenue', 'aero_revenue', 'parking_revenue',
                   'other_revenue')
if (!all(mandatory_aus %in% names(aus_fin))) {
  stop('ACCC supplementary financial export is missing: ',
       paste(setdiff(mandatory_aus, names(aus_fin)), collapse = ', '))
}
# The pre-update parser lacks these variables. Do NOT substitute zero or
# incorrectly label the old 'other' aggregate as excluding landside.
has_landside_detail <- all(c('landside_revenue',
                             'parking_transport_revenue') %in% names(aus_fin))
if (!has_landside_detail) {
  warning('ACCC financial workbook has no updated landside/parking-transport ',
          'categories; leaving these and other_commercial_revenue as NA.')
  for (nm in c('landside_revenue', 'parking_transport_revenue')) {
    if (!nm %in% names(aus_fin)) aus_fin[[nm]] <- NA_real_
  }
}
aus_fin <- aus_fin |>
  mutate(commercial_revenue = total_revenue - aero_revenue,
         other_commercial_revenue = if (has_landside_detail) {
           other_revenue
         } else NA_real_)

fy_end <- function(x) {
  x <- as.character(x)
  as.integer(2000L + as.integer(stringr::str_extract(x, '\\d{2}$')))
}

aus_long <- aus_fin |>
  left_join(aus_pass, by = c('airport', 'year')) |>
  mutate(year_end = fy_end(year)) |>
  filter(year_end %in% long_years) |>
  pivot_longer(cols = -c(airport, year, passengers, year_end),
               names_to = 'variable', values_to = 'value_m') |>
  mutate(country = 'Australia', period = year,
         item = variable,
         category = case_when(
           str_detect(variable, 'revenue') ~ 'revenue',
           str_detect(variable, 'expenses') ~ 'expenditure',
           TRUE ~ 'result'),
         year = year_end,
         amount_cad_2025_m = value_m * cad_per_aud_2025,
         per_passenger_cad_2025 = amount_cad_2025_m * 1e6 / passengers,
         grossup_method = 'not applicable') |>
  select(country, airport, year, period, item, category, variable,
         value_m, passengers, amount_cad_2025_m,
         per_passenger_cad_2025, grossup_method)

if (anyNA(aus_long$passengers) || any(aus_long$passengers <= 0)) {
  stop('Missing Australian passengers. Check year joins and units.')
}

# LONGITUDINAL REVENUE OUTPUT ---------------------------------------------
compare_vars <- c('total_revenue', 'aero_revenue', 'aero_plus_aif',
                  'aif_revenue', 'commercial_revenue',
                  'parking_transport_revenue', 'parking_revenue',
                  'landside_revenue', 'other_commercial_revenue')

annual_all <- bind_rows(can_values, aus_long) |>
  filter(variable %in% compare_vars, !is.na(value_m), passengers > 0) |>
  select(country, airport, year, period, variable, value_m,
         amount_cad_2025_m, passengers, per_passenger_cad_2025,
         grossup_method) |>
  arrange(country, airport, variable, year)
assert_unique(annual_all, c('country', 'airport', 'year', 'variable'),
              'cross-country annual revenue')

# The Australian aeronautical series must cover all airports/years.
if (nrow(annual_all |> filter(country == 'Australia',
                              variable == 'aero_revenue')) !=
    4L * length(long_years)) {
  stop('Incomplete Australian 2015–2025 aeronautical-revenue panel.')
}

# Coverage is explicit; e.g., YYC 2015 or pre-2023 non-aero categories.
coverage_long <- tidyr::expand_grid(
  country = c('Canada', 'Australia'),
  airport = c('YYC', 'YUL', 'YYZ', 'YVR', 'BNE', 'MEL', 'PER', 'SYD'),
  year = long_years, variable = compare_vars
) |>
  filter((country == 'Canada' & airport %in% ports$airport) |
           (country == 'Australia' & airport %in% c('BNE','MEL','PER','SYD'))) |>
  left_join(annual_all |> transmute(country, airport, year, variable,
                                   available = TRUE),
            by = c('country', 'airport', 'year', 'variable')) |>
  mutate(available = replace_na(available, FALSE))

# A 2015 index for each airport's aeronautical revenues per passenger.
# Main Canada variable = aero_plus_aif; main Australia = aero_revenue.
aero_series <- annual_all |>
  filter((country == 'Canada' & variable == 'aero_plus_aif') |
           (country == 'Australia' & variable == 'aero_revenue')) |>
  group_by(country, airport) |>
  mutate(baseline_pp_2015 = per_passenger_cad_2025[
           match(start_year, year)],
         real_pp_index_2015 = 100 * per_passenger_cad_2025 /
           baseline_pp_2015,
         real_pp_change_pct_2015 = real_pp_index_2015 - 100) |>
  ungroup()

# Never calculate a combined Canadian index from only the available airports.
can_big4_annual <- aero_series |>
  filter(country == 'Canada') |>
  group_by(year) |>
  summarise(airports = n_distinct(airport),
            revenue_cad_2025_m = if (airports[1] == 4L) {
              sum(amount_cad_2025_m)
            } else NA_real_,
            passengers = if (airports[1] == 4L) sum(passengers) else NA_real_,
            per_passenger_cad_2025 = revenue_cad_2025_m * 1e6 / passengers,
            .groups = 'drop') |>
  mutate(real_pp_index_2015 = 100 * per_passenger_cad_2025 /
           per_passenger_cad_2025[year == start_year],
         complete_big4 = airports == 4L)

# Endpoint comparisons require both 2015 and 2025 observations.
growth_summary <- aero_series |>
  group_by(country, airport) |>
  summarise(
    n_years = n_distinct(year),
    real_pp_2015 = per_passenger_cad_2025[match(start_year, year)],
    real_pp_2025 = per_passenger_cad_2025[match(base_year, year)],
    real_growth_pct = 100 * (real_pp_2025 / real_pp_2015 - 1),
    valid_endpoints = !is.na(real_pp_2015) & !is.na(real_pp_2025),
    .groups = 'drop'
  )

readr::write_csv(annual_all, file.path(out_dir, 'revenue_annual_2015_2025.csv'))
readr::write_csv(coverage_long, file.path(out_dir, 'revenue_coverage_2015_2025.csv'))
readr::write_csv(aero_series, file.path(out_dir, 'aero_revenue_growth_2015_2025.csv'))
readr::write_csv(growth_summary, file.path(out_dir, 'aero_growth_summary_2015_2025.csv'))
readr::write_csv(can_big4_annual,
                 file.path(out_dir, 'canadian_big4_aero_annual_2015_2025.csv'))

# Retain V7's THREE-YEAR revenue outputs and weighted pooling convention.
annual <- annual_all |>
  filter(year %in% recent_years) |>
  select(country, airport, year, period, variable,
         amount_cad_2025_m, passengers, per_passenger_cad_2025)
pooled <- annual |>
  group_by(country, airport, variable) |>
  summarise(years = n_distinct(year),
            period_start = min(year), period_end = max(year),
            revenue_cad_2025_m = sum(amount_cad_2025_m),
            passengers = sum(passengers),
            per_passenger_cad_2025 = revenue_cad_2025_m * 1e6 / passengers,
            .groups = 'drop') |>
  mutate(full_three_years = years == length(recent_years))
readr::write_csv(annual, file.path(out_dir, 'revenue_annual.csv'))
readr::write_csv(pooled, file.path(out_dir, 'revenue_pooled.csv'))

# V7 detailed cost comparison: only 2023–2025 -----------------------------
if (run_cost_comparison) {
  cost_dir <- here('output')
  cost_paths <- c(
    file.path(cost_dir, 'accc_airport_ebitda_by_activity_2022_2025.csv'),
    file.path(cost_dir, 'accc_airport_expenditure_by_type_2022_2025.csv'),
    file.path(cost_dir, 'accc_airport_expenditure_groups_2022_2025.csv')
  )
  if (!all(file.exists(cost_paths))) {
    stop('Regulatory cost exports missing. Run the ACCC regulatory parser ',
         'or set run_cost_comparison <- FALSE for revenue-only output.\n',
         paste(cost_paths[!file.exists(cost_paths)], collapse = '\n'))
  }
  reg_ebitda <- readr::read_csv(cost_paths[1], show_col_types = FALSE) |>
    filter(year %in% recent_years) |>
    left_join(aus_cpi |> select(year, adjustment_to_2025), by = 'year') |>
    mutate(ebitda_real_aud_m = ebitda * adjustment_to_2025,
           ebitda_cad_2025_m = ebitda_real_aud_m * cad_per_aud_2025)
  assert_unique(reg_ebitda, c('airport', 'year', 'activity'),
                'ACCC regulatory EBITDA')
  reg_activity <- reg_ebitda
  reg_total <- reg_ebitda |> filter(activity == 'total')
  if (nrow(reg_total) != 4L * length(recent_years) ||
      anyNA(reg_total$ebitda_real_aud_m)) {
    stop('Incomplete 2023–25 regulatory EBITDA.')
  }
  reg_exp_types <- readr::read_csv(cost_paths[2], show_col_types = FALSE) |>
    filter(year %in% recent_years)
  reg_exp_groups <- readr::read_csv(cost_paths[3], show_col_types = FALSE) |>
    filter(year %in% recent_years)

  aus_reg_costs <- function(data, name_col) {
    data |>
      rename(variable = all_of(name_col), value_m = value_aud_m) |>
      left_join(aus_pass |>
                  transmute(airport, year = fy_end(year), passengers),
                by = c('airport', 'year')) |>
      left_join(aus_cpi |> select(year, adjustment_to_2025), by = 'year') |>
      mutate(country = 'Australia', period = fyear,
             amount_cad_2025_m = value_m * adjustment_to_2025 *
               cad_per_aud_2025,
             per_passenger_cad_2025 = amount_cad_2025_m * 1e6 / passengers) |>
      select(country, airport, year, period, activity, variable,
             value_m, passengers, amount_cad_2025_m, per_passenger_cad_2025)
  }
  aus_cost_types <- aus_reg_costs(reg_exp_types, 'item')
  aus_cost_groups <- aus_reg_costs(reg_exp_groups, 'category')

  can_cost_categories <- c(
    'labour', 'purchased_services_other', 'depreciation_amortisation',
    'federal_ground_rent', 'pilt', 'musqueam_yvr_agreement',
    'asset_impairment', 'net_financing_costs', 'pension_remeasurement'
  )
  can_cost_groups <- can_long |>
    filter(year %in% recent_years, variable %in% can_cost_categories) |>
    transmute(country, airport, year, period, activity = 'total', variable,
              value_m, passengers, amount_cad_2025_m,
              per_passenger_cad_2025)
  can_operating <- can_long |>
    filter(year %in% recent_years, variable == 'operating_expenses_can') |>
    transmute(country, airport, year, period, activity = 'total',
              variable = 'operating_expenses', value_m, passengers,
              amount_cad_2025_m, per_passenger_cad_2025)
  aus_operating <- reg_ebitda |>
    filter(activity == 'total') |>
    left_join(aus_pass |>
                transmute(airport, year = fy_end(year), passengers),
              by = c('airport', 'year')) |>
    transmute(country = 'Australia', airport, year, period = fyear,
              activity = 'total', variable = 'operating_expenses',
              value_m = expenditure - depreciation_amortisation,
              passengers,
              amount_cad_2025_m = value_m * adjustment_to_2025 *
                cad_per_aud_2025,
              per_passenger_cad_2025 = amount_cad_2025_m * 1e6 / passengers)
  cost_groups_annual <- bind_rows(
    can_cost_groups, can_operating,
    aus_cost_groups |> filter(activity == 'total'), aus_operating
  ) |> arrange(variable, country, airport, year)

  pool_yields <- function(data) {
    data |>
      filter(!is.na(amount_cad_2025_m), !is.na(passengers), passengers > 0) |>
      group_by(country, airport, activity, variable) |>
      summarise(years = n_distinct(year),
                amount_cad_2025_m = sum(amount_cad_2025_m),
                passengers = sum(passengers),
                per_passenger_cad_2025 = amount_cad_2025_m * 1e6 / passengers,
                .groups = 'drop') |>
      mutate(full_three_years = years == length(recent_years))
  }
  cost_groups_pooled <- pool_yields(cost_groups_annual) |>
    arrange(variable, country, airport)
  aus_cost_types_pooled <- pool_yields(aus_cost_types)

  assert_unique(cost_groups_annual,
                c('country', 'airport', 'year', 'variable'),
                'harmonized cost categories')
  assert_unique(aus_cost_types,
                c('airport', 'year', 'activity', 'variable'),
                'ACCC expenditure types')

  readr::write_csv(cost_groups_annual,
                   file.path(out_dir, 'cost_groups_annual.csv'))
  readr::write_csv(cost_groups_pooled,
                   file.path(out_dir, 'cost_groups_pooled.csv'))
  readr::write_csv(aus_cost_types,
                   file.path(out_dir, 'aus_cost_types_annual.csv'))
  readr::write_csv(aus_cost_types_pooled,
                   file.path(out_dir, 'aus_cost_types_pooled.csv'))
  # V9: complete operating-to-net profitability comparison, 2023–2025 -----
  # Sydney retains ACCC operating figures; SCACH calendar-year consolidated
  # financing, pre-tax profit, tax, and net income are substituted below ONLY.
  # No ACCC source cells are changed or overwritten.
  required_reg <- c('ebitda', 'operating_profit', 'depreciation_amortisation',
                    'revenue', 'interest_financing_costs', 'profit_before_tax',
                    'income_tax_expense', 'net_profit_after_tax', 'abnormal_items',
                    'ebit_reported')
  if (!all(required_reg %in% names(reg_total))) {
    stop('Run accc_regulatory_ebitda_v5.R first. Missing: ',
         paste(setdiff(required_reg, names(reg_total)), collapse = ', '))
  }

  # Use the verified, existing SCACH CSV. No additional reports are downloaded.
  scach_path <- here('data', 'australia', 'regulatory_accounts', 'scach_income_verified_inputs_template.csv')
  
  scach <- readr::read_csv(scach_path, show_col_types = FALSE) |>
    filter(airport == 'SYD', calendar_year %in% recent_years) |>
    mutate(year = as.integer(calendar_year))
  assert_unique(scach, 'year', 'SCACH consolidated statements')
  if (nrow(scach) != length(recent_years)) {
    stop('SCACH must have one consolidated calendar-year record for 2023–2025.')
  }
  scach_require <- c('total_revenue', 'ebitda', 'depreciation', 'amortisation',
                     'ebit', 'finance_income', 'finance_costs',
                     'swap_fair_value_change', 'rps_early_redemption_loss',
                     'net_finance_costs', 'profit_before_tax',
                     'income_tax_expense', 'net_profit_after_tax')
  if (anyNA(scach[scach_require])) stop('Incomplete SCACH income statement.')
  if (any(abs(scach$ebitda - scach$depreciation - scach$amortisation -
              scach$ebit) > 0.15) ||
      any(abs(scach$ebit - scach$net_finance_costs -
              scach$profit_before_tax) > 0.15) ||
      any(abs(scach$profit_before_tax - scach$income_tax_expense -
              scach$net_profit_after_tax) > 0.15)) {
    stop('SCACH consolidated income statement failed reconciliation.')
  }
  # Swap fair-value gains are positive in the CSV; losses are negative.
  # Net costs = finance costs - income - swap change + one-off redemption loss.
  if (any(abs(scach$finance_costs - scach$finance_income -
              scach$swap_fair_value_change + scach$rps_early_redemption_loss -
              scach$net_finance_costs) > 0.15)) {
    stop('SCACH net financing components failed reconciliation.')
  }
  scach_join <- scach |>
    select(year, all_of(scach_require), source_url, source_note,
           reporting_period) |>
    rename_with(~ paste0('scach_', .x), -year)

  # Canadian source categories already parsed into can_long in V8.
  # There is no general assumption that tax-exempt = zero tax expense.
  # EBIT is an operating proxy: supplied EBITDA minus D&A. Separate asset
  # impairments are retained and NOT mechanically subtracted for every airport,
  # since their placement above/below operating income varies by operator.
  can_fin_vars <- c('total_revenue', 'ebitda_can',
                    'depreciation_amortisation', 'asset_impairment',
                    'net_financing_costs', 'net_income_can',
                    'profit_before_tax_can', 'income_tax_expense_can')
  can_fin <- can_long |>
    filter(year %in% recent_years, variable %in% can_fin_vars) |>
    select(airport, year, variable, value_m) |>
    pivot_wider(names_from = variable, values_from = value_m)
  for (nm in can_fin_vars) {
    if (!nm %in% names(can_fin)) can_fin[[nm]] <- NA_real_
  }
  can_fin <- tidyr::expand_grid(airport = ports$airport, year = recent_years) |>
    left_join(can_fin, by = c('airport', 'year')) |>
    left_join(pass_df, by = c('airport', 'year')) |>
    left_join(can_cpi |> select(year, adjustment_to_2025), by = 'year') |> 
    transmute(
      country = 'Canada', airport, year, period = as.character(year),
      passengers, adjustment_to_2025, fx_cad_per_unit = 1,
      ebitda = ebitda_can,
      ebit = ebitda_can - depreciation_amortisation - asset_impairment,
      depreciation_amortisation, asset_impairment,
      abnormal_items = NA_real_, reported_ebit = NA_real_,
      interest_financing_costs = net_financing_costs,
      net_financing_costs, finance_costs_gross = NA_real_,
      finance_income = NA_real_, one_off_financing_loss = NA_real_,
      profit_before_tax = net_income_can,
     # accounting_income_tax_expense = income_tax_expense_can,
      accounting_income_tax_expense = 0,
      net_income = net_income_can,
      ebitda_ebit_revenue_basis = total_revenue,
      net_income_revenue_basis = total_revenue,
      financial_source = 'Big 4 long / Canadian airport annual reports',
      financial_period = as.character(year),
      financing_definition = 'Reported financing costs NET of interest income; classification differs by operator',
      net_income_definition = 'Reported Canadian annual-report net income',
      scach_source_url = NA_character_)

  aus_financial <- reg_total |>
    left_join(aus_pass |>
                transmute(airport, year = fy_end(year), passengers),
              by = c('airport', 'year')) |>
    left_join(scach_join, by = 'year') |>
    transmute(
      country = 'Australia', airport, year, period = fyear, passengers,
      adjustment_to_2025, fx_cad_per_unit = cad_per_aud_2025,
      ebitda,
      # ACCC operating profit is AFTER D&A, so no second subtraction.
      ebit = operating_profit, depreciation_amortisation,
      asset_impairment = NA_real_, abnormal_items,
      reported_ebit = ebit_reported,
      interest_financing_costs = if_else(airport == 'SYD',
                                         scach_net_finance_costs,
                                         interest_financing_costs),
      net_financing_costs = if_else(airport == 'SYD',
                                     scach_net_finance_costs, NA_real_),
      finance_costs_gross = if_else(airport == 'SYD',
                                     scach_finance_costs, NA_real_),
      finance_income = if_else(airport == 'SYD',
                                 scach_finance_income, NA_real_),
      one_off_financing_loss = if_else(airport == 'SYD',
                                        scach_rps_early_redemption_loss, NA_real_),
      profit_before_tax = if_else(airport == 'SYD',
                                    scach_profit_before_tax, profit_before_tax),
      accounting_income_tax_expense = if_else(airport == 'SYD',
                                               scach_income_tax_expense,
                                               income_tax_expense),
      net_income = if_else(airport == 'SYD',
                             scach_net_profit_after_tax, net_profit_after_tax),
      ebitda_ebit_revenue_basis = revenue,
      # SCACH net margins MUST use matching SCACH consolidated revenue,
      # not ACCC regulated revenue for the June-ending fiscal year.
      net_income_revenue_basis = if_else(airport == 'SYD',
                                          scach_total_revenue, revenue),
      financial_source = if_else(airport == 'SYD',
                                  'ACCC operating / SCACH consolidated below-operating',
                                  'ACCC audited regulatory accounts'),
      financial_period = if_else(airport == 'SYD',
                                  scach_reporting_period, fyear),
      financing_definition = if_else(airport == 'SYD',
        'SCACH net finance costs (interest income, swap movement, RPS loss included)',
        'ACCC signed Interest line, expense-positive; gross/net decomposition unavailable'),
      net_income_definition = if_else(airport == 'SYD',
        'SCACH reported consolidated calendar-year net income',
        'ACCC audited total reported fiscal-year net profit after tax'),
      scach_source_url = if_else(airport == 'SYD', scach_source_url,
                                 NA_character_))

  financial_source_annual <- bind_rows(can_fin, aus_financial) |>
    arrange(country, airport, year)
  assert_unique(financial_source_annual,
                c('country', 'airport', 'year'), 'financial source audit')
  if (nrow(financial_source_annual) != 8L * length(recent_years) ||
      anyNA(financial_source_annual$passengers) ||
      any(financial_source_annual$passengers <= 0) ||
      anyNA(financial_source_annual$adjustment_to_2025)) {
    stop('Incomplete eight-airport recent financial/FX/CPI/passenger keys.')
  }
  if (any(abs(financial_source_annual$ebitda -
              financial_source_annual$depreciation_amortisation -
              if_else(financial_source_annual$country == 'Canada',
                      financial_source_annual$asset_impairment, 0) -
              financial_source_annual$ebit) > 0.03, na.rm = TRUE)) {
    stop('Operating EBIT reconciliation failed.')
  }
  # The reported finance costs and tax charge belong to the audited income
  # statements and must not be added to operating expenditure. The bridge
  # below uses PBT and tax solely to VALIDATE net income, not to reconstruct it.
  # Test tax bridge only where the accounting source supplies both amounts.
  tax_bridge <- financial_source_annual |>
    filter(!is.na(profit_before_tax),
           !is.na(accounting_income_tax_expense), !is.na(net_income)) |>
    mutate(residual = profit_before_tax - accounting_income_tax_expense - net_income)
  if (any(abs(tax_bridge$residual) > 0.05)) {
    print(tax_bridge |> filter(abs(residual) > 0.05))
    stop('Reported net profit does not reconcile to pretax profit less tax.')
  }
  # Sydney source columns are not overwritten: keep all ACCC figures in reg_total
  # and export the parallel original and substitute values for direct inspection.
  sydney_regulatory_scach_audit <- reg_total |>
    filter(airport == 'SYD') |>
    select(airport, year, fyear, revenue, ebitda, operating_profit,
           ebit_reported, abnormal_items, interest_reported,
           profit_before_tax, income_tax_expense, net_profit_after_tax) |>
    left_join(scach_join, by = 'year')
  readr::write_csv(sydney_regulatory_scach_audit,
                   file.path(out_dir, 'sydney_regulatory_scach_audit_2023_2025.csv'))
  readr::write_csv(financial_source_annual,
                   file.path(out_dir, 'financial_source_audit_annual_2023_2025.csv'))

  # Convert every NEW financial item using V8's existing annual CPI factors
  # and fixed 2025 CAD/AUD exchange rate, with the same passenger joins.
  # SCACH calendar-year data are indexed to their matching year number using
  # the existing Australian FY CPI index: no interpolation or hybrid P&L.
  financial_vars <- c('ebitda', 'ebit', 'interest_financing_costs',
                      'net_financing_costs', 'finance_costs_gross',
                      'finance_income', 'one_off_financing_loss',
                      'abnormal_items', 'reported_ebit',
                      'profit_before_tax', 'accounting_income_tax_expense',
                      'net_income', 'ebitda_ebit_revenue_basis',
                      'net_income_revenue_basis')
  financial_amounts_annual <- financial_source_annual |>
    select(country, airport, year, period, passengers,
           adjustment_to_2025, fx_cad_per_unit, all_of(financial_vars)) |>
    pivot_longer(all_of(financial_vars), names_to = 'variable',
                 values_to = 'value_m') |>
    mutate(activity = 'total',
           amount_cad_2025_m = value_m * adjustment_to_2025 * fx_cad_per_unit,
           per_passenger_cad_2025 = amount_cad_2025_m * 1e6 / passengers)
  assert_unique(financial_amounts_annual,
                c('country', 'airport', 'year', 'variable'), 'new financial values')
  readr::write_csv(financial_amounts_annual,
                   file.path(out_dir, 'financial_annual_2023_2025.csv'))

  # The wide tables use V8-and-a-half's existing layout: total aero includes
  # Canadian AIF; Australian 'aero_revenue' is already the comparable total.
  # AIF and its derived aero total are NOT summed into total revenue twice.
  revenue_comparison_annual <- annual |>
    mutate(variable = case_when(
      country == 'Canada' & variable == 'aero_revenue' ~ 'aero_excl_aif',
      country == 'Canada' & variable == 'aero_plus_aif' ~ 'total_aero',
      country == 'Australia' & variable == 'aero_revenue' ~ 'total_aero',
      TRUE ~ variable
    )) |>
    transmute(country, airport, year, variable, amount_cad_2025_m)
  cost_comparison_annual <- cost_groups_annual |>
    # Financing comes from the audited source join above, not a second copy.
    filter(!variable %in% c('net_financing_costs', 'pension_remeasurement')) |>
    transmute(country, airport, year, variable, amount_cad_2025_m)
  financial_comparison_annual <- financial_amounts_annual |>
    transmute(country, airport, year, variable, amount_cad_2025_m)
  comparison_annual_long <- bind_rows(revenue_comparison_annual,
                                      cost_comparison_annual,
                                      financial_comparison_annual) |>
    arrange(country, airport, year, variable)
  assert_unique(comparison_annual_long,
                c('country', 'airport', 'year', 'variable'),
                'merged revenues/costs/financial comparison')
  readr::write_csv(comparison_annual_long,
                   file.path(out_dir, 'comparison_annual_2023_2025.csv'))

  # Reconcile the pre-existing cost-group D&A to the audited financial bridge.
  da_cost <- cost_groups_annual |>
    filter(variable == 'depreciation_amortisation') |>
    select(country, airport, year, cost_da_m = value_m)
  da_fin <- financial_source_annual |>
    select(country, airport, year, financial_da_m = depreciation_amortisation)
  da_check <- inner_join(da_cost, da_fin,
                         by = c('country', 'airport', 'year'))
  if (nrow(da_check) != 8L * length(recent_years) ||
      any(abs(da_check$cost_da_m - da_check$financial_da_m) > 0.03,
          na.rm = TRUE)) stop('D&A extraction and financial bridge disagree.')

  comparison_vars <- c(
    'total_revenue', 'total_aero', 'aero_excl_aif', 'aif_revenue',
    'commercial_revenue', 'parking_transport_revenue', 'parking_revenue',
    'landside_revenue', 'other_commercial_revenue',
    'operating_expenses', 'labour', 'purchased_services_other',
    'depreciation_amortisation', 'federal_ground_rent', 'pilt',
    'musqueam_yvr_agreement', 'asset_impairment',
    'ebitda', 'ebitda_margin_pct', 'ebit', 'ebit_margin_pct',
    'abnormal_items', 'reported_ebit',
    'interest_financing_costs', 'net_financing_costs',
    'finance_costs_gross', 'finance_income', 'one_off_financing_loss',
    'profit_before_tax', 'accounting_income_tax_expense', 'net_income',
    'net_profit_margin_pct', 'ebitda_ebit_revenue_basis',
    'net_income_revenue_basis'
  )
  airport_keys <- bind_rows(
    ports |> transmute(country = 'Canada', airport),
    tibble(country = 'Australia', airport = c('BNE', 'MEL', 'PER', 'SYD'))
  ) |> distinct()
  passenger_pools <- bind_rows(
    pass_df |> filter(year %in% recent_years) |>
      mutate(country = 'Canada') |>
      select(country, airport, year, passengers),
    aus_pass |>
      transmute(country = 'Australia', airport, year = fy_end(year), passengers) |>
      filter(year %in% recent_years)
  ) |>
    group_by(country, airport) |>
    summarise(years = n_distinct(year),
              pooled_passengers = if (years[1] == length(recent_years) &&
                                      all(is.finite(passengers)) &&
                                      all(passengers > 0)) sum(passengers)
                                  else NA_real_, .groups = 'drop')
  if (nrow(passenger_pools) != 8L ||
      anyNA(passenger_pools$pooled_passengers)) {
    stop('Eight complete airport passenger pools are required.')
  }
  # Include explicit NA cells for any missing annual measure (no partial
  # three-year pooling, and no sum(..., na.rm=TRUE) masquerading as complete).
  comparison_pooled_long <- tidyr::expand_grid(
    airport_keys, year = recent_years,
    variable = setdiff(comparison_vars,
                       c('ebitda_margin_pct', 'ebit_margin_pct',
                         'net_profit_margin_pct'))
  ) |>
    left_join(comparison_annual_long,
              by = c('country', 'airport', 'year', 'variable')) |>
    group_by(country, airport, variable) |>
    summarise(years_available = sum(is.finite(amount_cad_2025_m)),
              amount_cad_2025_m = if (years_available[1] == length(recent_years))
                                    sum(amount_cad_2025_m) else NA_real_,
              .groups = 'drop') |>
    left_join(passenger_pools |>
                select(country, airport, pooled_passengers),
              by = c('country', 'airport')) |>
    mutate(unit_absolute = 'million_2025_CAD',
           unit_pp = '2025_CAD_per_passenger',
           value_absolute = amount_cad_2025_m,
           value_pp = 1e6 * amount_cad_2025_m / pooled_passengers)

  # Ratios of pooled, annual-CPI-adjusted sums (never averages of ratios).
  financial_margin_annual <- financial_source_annual |>
    transmute(country, airport, year,
              ebitda_real = ebitda * adjustment_to_2025 * fx_cad_per_unit,
              ebit_real = ebit * adjustment_to_2025 * fx_cad_per_unit,
              operating_rev_real = ebitda_ebit_revenue_basis *
                adjustment_to_2025 * fx_cad_per_unit,
              net_real = net_income * adjustment_to_2025 * fx_cad_per_unit,
              net_rev_real = net_income_revenue_basis *
                adjustment_to_2025 * fx_cad_per_unit)
  complete_ratio <- function(num, den) {
    if (length(num) != length(recent_years) ||
        any(!is.finite(num)) || any(!is.finite(den)) || sum(den) <= 0)
      return(NA_real_)
    100 * sum(num) / sum(den)
  }
  financial_margins_pooled <- financial_margin_annual |>
    group_by(country, airport) |>
    summarise(ebitda_margin_pct = complete_ratio(ebitda_real, operating_rev_real),
              ebit_margin_pct = complete_ratio(ebit_real, operating_rev_real),
              net_profit_margin_pct = complete_ratio(net_real, net_rev_real),
              ebitda_n = sum(is.finite(ebitda_real) & is.finite(operating_rev_real)),
              ebit_n = sum(is.finite(ebit_real) & is.finite(operating_rev_real)),
              net_n = sum(is.finite(net_real) & is.finite(net_rev_real)),
              .groups = 'drop') |>
    pivot_longer(ends_with('_pct'), names_to = 'variable',
                 values_to = 'margin_pct') |>
    mutate(years_available = case_when(
      variable == 'ebitda_margin_pct' ~ as.integer(ebitda_n),
      variable == 'ebit_margin_pct' ~ as.integer(ebit_n),
      TRUE ~ as.integer(net_n))) |>
    left_join(passenger_pools |>
                select(country, airport, pooled_passengers),
              by = c('country', 'airport')) |>
    transmute(country, airport, variable, years_available,
              amount_cad_2025_m = NA_real_, pooled_passengers,
              unit_absolute = 'percent', unit_pp = 'percent',
              value_absolute = margin_pct, value_pp = margin_pct)
  comparison_pooled_long <- bind_rows(comparison_pooled_long,
                                      financial_margins_pooled)
  assert_unique(comparison_pooled_long,
                c('country', 'airport', 'variable'), 'pooled comparison')
  readr::write_csv(comparison_pooled_long,
                   file.path(out_dir, 'comparison_pooled_2023_2025.csv'))

  make_comparison_wide <- function(value_col) {
    wide <- comparison_pooled_long |>
      select(country, airport, variable, value = all_of(value_col)) |>
      pivot_wider(names_from = variable, values_from = value)
    for (nm in comparison_vars) {
      if (!nm %in% names(wide)) wide[[nm]] <- NA_real_
    }
    wide |>
      arrange(country, airport) |>
      select(country, airport, all_of(comparison_vars))
  }
  comparison_pp_wide <- make_comparison_wide('value_pp')
  comparison_absolute_wide <- make_comparison_wide('value_absolute')
  if (nrow(comparison_pp_wide) != 8L ||
      nrow(comparison_absolute_wide) != 8L ||
      !identical(comparison_pp_wide[c('country', 'airport')],
                 comparison_absolute_wide[c('country', 'airport')]) ||
      !identical(names(comparison_pp_wide), names(comparison_absolute_wide))) {
    stop('Wide companion tables have mismatched airports or variable ordering.')
  }
  # Every monetary wide cell must come from the SAME pooled sum in both tables.
  for (nm in setdiff(comparison_vars,
                     c('ebitda_margin_pct', 'ebit_margin_pct',
                       'net_profit_margin_pct'))) {
    original <- comparison_absolute_wide[[nm]]
    expected <- comparison_pp_wide[[nm]] * passenger_pools$pooled_passengers[
      match(paste(comparison_pp_wide$country, comparison_pp_wide$airport),
            paste(passenger_pools$country, passenger_pools$airport))] / 1e6
    valid <- is.finite(original) & is.finite(expected)
    if (any(abs(original[valid] - expected[valid]) > 1e-7) ||
        any(is.na(original) != is.na(expected))) {
      stop('Absolute and per-passenger values disagree: ', nm)
    }
  }
  for (nm in c('ebitda_margin_pct', 'ebit_margin_pct',
               'net_profit_margin_pct')) {
    if (!identical(comparison_pp_wide[[nm]],
                   comparison_absolute_wide[[nm]])) {
      stop('Pooled margins differ between companion tables: ', nm)
    }
  }
  readr::write_csv(comparison_pp_wide,
                   file.path(out_dir, 'comparison_pp_wide_2023_2025.csv'),
                   na = '')
  readr::write_csv(comparison_absolute_wide,
                   file.path(out_dir, 'comparison_absolute_wide_2023_2025.csv'),
                   na = '')
  readr::write_csv(comparison_pooled_long |>
                     filter(years_available != length(recent_years)) |>
                     select(country, airport, variable, years_available),
                   file.path(out_dir, 'comparison_incomplete_coverage_2023_2025.csv'))
  variable_dictionary <- tibble(variable = comparison_vars) |>
    mutate(unit_pp = if_else(stringr::str_ends(variable, '_pct'),
                             'percent', '2025_CAD_per_passenger'),
           unit_absolute = if_else(stringr::str_ends(variable, '_pct'),
                                    'percent', 'million_2025_CAD'),
           definition = case_when(
             variable == 'ebit' ~ 'Operating EBIT = EBITDA less depreciation/amortisation; excludes abnormal-items bridge',
             variable == 'reported_ebit' ~ 'ACCC reported EBIT including abnormal items; only for Australia',
             variable == 'interest_financing_costs' ~ 'Canada: net financing expense; ACCC: signed Interest line; Sydney: SCACH net finance costs',
             variable == 'net_financing_costs' ~ 'Net financing costs only where explicitly reported (Canada/SCACH)',
             variable == 'finance_costs_gross' ~ 'SCACH separately reported finance costs; missing elsewhere is not zero',
             variable == 'accounting_income_tax_expense' ~ 'Accounting tax expense (negative if tax benefit), NOT tax payable/cash paid',
             variable == 'net_income' ~ 'Reported after-tax income; SCACH for Sydney, ACCC for other Australian airports',
             variable == 'net_profit_margin_pct' ~ 'Pooled net income / pooled matching-entity net-income revenue (SCACH for Sydney)',
             variable %in% c('ebit_margin_pct', 'ebitda_margin_pct') ~ 'Pooled operating profit / pooled operating-entity revenue (ACCC regulatory for Australia)',
             variable == 'ebitda_ebit_revenue_basis' ~ 'Regulatory airport revenue for Australian operating margins; Canadian report revenue',
             variable == 'net_income_revenue_basis' ~ 'SCACH consolidated revenue for Sydney net margin; regulatory/Canadian revenue otherwise',
             variable == 'total_aero' ~ 'Canada aeronautical fees plus AIF; Australia ACCC aeronautical revenue',
             TRUE ~ 'Existing V8 revenue/cost item, or audited new financing/earnings line'
           ))
  readr::write_csv(variable_dictionary,
                   file.path(out_dir, 'comparison_variable_dictionary_2023_2025.csv'))
  message('V9: financial annual audit, pooled sums, and two companion wide tables saved.')
  message('Sydney: SCACH Jan–Dec 2023–2025 financing/net results; ACCC June FY operating results.')

}

message('Saved airport revenue series (2015–2025) and recent pooled comparison.')
message('Australian CPI: ABS ', abs_id,
        '; supplementary data were already in constant FY2024–25 AUD.')
if (nrow(missing_hist)) message('Check missing historical revenue in the audit CSV.')
print(growth_summary |> arrange(country, airport), n = Inf)

keep_vars <- c(
  "country", "airport",
  "total_revenue", "total_aero", "aero_excl_aif", "aif_revenue",
  "commercial_revenue", "parking_transport_revenue",
  "operating_expenses", "operating_minus_rent",
  "labour", "purchased_services_other",
  "depreciation_amortisation", "federal_ground_rent", "pilt",
  "ebitda", "ebitda_margin",
  "ebit", "ebit_margin",
  "interest_expense", "net_financing_costs",
  "profit_before_tax", "income_tax_expense",
  "net_profit_after_tax", "net_profit_margin"
)

comparison_pp_final <- comparison_pp_wide |>
  select(any_of(keep_vars))

comparison_absolute_final <- comparison_absolute_wide |>
  select(any_of(keep_vars))

keep_vars <- c(
  "country",
  "airport",
  "total_revenue",
  "total_aero",
  "aero_excl_aif",
  "aif_revenue",
  "commercial_revenue",
  "parking_transport_revenue",
  "operating_expenses",
  "operating_minus_rent",
  "labour",
  "purchased_services_other",
  "federal_ground_rent",
  "pilt",
  "ebitda",
  "ebitda_margin",
  "depreciation_amortisation",
  "asset_impairment",
  "ebit",
  "ebit_margin",
  "interest_financing_costs",
  "abnormal_items",
  "profit_before_tax",
  "accounting_income_tax_expense",
  "net_income",
  "net_profit_margin_pct"
)

comparison_pp_final <- comparison_pp_wide |>
  select(any_of(keep_vars))

comparison_absolute_final <- comparison_absolute_wide |>
  select(any_of(keep_vars))

export_fin <- list('Per-passenger' = comparison_pp_final,
                   'Absolute values' = comparison_absolute_final)

openxlsx::write.xlsx(export_fin, here(out_dir, "airport_revenue_expense_profit_comparison_23_25.xlsx"))

