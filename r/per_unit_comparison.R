# Canada–Australia airport financial comparisons | 2015–2025
#
# Compares aeronautical revenue per passenger over 2015–2025 and detailed
# revenue, operating costs, and profitability over 2023–2025.
#
# Canadian amounts come from airport annual reports. Australian revenue
# comes from the ACCC supplementary database, with regulatory accounts used
# for detailed costs and profitability. Sydney's below-operating results
# use SCACH consolidated statements.
#
# Monetary comparisons are in 2025 CAD. Canadian amounts are adjusted using
# Canadian CPI; ACCC supplementary amounts are already in 2024–25 AUD.
# Australian amounts use the fixed 2025 CAD/AUD rate. Canada is reported by
# calendar year and Australia by June-ending financial year.
#
# Outputs: annual aeronautical revenue growth, annual and pooled financial
# comparison CSVs, source provenance, and a reusable financial audit RDS.
# The separate workbook script assembles the final Excel workbook.
#
# Source notes: YYZ's 2015 AIF gross-up uses an estimated collection fee;
# missing historical values remain missing. Sydney uses ACCC operating
# results and SCACH calendar-year results below operating profit.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(janitor)
  library(cansim)
  library(readxl)
  library(readabs)
})

# ============================================================================
# 1. CONFIGURATION AND SOURCE FILES
# ============================================================================
base_year       <- 2025L
start_year      <- 2015L
long_years      <- start_year:base_year
recent_years    <- 2023:2025
cad_per_aud_2025 <- 0.9009 # Bank of Canada, 2025 annual average
# https://www.bankofcanada.ca/rates/exchange/annual-average-exchange-rates/

out_dir <- here('output', 'airport_comparison')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Use the cached ABS CPI series unless a refresh is requested.
refresh_abs_cpi <- FALSE
# Estimated airline collection fee for YYZ's 2015 AIF revenue.
assumed_yyz_2015_collection_fee <- 0.04

assert_unique <- function(data, cols, label) {
  dup <- data |> count(across(all_of(cols))) |> filter(n > 1)
  if (nrow(dup)) {
    print(dup, n = 20)
    stop('Duplicate keys in ', label, ': ', paste(cols, collapse = ', '), call. = FALSE)
  }
  invisible(data)
}

# 1.1 Input paths
# ----------------------------------------------------------------
can_recent_path <- here('output', 'big4_long.csv')
can_historical_path <- here('data', 'canada', 'operator_reports',
                            'canadian_big4_aeronautical_revenue_2015_2022.xlsx')
aus_fin_path <- here('output', 'accc_airport_financial_analysis_2007_2025.xlsx')
aus_pass_path <- here('data', 'australia', 'accc_airport_passengers_2007_2025.csv')

# ============================================================================
# 2. PASSENGER TRAFFIC AND PRICE ADJUSTMENTS
# ============================================================================
# 2.1 Canadian passenger counts
# ----------------------------------------------------------------
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

# 2.2 Canadian CPI (2025 price base)
# ----------------------------------------------------------------
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

# 2.3 Australian fiscal-year CPI
# ----------------------------------------------------------------
# ABS quarterly All-groups CPI, weighted average of eight capitals.
# Official ABS time-series ID A2325846C (historical quarterly data).
# Compute each financial-year CPI as the average of Sep, Dec, Mar and Jun
# quarters. 
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

# ============================================================================
# 3. CANADIAN REVENUE AND FINANCIAL DATA
# ============================================================================
# 3.1 Historical revenue, 2015–2022
# ----------------------------------------------------------------
# Read historical revenue using column positions in the source workbook.
hist <- readxl::read_excel(can_historical_path, sheet = 'Main 2015-2022')
names(hist)[1:7] <- c('airport', 'year', 'aero_k', 'aif_reported_k',
                      'grossup_k', 'total_reported_k', 'total_comparable_k')
hist <- hist |>
  mutate(
    airport = as.character(airport), year = as.integer(year),
    across(c(aero_k, aif_reported_k, grossup_k,
             total_reported_k, total_comparable_k), as.numeric),
    # Most years the AIF is net of the airline fee, but it is gross for YYZ in 2015-2017
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

# Check comparable aeronautical revenue against verified historical totals.
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
          '. These observations remain missing, not zero, in the revenue series.')
}

hist_long <- bind_rows(
  hist |> transmute(airport, year, item = 'Aeronautical Revenues',
                      category = 'revenue', variable = 'aero_revenue',
                      value_m = aero_m, grossup_method),
  hist |> transmute(airport, year, item = 'Airport Improvement Fees (gross basis)',
                      category = 'revenue', variable = 'aif_revenue',
                      value_m = aif_gross_m, grossup_method)
)

# 3.2 Recent annual reports, 2023–2025
# ----------------------------------------------------------------
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
can_recent <- read_csv(can_recent_path) |>
  transmute(
    airport = recode(as.character(airport), Pearson = 'YYZ', Calgary = 'YYC',
                     Montreal = 'YUL', Vancouver = 'YVR'),
    year = as.integer(year), item = as.character(item),
    category = as.character(category), value_m = as.numeric(value),
    variable = unname(can_lookup[as.character(item)]),
    grossup_method = 'none'
  ) |>
  filter(year %in% recent_years)

# Require one Canadian source observation per airport, year and measure.
can_source <- bind_rows(
  hist_long,
  can_recent |> select(airport, year, item, category, variable,
                         value_m, grossup_method)
) |>
  filter(!is.na(variable))
assert_unique(can_source, c('airport', 'year', 'variable'),
              'Canadian mapped source categories')

# Join Canadian revenue, passenger counts and CPI to calculate real values.
can_long <- can_source |>
  left_join(pass_df, by = c('airport', 'year')) |>
  left_join(can_cpi |> select(year, adjustment_to_2025), by = 'year') |>
  mutate(country = 'Canada', period = as.character(year),
         amount_cad_2025_m = value_m * adjustment_to_2025,
         per_passenger_cad_2025 = amount_cad_2025_m * 1e6 / passengers)

# Calculate aeronautical revenue including airport improvement fees (AIF).
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

# The Canadian non-aeronautical subtotal is reported by the operator.
# Subtract parking/ground transportation only when both components exist.
# Missing parking is not interpreted as zero.
can_residual <- can_long |>
  filter(year %in% recent_years,
         variable %in% c('commercial_revenue', 'parking_transport_revenue')) |>
  select(airport, year, period, passengers, adjustment_to_2025,
         variable, value_m) |>
  tidyr::pivot_wider(names_from = variable, values_from = value_m) |>
  mutate(
    value_m = commercial_revenue - parking_transport_revenue,
    country = 'Canada',
    item = 'Non-aeronautical revenue minus parking and ground transportation',
    category = 'derived', variable = 'non_aero_minus_parking',
    amount_cad_2025_m = value_m * adjustment_to_2025,
    per_passenger_cad_2025 = amount_cad_2025_m * 1e6 / passengers,
    grossup_method = 'none'
  ) |>
  select(country, airport, year, period, item, category, variable,
         value_m, passengers, amount_cad_2025_m,
         per_passenger_cad_2025, grossup_method)

can_values <- bind_rows(
  can_long |> select(country, airport, year, period, item, category,
                     variable, value_m, passengers, amount_cad_2025_m,
                     per_passenger_cad_2025, grossup_method),
  can_aif |> select(country, airport, year, period, item, category,
                    variable, value_m, passengers, amount_cad_2025_m,
                    per_passenger_cad_2025, grossup_method),
  can_residual
)

# ============================================================================
# 4. AUSTRALIAN REVENUE AND PASSENGER DATA
# ============================================================================
# 4.1 ACCC passenger counts
# ----------------------------------------------------------------
# Passenger observations are individual passengers.
aus_pass <- readr::read_csv(aus_pass_path, show_col_types = FALSE) |>
  select(airport, year, passengers) |>
  mutate(year = as.character(year), passengers = as.numeric(passengers)) |>
  distinct()
assert_unique(aus_pass, c('airport', 'year'), 'Australian passenger source')

# 4.2 ACCC supplementary financial data
# ----------------------------------------------------------------
aus_fin <- read_excel(aus_fin_path, sheet = 'financial') |>
  mutate(year = as.character(year))
mandatory_aus <- c('total_revenue', 'aero_revenue', 'parking_revenue',
                   'other_revenue')
if (!all(mandatory_aus %in% names(aus_fin))) {
  stop('ACCC supplementary financial export is missing: ',
       paste(setdiff(mandatory_aus, names(aus_fin)), collapse = ', '))
}
# A parking-and-ground-transportation subtotal is required for the
# explicit residual. A missing total is not reconstructed from parking alone.
if (!'parking_transport_revenue' %in% names(aus_fin)) {
  warning('ACCC financial workbook lacks parking_transport_revenue; ',
          'non_aero_minus_parking will remain missing.')
  aus_fin$parking_transport_revenue <- NA_real_
}
if (!'landside_revenue' %in% names(aus_fin)) {
  aus_fin$landside_revenue <- NA_real_
}
aus_fin <- aus_fin |>
  mutate(commercial_revenue = total_revenue - aero_revenue,
         non_aero_minus_parking =
           commercial_revenue - parking_transport_revenue)

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

# ============================================================================
# 5. LONG-RUN AERONAUTICAL REVENUE COMPARISON
# ============================================================================
compare_vars <- c('total_revenue', 'aero_revenue', 'aero_plus_aif',
                  'aif_revenue', 'commercial_revenue',
                  'parking_transport_revenue', 'parking_revenue',
                  'landside_revenue', 'non_aero_minus_parking')

annual_all <- bind_rows(can_values, aus_long) |>
  filter(variable %in% compare_vars, !is.na(value_m), passengers > 0) |>
  select(country, airport, year, period, variable, value_m,
         amount_cad_2025_m, passengers, per_passenger_cad_2025,
         grossup_method) |>
  arrange(country, airport, variable, year)
assert_unique(annual_all, c('country', 'airport', 'year', 'variable'),
              'cross-country annual revenue')

# Require complete Australian aeronautical revenue coverage.
if (nrow(annual_all |> filter(country == 'Australia',
                              variable == 'aero_revenue')) !=
    4L * length(long_years)) {
  stop('Incomplete Australian 2015–2025 aeronautical-revenue panel.')
}

# Index aeronautical revenue per passenger to 2015 for each airport.
# Canadian revenue includes AIF; Australian aeronautical revenue is reported total.
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

# Calculate growth where both endpoint years are available.
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

# Retain 2023–2025 revenue observations for the detailed comparison.
annual <- annual_all |>
  filter(year %in% recent_years) |>
  select(country, airport, year, period, variable,
         amount_cad_2025_m, passengers, per_passenger_cad_2025)

# ============================================================================
# 6. OPERATING COSTS AND PROFITABILITY, 2023–2025
# ============================================================================
# 6.1 Operating expense groups
# ----------------------------------------------------------------
reg_ebitda_path <- here('output', 'accc_airport_ebitda_by_activity_2022_2025.csv')
reg_exp_groups_path <- here('output', 'accc_airport_expenditure_groups_2022_2025.csv')
reg_ebitda <- readr::read_csv(reg_ebitda_path, show_col_types = FALSE) |>
  filter(year %in% recent_years) |>
  left_join(aus_cpi |> select(year, adjustment_to_2025), by = 'year') |>
  mutate(ebitda_real_aud_m = ebitda * adjustment_to_2025,
         ebitda_cad_2025_m = ebitda_real_aud_m * cad_per_aud_2025)
assert_unique(reg_ebitda, c('airport', 'year', 'activity'),
              'ACCC regulatory EBITDA')
reg_total <- reg_ebitda |> filter(activity == 'total')
if (nrow(reg_total) != 4L * length(recent_years) ||
    anyNA(reg_total$ebitda_real_aud_m)) {
  stop('Incomplete 2023–25 regulatory EBITDA.')
}
reg_exp_groups <- readr::read_csv(reg_exp_groups_path, show_col_types = FALSE) |>
  filter(year %in% recent_years)

# Convert Australian expense groups to 2025 CAD and per-passenger values.
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

assert_unique(cost_groups_annual,
              c('country', 'airport', 'year', 'variable'),
              'harmonized cost categories')

# 6.2 Operating profit and net-income source data
# ----------------------------------------------------------------
# Use ACCC operating results for Sydney and SCACH consolidated results
# for financing, pretax income, tax, and net income.
required_reg <- c('ebitda', 'operating_profit', 'depreciation_amortisation',
                  'revenue', 'interest_financing_costs', 'profit_before_tax',
                  'income_tax_expense', 'net_profit_after_tax', 'abnormal_items',
                  'ebit_reported')
if (!all(required_reg %in% names(reg_total))) {
  stop('Run the ACCC regulatory EBITDA extraction first. Missing: ',
       paste(setdiff(required_reg, names(reg_total)), collapse = ', '))
}

# Read verified SCACH income-statement inputs.
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

# Calculate SCACH net finance costs from costs, finance income,
# swap fair-value movements, and early-redemption losses.
if (any(abs(scach$finance_costs - scach$finance_income -
            scach$swap_fair_value_change + scach$rps_early_redemption_loss -
            scach$net_finance_costs) > 0.15)) {
  stop('SCACH net financing components failed reconciliation.')
}

scach_join <- scach |>
  select(year, all_of(scach_require), source_url, source_note,
         reporting_period) |>
  rename_with(~ paste0('scach_', .x), -year)

# For Canadian airport authorities, use net income as pretax income and
# zero income-tax expense. EBIT deducts D&A and identified impairments.
# 6.3 Standardize Canadian financial measures
# ----------------------------------------------------------------
can_fin_vars <- c('total_revenue', 'ebitda_can',
                  'depreciation_amortisation', 'asset_impairment',
                  'net_financing_costs', 'net_income_can',
                  'profit_before_tax_can', 'income_tax_expense_can')
can_fin_raw <- can_long |>
  filter(year %in% recent_years, variable %in% can_fin_vars) |>
  select(airport, year, variable, value_m) |>
  pivot_wider(names_from = variable, values_from = value_m)
for (nm in can_fin_vars) {
  if (!nm %in% names(can_fin_raw)) can_fin_raw[[nm]] <- NA_real_
}
can_fin <- tidyr::expand_grid(airport = ports$airport, year = recent_years) |>
  left_join(can_fin_raw, by = c('airport', 'year')) |>
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
    accounting_income_tax_expense = 0,
    net_income = net_income_can,
    ebitda_ebit_revenue_basis = total_revenue,
    net_income_revenue_basis = total_revenue,
    financial_source = 'Big 4 long / Canadian airport annual reports',
    financial_period = as.character(year),
    financing_definition = 'Reported financing costs NET of interest income; classification differs by operator',
    net_income_definition = 'Reported Canadian annual-report net income',
    scach_source_url = NA_character_)

# 6.4 Standardize Australian financial measures
# ----------------------------------------------------------------
aus_financial <- reg_total |>
  left_join(aus_pass |>
              transmute(airport, year = fy_end(year), passengers),
            by = c('airport', 'year')) |>
  left_join(scach_join, by = 'year') |>
  transmute(
    country = 'Australia', airport, year, period = fyear, passengers,
    adjustment_to_2025, fx_cad_per_unit = cad_per_aud_2025,
    ebitda,
    # ACCC operating profit already includes D&A expense.
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

# 6.5 Combine and validate the eight-airport financial panel
# ----------------------------------------------------------------
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
# Check reported net income against pretax income less accounting tax expense.
tax_bridge <- financial_source_annual |>
  filter(!is.na(profit_before_tax),
         !is.na(accounting_income_tax_expense), !is.na(net_income)) |>
  mutate(residual = profit_before_tax - accounting_income_tax_expense - net_income)
if (any(abs(tax_bridge$residual) > 0.05)) {
  print(tax_bridge |> filter(abs(residual) > 0.05))
  stop('Reported net profit does not reconcile to pretax profit less tax.')
}
# Retain original ACCC and SCACH source tables for auditing.
# ============================================================================
# 7. ANNUAL FINANCIAL COMPARISON TABLES
# ============================================================================
# 7.1 Convert financial amounts to 2025 CAD
# ----------------------------------------------------------------
# Apply the annual price adjustments and fixed 2025 AUD/CAD rate.
# Sydney's calendar-year amounts use the corresponding Australian FY CPI factor.
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

# Align aeronautical revenue definitions: Canadian airport revenue including
# AIF corresponds to Australian total aeronautical revenue.
# 7.2 Combine annual revenue, cost and profit measures
# ----------------------------------------------------------------
revenue_comparison_annual <- annual |>
  mutate(variable = case_when(
    country == 'Canada' & variable == 'aero_revenue' ~ 'aero_excl_aif',
    country == 'Canada' & variable == 'aero_plus_aif' ~ 'total_aero',
    country == 'Australia' & variable == 'aero_revenue' ~ 'total_aero',
    TRUE ~ variable
  )) |>
  transmute(country, airport, year, variable, amount_cad_2025_m)

cost_comparison_annual <- cost_groups_annual |>
  # Use financing costs from the financial source panel.
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

# 7.3 Check depreciation and amortization
# ----------------------------------------------------------------
# Compare D&A extracted for operating costs with D&A used in the EBIT bridge.
# The two amounts must match for all 24 airport-years (tolerance: 0.03 million).
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

# 7.4 Define variables and passenger pools
# ----------------------------------------------------------------
comparison_vars <- c(
  'total_revenue', 'total_aero', 'aero_excl_aif', 'aif_revenue',
  'commercial_revenue', 'parking_transport_revenue', 'parking_revenue',
  'landside_revenue', 'non_aero_minus_parking',
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

# 7.5 Pool three years of complete financial observations
# ----------------------------------------------------------------
# Pool monetary amounts only when all three annual observations exist.
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

# 7.6 Annual and three-year pooled profitability margins
# ----------------------------------------------------------------
# Pooled margins divide pooled profit by pooled revenue.
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

# Use annual passenger counts for annual per-passenger comparisons;
# profitability margins are percentages in both output formats.
# 7.7 Build the annual per-passenger and absolute tables
# ----------------------------------------------------------------
margin_vars <- c('ebitda_margin_pct', 'ebit_margin_pct',
                 'net_profit_margin_pct')

comparison_annual_complete <- tidyr::expand_grid(
  airport_keys, year = recent_years,
  variable = setdiff(comparison_vars, margin_vars)
) |>
  left_join(comparison_annual_long,
            by = c('country', 'airport', 'year', 'variable')) |>
  left_join(financial_source_annual |>
              select(country, airport, year, passengers),
            by = c('country', 'airport', 'year')) |>
  mutate(value_absolute = amount_cad_2025_m,
         value_pp = amount_cad_2025_m * 1e6 / passengers)

annual_margins <- financial_margin_annual |>
  transmute(country, airport, year,
            # EBITDA margin: EBITDA / operating revenue
            ebitda_margin_pct = if_else(
              is.finite(ebitda_real) & is.finite(operating_rev_real) &
                operating_rev_real > 0,
              100 * ebitda_real / operating_rev_real, NA_real_),
            # EBIT margin: EBIT / operating revenue
            ebit_margin_pct = if_else(
              is.finite(ebit_real) & is.finite(operating_rev_real) &
                operating_rev_real > 0,
              100 * ebit_real / operating_rev_real, NA_real_),
            # Net after-tax profit margin: Net profit / net total revenue
            net_profit_margin_pct = if_else(
              is.finite(net_real) & is.finite(net_rev_real) & net_rev_real > 0,
              100 * net_real / net_rev_real, NA_real_)) |>
  pivot_longer(all_of(margin_vars), names_to = 'variable',
               values_to = 'value') |>
  transmute(country, airport, year, variable,
            value_absolute = value, value_pp = value)

comparison_annual_complete <- bind_rows(
  comparison_annual_complete |>
    select(country, airport, year, variable, value_absolute, value_pp),
  annual_margins
)

assert_unique(comparison_annual_complete,
              c('country', 'airport', 'year', 'variable'),
              'annual comparison')

# Pivot to wide for for excel workbook
make_comparison_wide <- function(data, value_col) {
  keys <- intersect(c('country', 'airport', 'year'), names(data))
  wide <- data |>
    select(all_of(keys), variable, value = all_of(value_col)) |>
    pivot_wider(names_from = variable, values_from = value)
  for (nm in comparison_vars) {
    if (!nm %in% names(wide)) wide[[nm]] <- NA_real_
  }
  wide |>
    arrange(across(all_of(keys))) |>
    select(all_of(keys), all_of(comparison_vars))
}
comparison_pp_wide <- make_comparison_wide(comparison_pooled_long, 'value_pp')
comparison_absolute_wide <- make_comparison_wide(comparison_pooled_long, 'value_absolute')
comparison_annual_pp_wide <- make_comparison_wide(comparison_annual_complete, 'value_pp')
comparison_annual_absolute_wide <- make_comparison_wide(comparison_annual_complete, 'value_absolute')

# Column name checks
if (nrow(comparison_annual_pp_wide) != 8L * length(recent_years) ||
    !identical(comparison_annual_pp_wide[c('country', 'airport', 'year')],
               comparison_annual_absolute_wide[c('country', 'airport', 'year')]) ||
    !identical(names(comparison_annual_pp_wide),
               names(comparison_annual_absolute_wide))) {
  stop('Annual wide tables have mismatched keys or variable ordering.')
}
# Verify annual absolute values equal per-passenger values times passengers.
# 8.1 Check per-passenger and absolute values
# ----------------------------------------------------------------
annual_check <- comparison_annual_pp_wide |>
  select(country, airport, year) |>
  left_join(financial_source_annual |>
              select(country, airport, year, passengers),
            by = c('country', 'airport', 'year'))

# Check calculated per-passenger totals with actual totals
for (nm in setdiff(comparison_vars, margin_vars)) {
  original <- comparison_annual_absolute_wide[[nm]]
  expected <- comparison_annual_pp_wide[[nm]] * annual_check$passengers / 1e6
  valid <- is.finite(original) & is.finite(expected)
  if (any(abs(original[valid] - expected[valid]) > 1e-7) ||
      any(is.na(original) != is.na(expected))) {
    stop('Annual absolute and per-passenger values disagree: ', nm)
  }
}

# Make sure the margins are the same in both tables
for (nm in margin_vars) {
  if (!identical(comparison_annual_pp_wide[[nm]],
                 comparison_annual_absolute_wide[[nm]])) {
    stop('Annual margins differ between companion tables: ', nm)
  }
}

# Check airports and column ordering in wide format tables
if (nrow(comparison_pp_wide) != 8L ||
    nrow(comparison_absolute_wide) != 8L ||
    !identical(comparison_pp_wide[c('country', 'airport')],
               comparison_absolute_wide[c('country', 'airport')]) ||
    !identical(names(comparison_pp_wide), names(comparison_absolute_wide))) {
  stop('Wide companion tables have mismatched airports or variable ordering.')
}

# Check pooled absolute and per-passenger tables against the pooled sums.
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

# ============================================================================
# 8. VALIDATE AND PREPARE COMPARISON OUTPUTS
# ============================================================================
# Select the measures for annual and pooled output tables.
# 8.2 Select output variables
# ----------------------------------------------------------------
keep_vars <- c(
  'total_revenue', 'total_aero', 'aero_excl_aif', 'aif_revenue',
  'commercial_revenue', 'parking_transport_revenue',
  'non_aero_minus_parking', 'operating_expenses', 'labour', 'purchased_services_other',
  'federal_ground_rent', 'pilt',
  'ebitda', 'ebitda_margin_pct',
  'depreciation_amortisation', 'asset_impairment',
  'ebit', 'ebit_margin_pct',
  'interest_financing_costs', 'abnormal_items',
  'profit_before_tax', 'accounting_income_tax_expense',
  'net_income', 'net_profit_margin_pct'
)

comparison_pooled_pp_final <- comparison_pp_wide |>
  select(country, airport, all_of(keep_vars))
comparison_pooled_absolute_final <- comparison_absolute_wide |>
  select(country, airport, all_of(keep_vars))
comparison_annual_pp_final <- comparison_annual_pp_wide |>
  select(country, airport, year, all_of(keep_vars))
comparison_annual_absolute_final <- comparison_annual_absolute_wide |>
  select(country, airport, year, all_of(keep_vars))

# Validate the residual in both presentations, for annual and pooled periods.
check_non_aero_residual <- function(d, label) {
  has_components <- is.finite(d$commercial_revenue) &
    is.finite(d$parking_transport_revenue)
  if (!all(is.finite(d$non_aero_minus_parking) == has_components) ||
      any(abs(d$non_aero_minus_parking[has_components] -
              (d$commercial_revenue[has_components] -
               d$parking_transport_revenue[has_components])) > 1e-6)) {
    stop('Non-aeronautical-minus-parking reconciliation fails: ', label,
         call. = FALSE)
  }
}
check_non_aero_residual(comparison_annual_absolute_final, 'annual totals')
check_non_aero_residual(comparison_annual_pp_final, 'annual per passenger')
check_non_aero_residual(comparison_pooled_absolute_final, 'pooled totals')
check_non_aero_residual(comparison_pooled_pp_final, 'pooled per passenger')

# 8.3 Write annual and pooled CSV outputs
# ----------------------------------------------------------------
export_fin <- list(
  'Pooled per-passenger' = comparison_pooled_pp_final,
  'Pooled absolute' = comparison_pooled_absolute_final,
  'Annual per-passenger' = comparison_annual_pp_final,
  'Annual absolute' = comparison_annual_absolute_final
)

openxlsx::write.xlsx(
  export_fin,
  file.path(out_dir, 'airport_revenue_expense_profit_comparison_23_25.xlsx')
)
readr::write_csv(comparison_pooled_pp_final,
                 file.path(out_dir, 'comparison_pooled_pp_2023_2025.csv'))
readr::write_csv(comparison_pooled_absolute_final,
                 file.path(out_dir, 'comparison_pooled_absolute_2023_2025.csv'))
readr::write_csv(comparison_annual_pp_final,
                 file.path(out_dir, 'comparison_annual_pp_2023_2025.csv'))
readr::write_csv(comparison_annual_absolute_final,
                 file.path(out_dir, 'comparison_annual_absolute_2023_2025.csv'))
readr::write_csv(aero_series,
                 file.path(out_dir, 'aero_revenue_growth_2015_2025.csv'))

message('Saved annual and pooled comparison tables, workbook, and counterfactual revenue series in ', out_dir)
message('Australian CPI: ABS ', abs_id,
        '; supplementary data were already in constant FY2024–25 AUD.')
message('Sydney: SCACH Jan–Dec 2023–2025 financing/net results; ACCC June FY operating results.')
print(growth_summary |> arrange(country, airport), n = Inf)


# ============================================================================
# 9. DOCUMENT THE SOURCE OF EACH COMPARISON MEASURE
# ============================================================================
# Track the source URL for each airport, year and comparison measure.

# Canadian annual reports
# 9.1 Canadian annual-report references
# ----------------------------------------------------------------
can_sources <- readr::read_csv(
  can_recent_path, show_col_types = FALSE
) |>
  transmute(
    airport = recode(
      airport,
      Pearson = 'YYZ', Calgary = 'YYC',
      Montreal = 'YUL', Vancouver = 'YVR'
    ),
    year = as.integer(year),
    can_url = source_url
  ) |>
  distinct()

assert_unique(can_sources, c('airport', 'year'),
              'Canadian source URLs')

# ACCC supplementary financial data
# 9.2 ACCC supplementary data references
# ----------------------------------------------------------------
sup_sources <- readr::read_csv(
  here('output', 'accc_financial_provenance.csv'),
  show_col_types = FALSE
) |>
  transmute(
    airport,
    year = fy_end(year),
    source_variable = variable,
    sup_url = source_url,
    sup_sheet = source_sheet
  )

assert_unique(
  sup_sources, c('airport', 'year', 'source_variable'),
  'ACCC supplementary provenance'
)

# Match comparison measures to ACCC supplementary source variables.
sup_map <- tribble(
  ~variable,                    ~source_variable,
  'total_revenue',              'total_revenue',
  'total_aero',                 'aero_revenue',
  'commercial_revenue',         'total_revenue',
  'parking_transport_revenue',  'parking_transport_revenue',
  'non_aero_minus_parking',     'total_revenue'
)

sup_sources <- sup_sources |>
  inner_join(sup_map, by = 'source_variable') |>
  select(airport, year, variable, sup_url, sup_sheet)

# ACCC regulatory accounts
# 9.3 ACCC regulatory-account references
# ----------------------------------------------------------------
reg_sources <- reg_total |>
  transmute(
    airport, year,
    reg_url = source_url,
    reg_sheet = source_sheet
  )

assert_unique(reg_sources, c('airport', 'year'),
              'ACCC regulatory provenance')

# SCACH consolidated income statements (Sydney only)
# 9.4 Sydney consolidated income-statement references
# ----------------------------------------------------------------
scach_sources <- scach |>
  transmute(
    year,
    scach_url = source_url
  )

# Sydney measures sourced from SCACH.
scach_vars <- c(
  'interest_financing_costs',
  'profit_before_tax',
  'accounting_income_tax_expense',
  'net_income',
  'net_profit_margin_pct'
)

# Attach a source URL and worksheet to each annual comparison measure.
# 9.5 Match each annual measure to its source
# ----------------------------------------------------------------
provenance_annual <- comparison_annual_complete |>
  filter(variable %in% keep_vars) |>
  transmute(
    country, airport, year, variable,
    has_value = is.finite(value_absolute)
  ) |>
  left_join(can_sources, by = c('airport', 'year')) |>
  left_join(sup_sources, by = c('airport', 'year', 'variable')) |>
  left_join(reg_sources, by = c('airport', 'year')) |>
  left_join(scach_sources, by = 'year') |>
  mutate(
    source_url = case_when(
      country == 'Canada' ~ can_url,
      country == 'Australia' &
        airport == 'SYD' &
        variable %in% scach_vars ~ scach_url,
      country == 'Australia' &
        variable %in% sup_map$variable ~ sup_url,
      country == 'Australia' ~ reg_url
    ),
    source_sheet = case_when(
      country == 'Australia' &
        airport == 'SYD' &
        variable %in% scach_vars ~
        'SCACH consolidated financial statements',
      country == 'Australia' &
        variable %in% sup_map$variable ~ sup_sheet,
      country == 'Australia' ~ reg_sheet,
      TRUE ~ NA_character_
    ),
    # Leave source references blank for unavailable measures.
    source_url = if_else(has_value, source_url, NA_character_),
    source_sheet = if_else(has_value, source_sheet, NA_character_)
  ) |>
  select(country, airport, year, variable,
         has_value, source_url, source_sheet) |>
  arrange(country, airport, year, variable)

assert_unique(
  provenance_annual,
  c('country', 'airport', 'year', 'variable'),
  'Annual comparison provenance'
)

if (any(provenance_annual$has_value &
        (is.na(provenance_annual$source_url) |
         !startsWith(coalesce(provenance_annual$source_url, ''),
                     'https://')))) {
  stop('Missing source URL for a populated comparison measure.')
}

readr::write_csv(
  provenance_annual |> select(-has_value),
  file.path(out_dir, 'comparison_sources_annual_2023_2025.csv')
)

# ============================================================================
# 10. SAVE INTERMEDIATE AUDIT DATA
# ============================================================================
saveRDS(
  list(
    annual_all = annual_all,
    cost_groups_annual = cost_groups_annual,
    financial_amounts_annual = financial_amounts_annual,
    comparison_annual_long = comparison_annual_long,
    annual_margins = annual_margins,
    comparison_pooled_long = comparison_pooled_long,
    financial_source_annual = financial_source_annual,
    pass_df = pass_df,
    aus_pass = aus_pass,
    can_cpi = can_cpi,
    aus_cpi = aus_cpi,
    passenger_pools = passenger_pools
  ),
  file.path(out_dir, 'financial_comparison_audit_2023_2025.rds')
)



