# Airport finances: documented comparison workbook, 2023-2025
# ------------------------------------------------------------
# Run from the root of the rromard/airports project, after
# reshape_big4_long.R, parse_accc_airports.R,
# accc_regulatory_ebitda.R, and per_unit_comparison.R.
#
# Produces ten worksheets, ordered for reading: comparisons, calculations,
# supporting sources, methodology, reconciliation. Supporting sheets carry
# descriptive source identifiers, file types and clickable links. The
# comparison sheets stay wide, source-free, with annual and pooled values.
# Excel formulas use multiline key-based lookups and simple arithmetic, left to right.
# Reconciliation compares Excel formulas with independent R CSV benchmarks.
# XLOOKUP selects identifiable records; key uniqueness is also checked in R.
# Primary calculated outputs have visually distinct column headings.
# The final comparison tables remain authoritative R results, not Excel rewrites.
#
# IMPORTANT: This exporter does not change the financial model. It reads the
# four authoritative comparison CSVs and saved audit objects. Excel formulas
# demonstrate the calculations, while final wide tables preserve R values.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(openxlsx)
})

# PATHS AND SETTINGS ------------------------------------------------------
root <- here::here()
out_dir <- here::here('output', 'airport_comparison')
output_file <- file.path(out_dir, 'airport_financial_comparison_documented_2023_2025.xlsx')

paths <- list(
  annual_absolute = file.path(out_dir, 'comparison_annual_absolute_2023_2025.csv'),
  annual_pp = file.path(out_dir, 'comparison_annual_pp_2023_2025.csv'),
  pooled_absolute = file.path(out_dir, 'comparison_pooled_absolute_2023_2025.csv'),
  pooled_pp = file.path(out_dir, 'comparison_pooled_pp_2023_2025.csv'),
  provenance = file.path(out_dir, 'comparison_sources_annual_2023_2025.csv'),
  audit = file.path(out_dir, 'financial_comparison_audit_2023_2025.rds'),
  canada = here::here('output', 'big4_long.csv'),
  supplementary = here::here('output', 'accc_financial_provenance.csv'),
  regulatory = here::here('output', 'accc_airport_ebitda_by_activity_2022_2025.csv'),
  australia_passengers = here::here('data', 'australia',
                                   'accc_airport_passengers_2007_2025.csv')
)

missing_paths <- names(paths)[!file.exists(unlist(paths))]
if (length(missing_paths)) {
  stop('Missing input(s) for workbook build: ',
       paste(missing_paths, collapse = ', '), '\n',
       paste(unlist(paths[missing_paths]), collapse = '\n'), '\n',
       'Run the upstream scripts and add the audit-export patch first.',
       call. = FALSE)
}

read_input <- function(path) readr::read_csv(path, show_col_types = FALSE)
assert_cols <- function(x, cols, label) {
  absent <- setdiff(cols, names(x))
  if (length(absent)) stop(label, ' is missing: ', paste(absent, collapse = ', '))
  invisible(x)
}
assert_unique <- function(x, cols, label) {
  duplicates <- x |>
    dplyr::count(dplyr::across(dplyr::all_of(cols))) |>
    dplyr::filter(n > 1L)
  if (nrow(duplicates)) {
    print(duplicates, n = 20)
    stop('Duplicate keys in ', label, call. = FALSE)
  }
  invisible(x)
}
within_tolerance <- function(a, b, tol = 1e-7) {
  same_missing <- is.na(a) == is.na(b)
  same_value <- is.na(a) | is.na(b) | abs(a - b) <= tol
  same_missing & same_value
}

# AUTHORITATIVE FINAL COMPARISON VALUES -----------------------------------
annual_abs <- read_input(paths$annual_absolute)
annual_pp <- read_input(paths$annual_pp)
pooled_abs <- read_input(paths$pooled_absolute)
pooled_pp <- read_input(paths$pooled_pp)

assert_cols(annual_abs, c('country', 'airport', 'year'), 'Annual totals')
assert_cols(annual_pp, c('country', 'airport', 'year'), 'Annual per passenger')
assert_cols(pooled_abs, c('country', 'airport'), 'Pooled totals')
assert_cols(pooled_pp, c('country', 'airport'), 'Pooled per passenger')

keep_vars <- setdiff(names(annual_abs), c('country', 'airport', 'year'))
if (!identical(names(annual_abs), names(annual_pp)) ||
    !identical(names(pooled_abs), names(pooled_pp)) ||
    !setequal(keep_vars, setdiff(names(pooled_abs), c('country', 'airport')))) {
  stop('The four comparison CSVs have incompatible measure columns.')
}
assert_unique(annual_abs, c('country', 'airport', 'year'), 'Annual totals')
assert_unique(annual_pp, c('country', 'airport', 'year'), 'Annual per passenger')
assert_unique(pooled_abs, c('country', 'airport'), 'Pooled totals')
assert_unique(pooled_pp, c('country', 'airport'), 'Pooled per passenger')
if (nrow(annual_abs) != 24L || nrow(pooled_abs) != 8L) {
  stop('Expected 24 annual and 8 pooled airport observations.')
}

# Preserve exact numeric cells from the four existing CSVs. In the final
# sheets, 'year' stays numeric for annual rows and blank for the pooled row.
make_final_wide <- function(annual, pooled) {
  bind_rows(
    annual |>
      mutate(period_type = 'annual', .before = year),
    pooled |>
      mutate(period_type = 'pooled 2023-25', year = NA_integer_,
             .after = airport) |>
      relocate(period_type, .before = year)
  ) |>
    arrange(country, airport, factor(period_type,
                                     levels = c('annual', 'pooled 2023-25')),
            year) |>
    select(country, airport, period_type, year, all_of(keep_vars))
}
# Rename only the reader-facing workbook measure: the upstream R and CSV
# variables stay as commercial_revenue to preserve the validated pipeline.
# Both final analysis sheets expose the new, more accurate column key.
final_totals <- make_final_wide(annual_abs, pooled_abs) |>
  rename(non_aero_revenue = commercial_revenue)
final_pp <- make_final_wide(annual_pp, pooled_pp) |>
  rename(non_aero_revenue = commercial_revenue)

# PROVENANCE --------------------------------------------------------------
provenance <- read_input(paths$provenance) |>
  mutate(year = as.integer(year))
assert_cols(provenance,
            c('country', 'airport', 'year', 'variable', 'source_url', 'source_sheet'),
            'Annual provenance')
assert_unique(provenance, c('country', 'airport', 'year', 'variable'),
              'Annual provenance')

# SOURCE INPUT SHEETS -----------------------------------------------------
can_inputs <- read_input(paths$canada) |>
  transmute(
    country = 'Canada',
    airport = recode(as.character(airport),
                     Pearson = 'YYZ', Calgary = 'YYC',
                     Montreal = 'YUL', Vancouver = 'YVR'),
    year = as.integer(year),
    measure = as.character(item), category,
    value = as.numeric(value),
    unit = 'nominal CAD millions',
    # These rows are imported from the Canadian compilation as supplied.
    # Do not claim a subtotal was calculated in this R pipeline.
    value_type = 'reported',
    calculation = if_else(measure == 'Goods and services',
      'Goods and services as reported in Canadian airport financial statements',
      NA_character_),
    source_sheet = 'Big 4 standard (compilation)',
    source_url
  ) |>
  filter(year %in% 2023:2025) |>
  arrange(airport, year, category, measure)
# Preserve all rows in the intern compilation, even if an item label repeats.

sup_inputs <- read_input(paths$supplementary) |>
  mutate(year = as.character(year)) |>
  transmute(
    country = 'Australia', airport,
    year = as.integer(2000 + as.integer(str_extract(year, '\\d{2}$'))),
    measure = as.character(variable),
    value = as.numeric(value),
    unit = 'real 2024-25 AUD millions',
    value_type = if_else(
      measure %in% c('parking_transport_revenue', 'other_revenue',
                     'other_expenses', 'other_profit'),
      'derived', 'reported'
    ),
    calculation = case_when(
      measure == 'parking_transport_revenue' ~
        'parking_revenue + landside_revenue',
      measure == 'other_revenue' ~
        'total_revenue - aero_revenue - parking_transport_revenue',
      measure == 'other_expenses' ~
        'total_expenses - aero_expenses - parking_expenses',
      measure == 'other_profit' ~
        'total_profit - aero_profit - parking_profit',
      TRUE ~ NA_character_
    ),
    source_sheet, source_url
  ) |>
  filter(year %in% 2023:2025) |>
  arrange(airport, year, measure)
assert_unique(sup_inputs, c('airport', 'year', 'measure'),
              'ACCC supplementary input records')

reg_raw <- read_input(paths$regulatory) |>
  filter(year %in% 2023:2025)
assert_cols(reg_raw,
            c('airport', 'year', 'fyear', 'activity',
              'source_file', 'source_sheet', 'source_url'),
            'ACCC regulatory records')
reg_original_vars <- c(
  'revenue', 'expenditure', 'salaries_wages', 'services_utilities',
  'property_maintenance', 'security_costs', 'consultants_advisors',
  'general_administration', 'other_costs', 'depreciation_land',
  'depreciation_other', 'amortisation_intangibles',
  'operating_profit_reported', 'abnormal_items', 'ebit_reported',
  'interest_reported', 'profit_before_tax', 'tax_charge_reported',
  'net_profit_after_tax', 'dividends_paid'
)
reg_derived_vars <- c(
  'depreciation_amortisation', 'purchased_services_other',
  'operating_profit', 'ebitda', 'ebit', 'net_profit_margin'
)
# Standardising the sign of a reported expense is not deriving a financial
# measure. These are reported ACCC lines in expense-positive presentation.
reg_standardised_reported <- c('interest_financing_costs', 'income_tax_expense')
reg_vars <- intersect(
  c(reg_original_vars, reg_standardised_reported, reg_derived_vars),
  names(reg_raw)
)
reg_inputs <- reg_raw |>
  select(airport, year, fyear, activity, source_file,
         source_sheet, source_url, all_of(reg_vars)) |>
  pivot_longer(all_of(reg_vars), names_to = 'measure', values_to = 'value') |>
  mutate(
    country = 'Australia',
    unit = if_else(measure == 'net_profit_margin',
                   'percent', 'nominal AUD millions'),
    value_type = if_else(measure %in% c(reg_original_vars,
                                          reg_standardised_reported),
                         'reported', 'derived'),
    calculation = case_when(
      measure == 'depreciation_amortisation' ~
        'depreciation_land + depreciation_other + amortisation_intangibles',
      measure == 'purchased_services_other' ~
        paste0('Sum of non-labour operating expense categories; ',
               'broadly equivalent to Canadian Goods and services'),
      measure == 'operating_profit' ~ 'revenue - expenditure',
      measure == 'ebitda' ~ 'operating_profit + depreciation_amortisation',
      measure == 'ebit' ~ 'operating_profit (excludes abnormal items)',
      measure == 'net_profit_margin' ~
        '100 * net_profit_after_tax / revenue',
      TRUE ~ NA_character_
    )
  ) |>
  # Display sign-standardised reported expenses as reported observations.
  mutate(measure = recode(measure,
    interest_financing_costs = 'Interest expense (reported)',
    income_tax_expense = 'Income tax expense (reported)',
    purchased_services_other = 'Goods and services')) |>
  select(country, airport, year, fyear, activity, measure, value, unit,
         value_type, calculation, source_file, source_sheet, source_url) |>
  arrange(airport, year, activity, measure)
assert_unique(reg_inputs, c('airport', 'year', 'activity', 'measure'),
              'ACCC regulatory input records')

# SAVED AUDIT OBJECTS -----------------------------------------------------
audit <- readRDS(paths$audit)
required_objects <- c(
  'annual_all', 'cost_groups_annual', 'financial_amounts_annual',
  'comparison_annual_long', 'annual_margins',
  'comparison_pooled_long', 'pass_df', 'aus_pass',
  'can_cpi', 'aus_cpi', 'passenger_pools', 'financial_source_annual'
)
if (!is.list(audit) || !all(required_objects %in% names(audit))) {
  stop('Audit RDS is incomplete. Missing objects: ',
       paste(setdiff(required_objects, names(audit)), collapse = ', '))
}

# ECONOMIC INPUTS: PASSENGERS, CPI, FX ------------------------------------
traffic_can <- audit$pass_df |>
  filter(year %in% 2023:2025) |>
  transmute(country = 'Canada', airport, year, measure = 'passengers',
            value = as.numeric(passengers), unit = 'passengers',
            value_type = 'reported', calculation = NA_character_,
            source_sheet = 'Statistics Canada 23-10-0253-01',
            source_url = paste0(
              'https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=',
              '2310025301'))

aus_pass_raw <- read_input(paths$australia_passengers)
assert_cols(aus_pass_raw, c('airport', 'year', 'passengers',
                            'source_url', 'source_sheet'),
            'ACCC passenger records')
traffic_aus <- aus_pass_raw |>
  transmute(country = 'Australia', airport,
            year = as.integer(2000 + as.integer(str_extract(
              as.character(year), '\\d{2}$'))),
            measure = 'passengers',
            value = as.numeric(passengers), unit = 'passengers',
            value_type = 'reported', calculation = NA_character_,
            source_sheet, source_url) |>
  filter(year %in% 2023:2025)

can_cpi_input <- audit$can_cpi |>
  filter(year %in% 2023:2025) |>
  transmute(country = 'Canada', airport = NA_character_, year,
            measure = 'cpi_annual_index', value = cpi_original,
            unit = 'index', value_type = 'reported',
            calculation = NA_character_,
            source_sheet = 'Statistics Canada 18-10-0005-01',
            source_url = paste0(
              'https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=',
              '1810000501'))
can_cpi_factor <- audit$can_cpi |>
  filter(year %in% 2023:2025) |>
  transmute(country = 'Canada', airport = NA_character_, year,
            measure = 'adjustment_to_2025', value = adjustment_to_2025,
            unit = 'factor', value_type = 'derived',
            calculation = '2025 Canadian CPI / source-year Canadian CPI',
            source_sheet = 'Statistics Canada 18-10-0005-01',
            source_url = paste0(
              'https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=',
              '1810000501'))

# Direct Table 1 spreadsheet from the June quarter 2025 release. This is
# the reference edition used to document ABS series A2325846C.
abs_url <- paste0(
  'https://www.abs.gov.au/statistics/economy/',
  'price-indexes-and-inflation/consumer-price-index-australia/',
  'jun-quarter-2025/640101.xlsx'
)
aus_cpi_input <- audit$aus_cpi |>
  filter(year %in% 2023:2025) |>
  transmute(country = 'Australia', airport = NA_character_, year,
            measure = 'cpi_fy_index', value = cpi_original,
            unit = 'index', value_type = 'derived',
            calculation = 'mean of four quarterly ABS CPI index values in the FY',
            source_sheet = 'ABS Table 1, series A2325846C', source_url = abs_url)
aus_cpi_factor <- audit$aus_cpi |>
  filter(year %in% 2023:2025) |>
  transmute(country = 'Australia', airport = NA_character_, year,
            measure = 'adjustment_to_2025', value = adjustment_to_2025,
            unit = 'factor', value_type = 'derived',
            calculation = 'FY2024-25 CPI / source-year Australian FY CPI',
            source_sheet = 'ABS Table 1, series A2325846C', source_url = abs_url)

fx_value <- audit$financial_source_annual |>
  filter(country == 'Australia') |>
  pull(fx_cad_per_unit) |>
  unique()
if (length(fx_value) != 1L || !is.finite(fx_value)) {
  stop('Expected a single 2025 CAD/AUD conversion factor.')
}
fx_input <- tibble(
  country = 'Australia', airport = NA_character_, year = 2025L,
  measure = 'CAD_per_AUD_2025', value = fx_value,
  unit = 'CAD per AUD', value_type = 'reported',
  calculation = '2025 Bank of Canada annual average; applied to all years',
  source_sheet = 'Bank of Canada annual average exchange rates',
  source_url = 'https://www.bankofcanada.ca/rates/exchange/annual-average-exchange-rates/'
)

traffic_factors <- bind_rows(
  traffic_can, traffic_aus, can_cpi_input, can_cpi_factor,
  aus_cpi_input, aus_cpi_factor, fx_input
) |>
  arrange(country, airport, year, measure)
assert_unique(traffic_factors, c('country', 'airport', 'year', 'measure'),
              'Traffic, CPI and FX inputs')

# CALCULATION AUDIT: ANNUAL RECORDS --------------------------------------
# Reuse the ORIGINAL source amounts and adjusted amounts from the analysis.
# Only harmonize variable names already harmonized in that analysis.
revenue_audit <- audit$annual_all |>
  filter(year %in% 2023:2025) |>
  mutate(
    variable = case_when(
      country == 'Canada' & variable == 'aero_revenue' ~ 'aero_excl_aif',
      country == 'Canada' & variable == 'aero_plus_aif' ~ 'total_aero',
      country == 'Australia' & variable == 'aero_revenue' ~ 'total_aero',
      TRUE ~ variable
    ),
    source_stream = 'revenue'
  ) |>
  transmute(country, airport, year, period, variable, source_stream,
            source_amount_m = value_m, passengers,
            amount_cad_2025_m, per_passenger_cad_2025)

cost_audit <- audit$cost_groups_annual |>
  filter(!variable %in% c('net_financing_costs', 'pension_remeasurement')) |>
  transmute(country, airport, year, period, variable,
            source_stream = 'costs', source_amount_m = value_m,
            passengers, amount_cad_2025_m, per_passenger_cad_2025)

financial_audit <- audit$financial_amounts_annual |>
  transmute(country, airport, year, period, variable,
            source_stream = 'profitability', source_amount_m = value_m,
            passengers, amount_cad_2025_m, per_passenger_cad_2025)

calculation_amounts <- bind_rows(revenue_audit, cost_audit, financial_audit)
assert_unique(calculation_amounts,
              c('country', 'airport', 'year', 'variable'),
              'Annual source-amount calculations')

# These must match the already validated annual comparison amounts exactly.
comparison_amounts <- audit$comparison_annual_long |>
  select(country, airport, year, variable,
         comparison_amount_m = amount_cad_2025_m)
if (nrow(anti_join(comparison_amounts, calculation_amounts,
                    by = c('country', 'airport', 'year', 'variable'))) ||
    nrow(anti_join(calculation_amounts, comparison_amounts,
                    by = c('country', 'airport', 'year', 'variable')))) {
  stop('Source-amount audit has a different set of comparison keys.')
}
check_amounts <- calculation_amounts |>
  left_join(comparison_amounts,
            by = c('country', 'airport', 'year', 'variable'))
if (nrow(check_amounts) != nrow(comparison_amounts) ||
    !all(within_tolerance(check_amounts$amount_cad_2025_m,
                          check_amounts$comparison_amount_m))) {
  stop('Audit calculation amounts do not match comparison_annual_long.')
}

factors <- bind_rows(
  audit$can_cpi |>
    transmute(country = 'Canada', year,
              annual_cpi_factor = adjustment_to_2025),
  audit$aus_cpi |>
    transmute(country = 'Australia', year,
              annual_cpi_factor = adjustment_to_2025)
)

calculation_amounts <- calculation_amounts |>
  left_join(factors, by = c('country', 'year')) |>
  mutate(
    source_basis = case_when(
      country == 'Canada' ~ 'nominal CAD millions',
      source_stream == 'revenue' ~ 'real 2024-25 AUD millions',
      TRUE ~ 'nominal AUD millions'
    ),
    cpi_factor = if_else(country == 'Australia' & source_stream == 'revenue',
                         1, annual_cpi_factor),
    fx_cad_per_unit = if_else(country == 'Canada', 1, fx_value),
    # This is the model's existing conversion identity; it is an audit
    # CHECK ONLY, not a replacement for its reported adjusted amounts.
    converted_check = source_amount_m * cpi_factor * fx_cad_per_unit
  )
if (!all(within_tolerance(calculation_amounts$converted_check,
                          calculation_amounts$amount_cad_2025_m,
                          tol = 1e-6))) {
  stop('Source amount x CPI x FX differs from the analysis output.')
}

# Value provenance: 'reported' means the measure is carried as a source
# observation by the analysis, not that every source compilation subtotal
# was independently confirmed in this exporter.
value_type_for <- function(country, airport, variable) {
  case_when(
    variable %in% c('ebitda_margin_pct', 'ebit_margin_pct',
                    'net_profit_margin_pct') ~ 'derived',
    country == 'Canada' & variable == 'accounting_income_tax_expense' ~
      'assumed',
    country == 'Canada' & variable %in% c('total_aero', 'ebit',
                                         'profit_before_tax') ~ 'derived',
    country == 'Australia' & variable %in% c(
      'commercial_revenue', 'parking_transport_revenue',
      'other_commercial_revenue', 'operating_expenses',
      'purchased_services_other', 'depreciation_amortisation',
      'ebitda', 'ebit') ~ 'derived',
    TRUE ~ 'reported'
  )
}
formula_for <- function(country, airport, variable) {
  case_when(
    variable == 'total_aero' & country == 'Canada' ~
      'aero_excl_aif + aif_revenue',
    variable == 'commercial_revenue' & country == 'Australia' ~
      'total_revenue - total_aero',
    variable == 'parking_transport_revenue' & country == 'Australia' ~
      'parking_revenue + landside_revenue',
    variable == 'other_commercial_revenue' & country == 'Australia' ~
      'total_revenue - total_aero - parking_transport_revenue',
    variable == 'operating_expenses' & country == 'Australia' ~
      'regulatory expenditure - depreciation_amortisation',
    variable == 'purchased_services_other' & country == 'Australia' ~
      'sum of regulatory non-labour, non-depreciation expense items',
    variable == 'depreciation_amortisation' & country == 'Australia' ~
      'three regulatory depreciation/amortisation expense lines',
    variable == 'ebitda' & country == 'Australia' ~
      'operating profit + depreciation_amortisation',
    variable == 'ebit' & country == 'Canada' ~
      'ebitda - depreciation_amortisation - asset_impairment',
    variable == 'ebit' & country == 'Australia' ~
      'operating profit (after D&A, excludes abnormal items)',
    variable == 'profit_before_tax' & country == 'Canada' ~
      'net_income (Canadian tax-exempt airport authority convention)',
    variable == 'accounting_income_tax_expense' & country == 'Canada' ~
      'assumed zero; NOT a separately reported tax charge',
    variable == 'ebitda_margin_pct' ~
      '100 * EBITDA / matched operating revenue',
    variable == 'ebit_margin_pct' ~
      '100 * EBIT / matched operating revenue',
    variable == 'net_profit_margin_pct' ~
      '100 * net income / matched revenue basis',
    TRUE ~ NA_character_
  )
}

calculation_amounts <- calculation_amounts |>
  mutate(
    period_type = 'annual',
    value_type = value_type_for(country, airport, variable),
    calculation = formula_for(country, airport, variable),
    value_absolute = amount_cad_2025_m,
    value_pp = per_passenger_cad_2025,
    unit_absolute = 'million_2025_CAD',
    unit_pp = '2025_CAD_per_passenger'
  ) |>
  select(country, airport, period_type, year, period, variable,
         value_type, calculation, source_stream, source_basis,
         source_amount_m, cpi_factor, fx_cad_per_unit,
         amount_cad_2025_m, passengers,
         value_absolute, value_pp, unit_absolute, unit_pp)

margin_audit <- audit$annual_margins |>
  left_join(
    audit$financial_source_annual |>
      select(country, airport, year, period, passengers),
    by = c('country', 'airport', 'year')
  ) |>
  transmute(
    country, airport, period_type = 'annual', year, period, variable,
    value_type = 'derived',
    calculation = formula_for(country, airport, variable),
    source_stream = 'profitability ratio',
    source_basis = 'ratio of matching adjusted income statement amounts',
    source_amount_m = NA_real_, cpi_factor = NA_real_,
    fx_cad_per_unit = NA_real_, amount_cad_2025_m = NA_real_,
    passengers, value_absolute, value_pp,
    unit_absolute = 'percent', unit_pp = 'percent'
  )

calculations_annual <- bind_rows(calculation_amounts, margin_audit) |>
  filter(variable %in% keep_vars) |>
  left_join(provenance,
            by = c('country', 'airport', 'year', 'variable')) |>
  # Model conventions are documented, not falsely attributed to reports.
  mutate(source_url = if_else(value_type == 'assumed',
                              NA_character_, source_url))
assert_unique(calculations_annual,
              c('country', 'airport', 'year', 'variable'),
              'Calculations annual')

# Ensure every populated annual final cell is represented in the calculation
# audit, including missing-value patterns. Margins use percent as printed,
# not fractions (58.9 means 58.9%).
check_final_annual <- function(final_csv, audit_col) {
  left <- final_csv |>
    pivot_longer(all_of(keep_vars), names_to = 'variable',
                 values_to = 'final_value')
  right <- calculations_annual |>
    select(country, airport, year, variable,
           calculated_value = all_of(audit_col))
  joined <- left |>
    left_join(right, by = c('country', 'airport', 'year', 'variable'))
  if (nrow(joined) != nrow(left) ||
      !all(within_tolerance(joined$final_value,
                            joined$calculated_value, tol = 1e-6))) {
    stop('Calculation audit differs from final annual ', audit_col, ' CSV.')
  }
}
check_final_annual(annual_abs, 'value_absolute')
check_final_annual(annual_pp, 'value_pp')

# Every populated annual final cell must have an underlying source or an
# explicitly documented model assumption. The URL was collected upstream.
missing_links <- calculations_annual |>
  filter(is.finite(value_absolute), value_type != 'assumed',
         is.na(source_url) | !str_detect(source_url, '^https?://'))
if (nrow(missing_links)) {
  print(missing_links |>
          select(country, airport, year, variable, value_type), n = 30)
  stop('Missing source links for populated annual calculation records.')
}

# POOLED CALCULATION RECORDS ----------------------------------------------
# NO second set of annual URLs. A pooled result is a derived aggregate of
# three annual records; the annual audit supplies its three source links.
pooled_audit <- audit$comparison_pooled_long |>
  filter(variable %in% keep_vars) |>
  transmute(
    country, airport, period_type = 'pooled', year = NA_integer_,
    period = '2023-25 pooled', variable,
    value_type = 'derived',
    calculation = if_else(
      str_ends(variable, '_margin_pct'),
      '100 * sum(annual adjusted profit) / sum(annual adjusted revenue)',
      'sum(annual adjusted values); divide by sum(passengers) for per-pax'
    ),
    source_stream = 'pooled analysis',
    source_basis = '2025 CAD / three-year sum',
    source_amount_m = NA_real_, cpi_factor = NA_real_,
    fx_cad_per_unit = NA_real_,
    amount_cad_2025_m,
    passengers = pooled_passengers,
    value_absolute, value_pp, unit_absolute, unit_pp,
    source_url = NA_character_,
    source_sheet = 'See three annual records for this airport and variable'
  )
assert_unique(pooled_audit, c('country', 'airport', 'variable'),
              'Pooled calculation records')

check_pooled <- function(final_csv, audit_col) {
  left <- final_csv |>
    pivot_longer(all_of(keep_vars), names_to = 'variable',
                 values_to = 'final_value')
  right <- pooled_audit |>
    select(country, airport, variable,
           calculated_value = all_of(audit_col))
  joined <- left |> left_join(right, by = c('country', 'airport', 'variable'))
  if (nrow(joined) != nrow(left) ||
      !all(within_tolerance(joined$final_value,
                            joined$calculated_value, tol = 1e-6))) {
    stop('Pooled values differ from authoritative ', audit_col, ' CSV.')
  }
}
check_pooled(pooled_abs, 'value_absolute')
check_pooled(pooled_pp, 'value_pp')

calculations <- bind_rows(calculations_annual, pooled_audit) |>
  arrange(country, airport, variable, year, period_type) |>
  select(country, airport, period_type, year, period,
         variable, value_type, calculation, source_stream, source_basis,
         source_amount_m, cpi_factor, fx_cad_per_unit,
         amount_cad_2025_m, passengers, value_absolute, value_pp,
         unit_absolute, unit_pp, source_sheet, source_url)

# Workbook-only key aliases. Each XLOOKUP source and reconciliation key
# uses these same keys; the authoritative source CSVs and R audit remain intact.
workbook_measure_key <- function(x) {
  recode(as.character(x),
         commercial_revenue = 'non_aero_revenue',
         purchased_services_other = 'Goods and services')
}

# READER-FACING EXCEL CALCULATION TABLES -------------------------------
# The existing audit/reconciliation objects above remain unchanged. From
# here onward we build plain spreadsheet tables: a formula in a calculated
# column uses the same operation down every row. Financial measures that
# already exist in the source data are not redefined just to add formulas.
margin_vars <- c('ebitda_margin_pct', 'ebit_margin_pct',
                 'net_profit_margin_pct')
amount_vars <- setdiff(keep_vars, margin_vars)
# The two income-statement revenue bases are also needed to make margins
# transparent, especially Sydney's distinct calendar-year net revenue.
calculation_vars <- c(amount_vars, 'ebitda_ebit_revenue_basis',
                      'net_income_revenue_basis')
calc_source <- calculation_amounts |>
  filter(variable %in% calculation_vars) |>
  arrange(country, airport, year, match(variable, calculation_vars))
assert_unique(calc_source, c('country', 'airport', 'year', 'variable'),
              'Displayed annual calculation keys')

# Data inputs only; blank result cells are populated with Excel formulas
# below. Source currencies and CPI conventions are explained in Methodology.
calc_annual <- calc_source |>
  transmute(
    country, airport, year,
    measure = workbook_measure_key(variable),
    original_amount_m = source_amount_m,
    source_price_basis = if_else(
      country == 'Australia' & source_stream == 'revenue',
      'Already inflation-adjusted', 'Nominal'
    ),
    cpi_factor,
    inflation_adjusted_m = NA_real_,
    fx_cad_per_unit,
    adjusted_amount_m = NA_real_,
    passengers,
    per_passenger_cad_2025 = NA_real_
  )

# Row selection is deliberately delegated to the Excel lookup formulas.
# Matching is by country, airport, year and measure, not R-computed rows.

# The pooled block uses three annual formula results. It stays on the same
# worksheet: CPI and FX are *never* applied again to these pooled amounts.
calc_pooled_source <- audit$comparison_pooled_long |>
  filter(variable %in% amount_vars) |>
  arrange(country, airport, match(variable, amount_vars))
calc_pooled <- calc_pooled_source |>
  transmute(
    country, airport,
    measure = workbook_measure_key(variable),
    amount_2023_m = NA_real_, amount_2024_m = NA_real_,
    amount_2025_m = NA_real_, pooled_amount_m = NA_real_,
    passengers_2023 = NA_real_, passengers_2024 = NA_real_,
    passengers_2025 = NA_real_, pooled_passengers = NA_real_,
    pooled_per_passenger = NA_real_
  )

# Unlike SUMIFS, XLOOKUP returns the first matching value. Verify that
# every key is unique before exporting its source table to the workbook.
assert_unique(calc_annual, c('country', 'airport', 'year', 'measure'),
              'Annual Excel XLOOKUP sources')
assert_unique(calc_pooled, c('country', 'airport', 'measure'),
              'Pooled Excel XLOOKUP sources')

# The annual profitability bridge shows the numbers actually used by the
# comparison: source EBITDA, D&A, and impairment -> operating EBIT. PBT,
# income-tax expense, and net income are carried from reported/assumed
# figures; they are NOT silently treated as reconstructed earnings.
profit_source <- audit$financial_source_annual |>
  filter(year %in% 2023:2025) |>
  arrange(country, airport, year)
profit_annual <- profit_source |>
  transmute(
    country, airport, year,
    ebitda_m = NA_real_, depreciation_amortisation_m = NA_real_,
    asset_impairment_m = NA_real_, operating_ebit_m = NA_real_,
    operating_revenue_m = NA_real_,
    ebitda_margin_pct = NA_real_, ebit_margin_pct = NA_real_,
    interest_financing_m = NA_real_, profit_before_tax_m = NA_real_,
    income_tax_expense_m = NA_real_, net_income_m = NA_real_,
    net_revenue_m = NA_real_, net_profit_margin_pct = NA_real_
  )

# One pooled row per airport and margin. Visible numerator/denominator keys
# identify exactly which annual amounts are retrieved by Excel. Pooled
# margins divide three-year summed numerators by summed denominators.
profit_pooled <- tidyr::expand_grid(
  profit_source |> distinct(country, airport),
  measure = margin_vars
) |>
  arrange(country, airport, match(measure, margin_vars)) |>
  transmute(
    country, airport,
    margin = recode(measure,
      ebitda_margin_pct = 'EBITDA margin',
      ebit_margin_pct = 'EBIT margin',
      net_profit_margin_pct = 'Net profit margin'),
    numerator_measure = recode(measure,
      ebitda_margin_pct = 'ebitda',
      ebit_margin_pct = 'ebit',
      net_profit_margin_pct = 'net_income'),
    denominator_measure = if_else(measure == 'net_profit_margin_pct',
                                  'net_income_revenue_basis',
                                  'ebitda_ebit_revenue_basis'),
    numerator_2023_m = NA_real_, numerator_2024_m = NA_real_,
    numerator_2025_m = NA_real_, pooled_numerator_m = NA_real_,
    denominator_2023_m = NA_real_, denominator_2024_m = NA_real_,
    denominator_2025_m = NA_real_, pooled_denominator_m = NA_real_,
    pooled_margin_pct = NA_real_
  )
assert_unique(profit_annual, c('country', 'airport', 'year'),
              'Annual profit Excel XLOOKUP sources')
assert_unique(profit_pooled, c('country', 'airport', 'margin'),
              'Pooled profit Excel XLOOKUP sources')

# Sydney's calendar-year consolidated financing/net-income figures do NOT
# come from ACCC's regulatory workbooks. Keep their document links in the
# input audit, even though the calculation worksheets have no source columns.
scach_vars <- c(
  'interest_financing_costs', 'finance_costs_gross', 'finance_income',
  'one_off_financing_loss', 'profit_before_tax',
  'accounting_income_tax_expense', 'net_income',
  'net_income_revenue_basis'
)
scach_inputs <- audit$financial_source_annual |>
  filter(country == 'Australia', airport == 'SYD', year %in% 2023:2025) |>
  select(airport, year, scach_source_url, all_of(scach_vars)) |>
  pivot_longer(all_of(scach_vars), names_to = 'measure',
               values_to = 'value') |>
  transmute(
    country = 'Australia', airport, year, fyear = as.character(year),
    activity = 'SCACH consolidated (calendar year)',
    measure, value, unit = 'nominal AUD millions',
    value_type = 'reported', calculation = NA_character_,
    source_file = NA_character_,
    source_sheet = 'SCACH consolidated income statement',
    source_url = scach_source_url
  )
if (nrow(scach_inputs) != 3L * length(scach_vars) ||
    anyNA(scach_inputs$source_url)) {
  stop('Sydney SCACH inputs or annual document links are incomplete.')
}
reg_inputs <- bind_rows(reg_inputs, scach_inputs) |>
  arrange(airport, year, activity, measure)
assert_unique(reg_inputs, c('airport', 'year', 'activity', 'measure'),
              'Australian regulatory and SCACH input records')

# 'source_sheet' is the upstream field name. Reader-facing worksheets use
# 'source_detail' because a reference may identify an Excel tab, report
# section or statistical series, rather than necessarily a worksheet.
can_inputs <- can_inputs |> rename(source_detail = source_sheet)
sup_inputs <- sup_inputs |> rename(source_detail = source_sheet)
reg_inputs <- reg_inputs |>
  rename(source_detail = source_sheet) |>
  select(-source_file)
traffic_factors <- traffic_factors |> rename(source_detail = source_sheet)

# Each distinct URL has a single, stable short ID and source type across
# all supporting worksheets. Source IDs are labels, not new provenance data.
source_observations <- bind_rows(
  can_inputs |>
    transmute(source_url, airport, year, source_group = 'Canadian finance'),
  sup_inputs |>
    transmute(source_url, airport, year, source_group = 'Supplementary'),
  reg_inputs |>
    transmute(source_url, airport, year, source_group = 'Regulatory'),
  traffic_factors |>
    transmute(source_url, airport, year, source_group = 'Statistics')
) |>
  filter(!is.na(source_url), nzchar(source_url)) |>
  mutate(source_url = stringr::str_trim(source_url)) |>
  arrange(source_url, factor(source_group, levels = c(
    'Canadian finance', 'Supplementary', 'Regulatory',
    'Statistics', 'Calculation')), year) |>
  distinct(source_url, .keep_all = TRUE)

source_catalog <- source_observations |>
  mutate(
    url_lc = stringr::str_to_lower(source_url),
    source_id = case_when(
      str_detect(url_lc, fixed('2024-25-supplementary-database.xlsx')) ~
        'AUS-ACCC-SUPP-2025',
      str_detect(url_lc, fixed('2022-23-airport-monitoring-report-regulatory-accounts.xlsx')) ~
        'AUS-ACCC-REG-2023',
      str_detect(url_lc, fixed('2023-24-regulatory-accounts-revised-perth-data')) ~
        'AUS-ACCC-REG-2024-PER-REV',
      str_detect(url_lc, fixed('2023-24-regulatory-accounts.xlsx')) ~
        'AUS-ACCC-REG-2024',
      str_detect(url_lc, fixed('2024-25-regulatory-accounts_0.xlsx')) ~
        'AUS-ACCC-REG-2025',
      str_detect(url_lc, fixed('jun-quarter-2025/640101.xlsx')) ~
        'AUS-ABS-CPI-2025',
      str_detect(url_lc, fixed('pid=1810000501')) ~
        'CAN-STATCAN-CPI',
      str_detect(url_lc, fixed('pid=2310025301')) ~
        'CAN-STATCAN-TRAFFIC',
      str_detect(url_lc, fixed('bankofcanada.ca/rates/exchange')) ~
        'CAN-BOC-FX-2025',
      str_detect(url_lc, 'admtl\\.com/.*/publications') ~
        'CAN-YUL-PUBLICATIONS',
      str_detect(url_lc, fixed('torontopearson.com')) ~
        paste0('CAN-YYZ-', year),
      str_detect(url_lc, fixed('yyc.com')) ~
        paste0('CAN-YYC-', year),
      str_detect(url_lc, 'yvr\\.ca') ~
        paste0('CAN-YVR-', year),
      str_detect(url_lc, fixed('admtl.com')) ~
        paste0('CAN-YUL-', year),
      str_detect(url_lc, fixed('links.sgx.com')) ~
        paste0('AUS-SCACH-', year),
      TRUE ~ sprintf('SOURCE-%03d', row_number())
    ),
    source_type = case_when(
      str_detect(url_lc, '\\.(xlsx|xls)([?#]|$)') ~ 'Excel',
      str_detect(url_lc, '\\.(pdf)([?#]|$)') ~ 'PDF',
      # Some known PDF download endpoints do not end in .pdf.
      str_detect(url_lc, fixed('torontopearson.com')) ~ 'PDF',
      str_detect(url_lc, 'admtl\\.com/file\\?') ~ 'PDF',
      str_detect(url_lc, fixed('links.sgx.com/fileopen/')) ~ 'PDF',
      TRUE ~ 'Webpage'
    )
  ) |>
  select(source_url, source_id, source_type)
assert_unique(source_catalog, 'source_url', 'Source URL catalog')
assert_unique(source_catalog, 'source_id', 'Source identifiers')

append_source_labels <- function(x) {
  original_count <- nrow(x)
  x <- x |>
    mutate(source_url = na_if(str_trim(source_url), '')) |>
    left_join(source_catalog, by = 'source_url')
  if (nrow(x) != original_count ||
      any(!is.na(x$source_url) & is.na(x$source_id))) {
    stop('Source identification did not cover all source URLs.')
  }
  # Keep identification next to the link at the right edge of each sheet.
  core_columns <- setdiff(names(x),
                          c('source_id', 'source_type',
                            'source_detail', 'source_url'))
  x |>
    select(all_of(core_columns), source_id, source_type,
           source_detail, source_url)
}
can_inputs <- append_source_labels(can_inputs)
sup_inputs <- append_source_labels(sup_inputs)
reg_inputs <- append_source_labels(reg_inputs)
traffic_factors <- append_source_labels(traffic_factors)

# METHODOLOGY AND VARIABLE DICTIONARY -------------------------------------
methodology <- tribble(
  ~topic, ~detail,
  'Scope',
    'Four Canadian airports (YYC, YUL, YVR, YYZ) and four Australian airports (BNE, MEL, PER, SYD); years 2023-2025.',
  'Reporting years',
    'Canada uses calendar years. ACCC accounts use June-ending financial years, labelled by ending year.',
  'Monetary units',
    'Final annual and pooled totals are in millions of constant 2025 CAD; per-passenger figures are 2025 CAD per passenger.',
  'Source price basis',
    'Nominal means source-year dollars requiring a CPI adjustment. Already inflation-adjusted means Australian ACCC supplementary financial figures expressed in FY2024-25 AUD; CPI factor applied = 1.',
  'Calculation sheet units',
    'Annual original, inflation-adjusted and converted amounts are in millions of the relevant currency; converted amounts and pooled amounts are in millions of 2025 CAD. Per-passenger values are 2025 CAD per passenger.',
  'Canadian inflation',
    'Source nominal CAD multiplied by 2025 Canadian CPI / source-year Canadian CPI.',
  'Australian supplementary financials',
    'Amounts already adjusted by the ACCC to FY2024-25 AUD millions; calculated residuals are identified. No additional CPI adjustment; convert once using 2025 CAD/AUD.',
  'Australian regulatory accounts',
    'Source nominal AUD multiplied by Australian FY2024-25 CPI / source-year FY CPI, then by 2025 CAD/AUD.',
  'Currency conversion',
    'The Bank of Canada reports the 2025 average CAD per AUD (0.9009 in the current model). Applying that same rate to all three years is the chosen conversion convention.',
  'Per-passenger measures',
    'Adjusted annual amount (CAD millions) times 1,000,000 divided by the corresponding annual passenger count.',
  'Pooled per passenger',
    'Sum of three adjusted annual amounts times 1,000,000 / sum of three annual passenger counts; no partial pooling.',
  'Pooled totals',
    'Sum of three adjusted annual 2025 CAD amounts. A missing year makes the pooled measure missing.',
  'Profit margins',
    'Ratios of matched operating or net-income amounts and denominators. Pooled margins are ratios of three-year sums.',
  'Canadian AIF',
    'Aeronautical revenue including AIF is a separate derived measure; AIF is not added again to total revenue.',
  'Sydney below-operating results',
    'Selected financing, pretax profit, tax and net-income measures use SCACH consolidated calendar-year statements. Operating measures use June-ending ACCC sources.',
  'Source audit',
    'Reported = source observation, including sign standardisation and unit changes; derived = a new measure calculated from financial components; assumed = an analytical convention (for example, Canadian zero income-tax expense). Source IDs identify repeated documents.',
  'Canadian input classifications',
    'Canadian financial input rows are imported as supplied by the Big 4 compilation. The workbook does not reclassify an imported subtotal as derived unless the analysis actually computes it.',
  'Commercial and other revenue',
    'The workbook uses non_aero_revenue for the upstream commercial_revenue measure. Canada uses the reported commercial/non-aeronautical subtotal, including interest income and contributions (interest income is minor); Australia uses ACCC total revenue less aeronautical revenue.',
  'Reconciliation',
    'The Reconciliation sheet compares independently generated R CSV benchmarks with values calculated by Excel on the two calculation sheets. Check both annual and pooled values; both-missing is distinct from a numeric pass. Excel must recalculate formulas when the workbook is opened.',
  'Inputs versus presentation',
    'The two final wide worksheets reproduce the numerical comparison CSVs and intentionally omit source links.',
  'Zero Canadian income tax',
    'Canadian airport authorities are assigned zero accounting income-tax expense as an analytical convention; do not treat this as a reported charge.',
  'Missing values',
    'Blank means unavailable or inapplicable, not zero. Derived and pooled measures do not substitute zero for missing source values.',
  'Reported expense signs',
    'Australian reported interest and income-tax expenses are shown as positive charges where the original regulatory accounts use negative signs. Sign standardisation does not make them derived measures.',
  'Australian CPI source',
    'ABS Consumer Price Index, June quarter 2025, Table 1, All-groups CPI, series A2325846C. Each financial-year CPI averages September, December, March and June quarters.',
  'Calculation sheets',
    'Financial calculations: original amounts, source price basis, CPI adjustment, exchange rate and per-passenger formulas follow left to right. Profit and margin calculations: keyed lookups retrieve explicit financial measures; arithmetic and margins are separate. Reported net income is not re-derived.',
  'Excel lookup keys',
    'XLOOKUP selects pooled annual figures and profitability inputs using visible country, airport, year and measure keys (passengers use country, airport and year). Input keys are checked for uniqueness in R. Lookups use direct return columns, and numeric criteria distinguish unavailable values from genuine zeroes; row order is irrelevant. Lookup formulas contain line breaks for readability. XLOOKUP requires a compatible modern version of Excel.',
  'Highlighted headings',
    'Pale blue column headings identify primary calculated outputs; other formulas provide intermediate values or audit lookups.',
  'Australian EBIT bridge',
    'The separately identified impairment term is fixed at zero in the Australian operating EBIT bridge because the model uses regulatory operating profit (already after depreciation/amortisation). This zero is a bridge convention, not a claim that reported impairment was zero.',
  'Financial source interpretation',
    'Capital expenditures, cash tax, financing charges, and non-cash impairments must not be conflated with operating expenses.'
)

var_dictionary <- tribble(
  ~variable, ~description,
  'total_revenue', 'Total airport revenue (AIF included where applicable)',
  'total_aero', 'Comparable total aeronautical revenue, including Canadian AIF',
  'aero_excl_aif', 'Canadian aeronautical revenue excluding AIF',
  'aif_revenue', 'Canadian airport improvement fees',
  'commercial_revenue', 'Commercial and other revenue',
  'parking_transport_revenue', 'Car parking and landside/ground transportation revenue',
  'operating_expenses', 'Operating expenses excluding depreciation and amortisation',
  'labour', 'Salaries, wages and benefits',
  'purchased_services_other', 'Goods and services (reported in Canada; reconstructed from comparable non-labour cost categories in Australia)',
  'federal_ground_rent', 'Canadian federal ground rent',
  'pilt', 'Canadian payment in lieu of municipal taxes',
  'ebitda', 'Earnings before interest, taxes, depreciation and amortisation',
  'ebitda_margin_pct', 'EBITDA / matched operating revenue, percentage points',
  'depreciation_amortisation', 'Depreciation and amortisation expense',
  'asset_impairment', 'Separately identified Canadian asset impairment',
  'ebit', 'Operating EBIT (excluding abnormal items as defined in the model)',
  'ebit_margin_pct', 'Operating EBIT / matched operating revenue, percentage points',
  'interest_financing_costs', 'Financing/interest costs per model source conventions',
  'abnormal_items', 'ACCC reported abnormal items',
  'profit_before_tax', 'Profit before income tax',
  'accounting_income_tax_expense', 'Accounting income-tax expense, not cash taxes',
  'net_income', 'Reported net income / profit after tax',
  'net_profit_margin_pct', 'Net income / matching source revenue, percentage points'
)
if (!setequal(var_dictionary$variable, keep_vars)) {
  stop('Methodology variable dictionary is out of sync with comparison CSVs: ',
       paste(setdiff(keep_vars, var_dictionary$variable), collapse = ', '))
}
# The dictionary lists the workbook's visible field key, after checking
# completeness against the authoritative (unchanged) source variable keys.
var_dictionary <- var_dictionary |>
  mutate(variable = recode(variable,
                           commercial_revenue = 'non_aero_revenue'))

# WORKBOOK FORMATTING -----------------------------------------------------
wb <- openxlsx::createWorkbook(creator = 'Airport financial comparison')
navy <- '#18324F'
blue <- '#215C83'
white <- '#FFFFFF'

sheet_names <- list(
  methodology = 'Methodology & definitions',
  canada = 'Canadian airport finances',
  australia = 'Australian airport finances',
  regulatory = 'Australian costs & profits',
  traffic = 'Passengers & adjustments',
  calculations = 'Financial calculations',
  profit = 'Profit & margin calculations',
  reconciliation = 'Reconciliation',
  totals = 'Total value comparison',
  pp = 'Per-passenger comparison'
)
if (any(nchar(unlist(sheet_names)) > 31L) ||
    anyDuplicated(unlist(sheet_names))) {
  stop('Excel worksheet names must be unique and no longer than 31 chars.')
}

style_title <- openxlsx::createStyle(
  fontSize = 16, textDecoration = 'bold', fontColour = white,
  fgFill = navy, valign = 'center'
)
style_subtitle <- openxlsx::createStyle(
  fontSize = 10, fontColour = blue, valign = 'center',
  wrapText = TRUE
)
style_number <- openxlsx::createStyle(
  numFmt = '#,##0.0;[Red](#,##0.0);"-"'
)
style_count <- openxlsx::createStyle(
  numFmt = '#,##0;[Red](#,##0);"-"'
)
style_year <- openxlsx::createStyle(numFmt = '0')
style_factor <- openxlsx::createStyle(
  numFmt = '0.0000;[Red](0.0000);"-"'
)
style_margin <- openxlsx::createStyle(
  numFmt = '0.0"%";[Red](0.0"%");"-"'
)
style_link <- openxlsx::createStyle(
  fontColour = '#0869A8', textDecoration = 'underline'
)
# A small visual cue for output columns users are most likely to inspect.
# Apply to header cells only, never to every formula result or input.
style_result_header <- openxlsx::createStyle(
  fgFill = '#DCEAF3', fontColour = navy, textDecoration = 'bold',
  halign = 'center', valign = 'center', wrapText = TRUE,
  border = 'Bottom', borderColour = '#A8C4D8'
)
highlight_output_headers <- function(sheet, table_columns,
                                     output_columns, header_row = 4L) {
  missing <- setdiff(output_columns, table_columns)
  if (length(missing)) {
    stop('Unknown output header(s) in ', sheet, ': ',
         paste(missing, collapse = ', '))
  }
  openxlsx::addStyle(
    wb, sheet, style_result_header,
    rows = header_row, cols = match(output_columns, table_columns),
    gridExpand = TRUE, stack = TRUE
  )
  invisible(NULL)
}

# Supporting-sheet source links are clickable; calculation tables below
# also receive Excel arithmetic formulas.
write_source_links <- function(sheet, original, first_data_row = 5L) {
  if (!'source_url' %in% names(original)) return(invisible(NULL))
  col_no <- match('source_url', names(original))
  valid <- which(!is.na(original$source_url) &
                 str_detect(original$source_url, '^https?://'))
  if (!length(valid)) return(invisible(NULL))
  for (i in valid) {
    url <- gsub('"', '""', original$source_url[[i]], fixed = TRUE)
    f <- sprintf('HYPERLINK("%s","Open source")', url)
    openxlsx::writeFormula(wb, sheet, x = f,
                           startCol = col_no,
                           startRow = first_data_row + i - 1L)
  }
  openxlsx::addStyle(
    wb, sheet, style_link,
    rows = first_data_row + valid - 1L, cols = col_no,
    gridExpand = TRUE, stack = TRUE
  )
  invisible(NULL)
}

# Human-readable supporting-sheet column headers. Comparison sheets
# retain authoritative numerical values, with a workbook-only revenue alias.
header_labels <- c(
  country = 'Country', airport = 'Airport',
  year = 'Year', period = 'Reporting period',
  period_type = 'Period type',
  fyear = 'Financial year', activity = 'Activity',
  measure = 'Measure', variable = 'Measure',
  category = 'Category',
  value = 'Value', unit = 'Units',
  value_type = 'Value type', calculation = 'Calculation / derivation',
  original_amount_m = 'Original amount',
  source_price_basis = 'Source price basis',
  inflation_adjusted_m = 'Inflation-adjusted amount',
  amount_2023_m = '2023 amount',
  amount_2024_m = '2024 amount',
  amount_2025_m = '2025 amount',
  pooled_amount_m = 'Three-year total',
  passengers_2023 = '2023 passengers',
  passengers_2024 = '2024 passengers',
  passengers_2025 = '2025 passengers',
  pooled_passengers = 'Three-year passengers',
  pooled_per_passenger = 'Pooled per passenger',
  ebitda_m = 'EBITDA',
  depreciation_amortisation_m = 'Depreciation and amortisation',
  asset_impairment_m = 'Impairment in EBIT bridge',
  operating_ebit_m = 'Operating EBIT',
  operating_revenue_m = 'Operating revenue basis',
  interest_financing_m = 'Interest/financing costs',
  profit_before_tax_m = 'Profit before tax',
  income_tax_expense_m = 'Income tax expense',
  net_income_m = 'Net income',
  net_revenue_m = 'Net margin revenue basis',
  numerator_measure = 'Numerator measure',
  denominator_measure = 'Denominator measure',
  numerator_2023_m = '2023 numerator',
  numerator_2024_m = '2024 numerator',
  numerator_2025_m = '2025 numerator',
  pooled_numerator_m = 'Three-year numerator',
  denominator_2023_m = '2023 denominator',
  denominator_2024_m = '2024 denominator',
  denominator_2025_m = '2025 denominator',
  pooled_denominator_m = 'Three-year denominator',
  pooled_margin_pct = 'Pooled margin (%)',
  ebitda_margin_pct = 'EBITDA margin (%)',
  ebit_margin_pct = 'EBIT margin (%)',
  net_profit_margin_pct = 'Net profit margin (%)',
  cpi_factor = 'CPI factor applied',
  fx_cad_per_unit = 'Exchange rate',
  adjusted_amount_m = 'Amount in 2025 CAD',
  passengers = 'Passengers',
  per_passenger_cad_2025 = 'CAD per passenger',
  margin_numerator_m = 'Margin numerator',
  margin_denominator_m = 'Margin denominator',
  margin_pct = 'Margin (%)',
  margin = 'Margin',
  source_id = 'Source ID', source_type = 'Source type',
  source_detail = 'Source detail', source_url = 'Source URL'
)

add_data_sheet <- function(sheet, input_data, title, subtitle,
                           table_name, freeze_cols = 2L,
                           show_gridlines = FALSE,
                           preserve_headers = FALSE) {
  if (!nrow(input_data)) stop('No rows available for sheet: ', sheet)
  original <- as.data.frame(input_data)
  display <- original
  if ('source_url' %in% names(display)) {
    display$source_url <- if_else(
      !is.na(original$source_url) & nzchar(original$source_url),
      'Open source', NA_character_
    )
  }
  if (!preserve_headers) {
    translated <- header_labels[names(display)]
    names(display) <- ifelse(is.na(translated), names(display), translated)
  }
  if (anyDuplicated(names(display))) {
    stop('Translated headers are duplicated on sheet: ', sheet)
  }

  openxlsx::addWorksheet(wb, sheet, gridLines = show_gridlines)
  nc <- ncol(display)
  openxlsx::writeData(wb, sheet, title, startRow = 1, startCol = 1)
  openxlsx::mergeCells(wb, sheet, rows = 1, cols = seq_len(nc))
  openxlsx::addStyle(wb, sheet, style_title, rows = 1,
                     cols = seq_len(nc), gridExpand = TRUE)
  openxlsx::setRowHeights(wb, sheet, rows = 1, heights = 32)
  openxlsx::writeData(wb, sheet, subtitle, startRow = 2, startCol = 1)
  openxlsx::mergeCells(wb, sheet, rows = 2, cols = seq_len(nc))
  openxlsx::addStyle(wb, sheet, style_subtitle, rows = 2,
                     cols = seq_len(nc), gridExpand = TRUE)
  openxlsx::setRowHeights(wb, sheet, rows = 2, heights = 27)

  openxlsx::writeDataTable(
    wb, sheet, display, startRow = 4, tableStyle = 'TableStyleMedium2',
    tableName = table_name, withFilter = TRUE
  )
  openxlsx::freezePane(wb, sheet, firstActiveRow = 5,
                       firstActiveCol = min(freeze_cols + 1L, nc))

  # Underlying model column names determine formatting even when the reader
  # sees a plain-language label instead of a snake_case field name.
  widths <- rep(17, nc)
  names(widths) <- names(original)
  widths[names(widths) %in% c('airport', 'country')] <- 13
  widths[names(widths) == 'year'] <- 10
  widths[names(widths) %in% c('period', 'period_type',
                              'fyear', 'activity', 'unit',
                              'value_type', 'source_type')] <- 17
  widths[names(widths) == 'source_price_basis'] <- 29
  widths[names(widths) %in% c('numerator_measure',
                              'denominator_measure')] <- 29
  widths[names(widths) %in% c('variable', 'measure', 'category')] <- 30
  widths[names(widths) == 'calculation'] <- 47
  widths[names(widths) == 'source_detail'] <- 46
  widths[names(widths) == 'source_id'] <- 27
  widths[names(widths) == 'source_url'] <- 16
  widths[names(widths) %in% c('margin_numerator_m',
                              'margin_denominator_m',
                              'per_passenger_cad_2025',
                              'pooled_per_passenger',
                              'pooled_amount_m',
                              'operating_revenue_m',
                              'net_revenue_m',
                              'pooled_numerator_m',
                              'pooled_denominator_m')] <- 24
  openxlsx::setColWidths(wb, sheet, cols = seq_len(nc), widths = widths)

  data_rows <- seq.int(5L, 4L + nrow(original))
  numeric_cols <- names(original)[vapply(original, is.numeric, logical(1))]
  for (nm in numeric_cols) {
    style <- if (nm == 'year') style_year else
      if (nm %in% c('passengers', 'passengers_2023',
                      'passengers_2024', 'passengers_2025',
                      'pooled_passengers')) style_count else
        if (nm %in% c('cpi_factor', 'fx_cad_per_unit')) style_factor else
          if (nm == 'margin_pct' || str_ends(nm, '_margin_pct'))
            style_margin else style_number
    openxlsx::addStyle(wb, sheet, style,
                       rows = data_rows, cols = match(nm, names(original)),
                       gridExpand = TRUE, stack = TRUE)
  }

  # Mixed-unit 'Value' field: passenger counts must be integers and CPI/FX
  # adjustment factors must not be rounded to one decimal place.
  if ('value' %in% numeric_cols && 'unit' %in% names(original)) {
    val_col <- match('value', names(original))
    factor_rows <- which(original$unit %in% c('factor', 'CAD per AUD'))
    count_rows <- which(original$unit == 'passengers')
    if (length(factor_rows)) {
      openxlsx::addStyle(wb, sheet, style_factor,
                         rows = 4L + factor_rows, cols = val_col,
                         gridExpand = TRUE, stack = TRUE)
    }
    if (length(count_rows)) {
      openxlsx::addStyle(wb, sheet, style_count,
                         rows = 4L + count_rows, cols = val_col,
                         gridExpand = TRUE, stack = TRUE)
    }
  }
  write_source_links(sheet, original)
  invisible(NULL)
}

# Documentation is written first; sheet order is set before saving.
method_sheet <- sheet_names$methodology
openxlsx::addWorksheet(wb, method_sheet, gridLines = FALSE)
openxlsx::writeData(wb, method_sheet,
                    'Airport financial comparison | methodology',
                    startRow = 1, startCol = 1)
openxlsx::mergeCells(wb, method_sheet, cols = 1:3, rows = 1)
openxlsx::addStyle(wb, method_sheet, style_title, rows = 1,
                   cols = 1:3, gridExpand = TRUE)
openxlsx::setRowHeights(wb, method_sheet, 1, heights = 32)
openxlsx::writeDataTable(wb, method_sheet, methodology,
                         startRow = 4, tableName = 'Methods',
                         tableStyle = 'TableStyleMedium2')
dictionary_row <- 4L + nrow(methodology) + 4L
openxlsx::writeData(wb, method_sheet, 'Measure dictionary',
                    startRow = dictionary_row)
openxlsx::addStyle(wb, method_sheet, style_title,
                   rows = dictionary_row, cols = 1:2,
                   gridExpand = TRUE)
openxlsx::writeDataTable(wb, method_sheet, var_dictionary,
                         startRow = dictionary_row + 2L,
                         tableName = 'Measures',
                         tableStyle = 'TableStyleMedium2')
openxlsx::setColWidths(wb, method_sheet, cols = 1, widths = 31)
openxlsx::setColWidths(wb, method_sheet, cols = 2, widths = 105)
openxlsx::freezePane(wb, method_sheet, firstActiveRow = 5)

add_data_sheet(sheet_names$canada, can_inputs,
               'Canadian airport financial records',
               paste0('Original financial amounts from Canadian operator reports; ',
                      'source details and links appear at the right.'),
               'CanadaInputRecords')
add_data_sheet(sheet_names$australia, sup_inputs,
               'Australian airport revenue and operating records',
               'Amounts already adjusted by the ACCC to FY2024-25 AUD millions; calculated residuals are identified.',
               'SupplementaryRecords')
add_data_sheet(sheet_names$regulatory, reg_inputs,
               'Australian operating costs and profit records',
               paste0('Regulatory financial accounts in nominal AUD millions; ',
                      'published and calculated amounts are distinguished.'),
               'RegulatoryRecords')
add_data_sheet(sheet_names$traffic, traffic_factors,
               'Passengers, inflation and currency adjustments',
               paste0('Passenger counts, Canadian and Australian CPI, and the ',
                      '2025 Bank of Canada exchange rate.'),
               'TrafficCPIExchange')
# Financial calculations and profitability each have a main annual table
# and one small pooled block on the SAME worksheet, not extra worksheets.
add_data_sheet(sheet_names$calculations, calc_annual,
               'Financial calculations | annual amounts',
               paste0('Original and adjusted amounts in millions; ',
                      'source price basis explains CPI factor = 1. ',
                      'Converted amounts in 2025 CAD; per-passenger in CAD. ',
                      'Three-year pooling appears below.'),
               'AnnualFinancialCalculations', freeze_cols = 4L)

add_data_sheet(sheet_names$profit, profit_annual,
               'Profit and margin calculations | annual',
               paste0('Amounts in millions of 2025 CAD; margins are percentages. ',
                      'Australian impairment bridge term is zero by model convention. ',
                      'Reported net income is not reconstructed; pooled margins below.'),
               'AnnualProfitMargins', freeze_cols = 3L)

# Write a second normal Excel table below the first one. This deliberately
# avoids mixed-purpose formulas in a column. Every calculated output column
# has one uniform fill-down formula within its table.
section_style <- openxlsx::createStyle(
  fontSize = 12, textDecoration = 'bold', fontColour = navy,
  fgFill = '#E7EFF5'
)
append_pooled_table <- function(sheet, data, title, table_name, after_rows) {
  heading_row <- as.integer(5L + after_rows + 3L)
  header_row <- heading_row + 2L
  first_data_row <- header_row + 1L
  openxlsx::writeData(wb, sheet, title, startRow = heading_row)
  openxlsx::addStyle(wb, sheet, section_style, rows = heading_row,
                     cols = seq_len(ncol(data)), gridExpand = TRUE)
  openxlsx::setRowHeights(wb, sheet, heading_row, heights = 24)
  displayed <- as.data.frame(data)
  readable <- header_labels[names(displayed)]
  names(displayed) <- ifelse(is.na(readable), names(displayed), readable)
  if (anyDuplicated(names(displayed))) {
    stop('Duplicate pooled table headings for ', sheet)
  }
  openxlsx::writeDataTable(
    wb, sheet, displayed, startRow = header_row,
    tableStyle = 'TableStyleMedium2', tableName = table_name,
    withFilter = TRUE
  )
  for (j in seq_along(data)) {
    nm <- names(data)[[j]]
    if (!is.numeric(data[[j]])) next
    style <- if (nm == 'year') style_year else
      if (str_detect(nm, 'passengers') &&
          !str_detect(nm, 'per_passenger')) style_count else
        if (str_detect(nm, 'margin_pct')) style_margin else style_number
    openxlsx::addStyle(wb, sheet, style,
                       rows = seq.int(first_data_row,
                                      length.out = nrow(data)),
                       cols = j, gridExpand = TRUE, stack = TRUE)
  }
  openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(data)),
                         widths = rep(21, ncol(data)))
  openxlsx::setColWidths(wb, sheet, cols = c(1L, 2L), widths = 14)
  openxlsx::setColWidths(wb, sheet, cols = 3L, widths = 30)
  if (identical(sheet, sheet_names$profit)) {
    # The pooled margin measure keys should remain readable without opening
    # the cells, and the same columns also hold annual D&A / impairment.
    openxlsx::setColWidths(wb, sheet, cols = 4:5, widths = 31)
  }
  first_data_row
}

calc_pooled_start <- append_pooled_table(
  sheet_names$calculations, calc_pooled,
  'Three-year pooled amounts | 2025 CAD millions',
  'PooledFinancialCalculations', nrow(calc_annual)
)
profit_pooled_start <- append_pooled_table(
  sheet_names$profit, profit_pooled,
  'Three-year pooled margins | sums of matched profits and revenue',
  'PooledProfitMargins', nrow(profit_annual)
)

# Consistent Excel formulas: retrieve identified observations first, then
# do plain arithmetic in distinct columns. Every data column in a given table
# contains the same fill-down formula (apart from its row/selected year).
# Excel's structured table references are stable when source data are sorted.
write_column_formulas <- function(sheet, col, first_row, formulas) {
  if (!length(formulas)) return(invisible(NULL))
  openxlsx::writeFormula(wb, sheet, x = formulas,
                         startCol = col, startRow = first_row)
  invisible(NULL)
}

# A key-based XLOOKUP returns a DIRECT table column, not an IF-generated
# array. V6 used IF(ISNUMBER(...), value, "") as its return_array; that
# construction can produce blank lookups when evaluated inside Excel tables.
# Instead, require both the original input AND the requested result to be
# numeric within lookup_array. Thus numeric zero survives, while missing or
# still-unavailable values produce "" (including reconciliation results).
# Uniqueness of the displayed source keys is checked in R before export.
# Newlines are intentional: Excel displays them in its expanded formula bar.
# _xlfn.XLOOKUP is the OOXML name; modern Excel displays ordinary XLOOKUP.
excel_lookup <- function(group, value_col, criteria,
                         presence_col = 'Original amount') {
  if (!length(criteria) || is.null(names(criteria)) ||
      any(!nzchar(names(criteria)))) {
    stop('Excel lookup requires named criteria.')
  }
  key_matches <- vapply(seq_along(criteria), function(j) {
    sprintf('(%s[%s]=%s)', group, names(criteria)[[j]], criteria[[j]])
  }, character(1))
  # Do not put array-producing IF() expressions in XLOOKUP's return_array.
  # Inclusion requires a numeric underlying source and a numeric result.
  numeric_checks <- unique(c(
    sprintf('ISNUMBER(%s[%s])', group, presence_col),
    sprintf('ISNUMBER(%s[%s])', group, value_col)
  ))
  lookup_conditions <- paste0('    ', c(key_matches, numeric_checks),
                              collapse = ' *\n')
  paste0(
    '_xlfn.XLOOKUP(\n',
    '  1,\n',
    lookup_conditions, ',\n',
    '  ', group, '[', value_col, '],\n',
    '  ""\n',
    ')'
  )
}

# Block 1: original amount -> CPI-adjusted amount -> CAD -> per passenger.
# An explicit price-basis field is displayed immediately before the CPI factor.
rr <- seq.int(5L, length.out = nrow(calc_annual))
write_column_formulas(sheet_names$calculations, 8L, 5L,
  sprintf('IF(COUNT(E%d,G%d)=2,E%d*G%d,"")', rr, rr, rr, rr))
write_column_formulas(sheet_names$calculations, 10L, 5L,
  sprintf('IF(COUNT(H%d,I%d)=2,H%d*I%d,"")', rr, rr, rr, rr))
write_column_formulas(sheet_names$calculations, 12L, 5L,
  sprintf('IF(AND(COUNT(J%d:K%d)=2,K%d>0),J%d*1000000/K%d,"")',
          rr, rr, rr, rr, rr))

# Block 2: pooled annual amounts are LOOKED UP by visible key fields, not
# by source row positions selected by R. A distinct year is bound in each
# column. Passengers are obtained from unique source passenger records,
# rather than summing passenger counts repeated for every financial measure.
pr <- seq.int(calc_pooled_start, length.out = nrow(calc_pooled))
for (j in seq_along(2023:2025)) {
  yr <- (2023:2025)[[j]]
  amount_formulas <- vapply(pr, function(row) {
    excel_lookup('AnnualFinancialCalculations', 'Amount in 2025 CAD',
                 c(Country = sprintf('$A%d', row),
                   Airport = sprintf('$B%d', row),
                   Year = as.character(yr),
                   Measure = sprintf('$C%d', row)))
  }, character(1))
  passenger_formulas <- vapply(pr, function(row) {
    excel_lookup('TrafficCPIExchange', 'Value',
                 c(Country = sprintf('$A%d', row),
                   Airport = sprintf('$B%d', row),
                   Year = as.character(yr),
                   Measure = '"passengers"'),
                 presence_col = 'Value')
  }, character(1))
  write_column_formulas(sheet_names$calculations, 3L + j,
                        calc_pooled_start, amount_formulas)
  write_column_formulas(sheet_names$calculations, 7L + j,
                        calc_pooled_start, passenger_formulas)
}
write_column_formulas(sheet_names$calculations, 7L, calc_pooled_start,
  sprintf('IF(COUNT(D%d:F%d)=3,SUM(D%d:F%d),"")', pr, pr, pr, pr))
write_column_formulas(sheet_names$calculations, 11L, calc_pooled_start,
  sprintf('IF(COUNT(H%d:J%d)=3,SUM(H%d:J%d),"")', pr, pr, pr, pr))
write_column_formulas(sheet_names$calculations, 12L, calc_pooled_start,
  sprintf('IF(AND(COUNT(G%d,K%d)=2,K%d>0),G%d*1000000/K%d,"")',
          pr, pr, pr, pr, pr))

# Annual profitability bridge. The lookups select measure-specific financial
# observations from AnnualFinancialCalculations using visible key criteria.
# Column G and margin columns carry only the arithmetic. For the Australian
# bridge, the impairment term is explicitly fixed at zero by the model;
# Canadian impairment comes from a keyed lookup (missing remains missing).
bridge_link_vars <- c(
  'D' = 'ebitda', 'E' = 'depreciation_amortisation',
  'H' = 'ebitda_ebit_revenue_basis',
  'K' = 'interest_financing_costs',
  'L' = 'profit_before_tax',
  'M' = 'accounting_income_tax_expense',
  'N' = 'net_income',
  'O' = 'net_income_revenue_basis'
)
mr <- seq.int(5L, length.out = nrow(profit_annual))
for (column_letter in names(bridge_link_vars)) {
  measure_name <- bridge_link_vars[[column_letter]]
  formulas <- vapply(mr, function(row) {
    excel_lookup('AnnualFinancialCalculations', 'Amount in 2025 CAD',
                 c(Country = sprintf('$A%d', row),
                   Airport = sprintf('$B%d', row),
                   Year = sprintf('$C%d', row),
                   Measure = sprintf('"%s"', measure_name)))
  }, character(1))
  write_column_formulas(sheet_names$profit,
                        openxlsx::col2int(column_letter), 5L, formulas)
}
impairment_formulas <- vapply(mr, function(row) {
  can_impairment <- excel_lookup(
    'AnnualFinancialCalculations', 'Amount in 2025 CAD',
    c(Country = sprintf('$A%d', row),
      Airport = sprintf('$B%d', row),
      Year = sprintf('$C%d', row),
      Measure = '"asset_impairment"'))
  sprintf('IF($A%d="Australia",0,%s)', row, can_impairment)
}, character(1))
write_column_formulas(sheet_names$profit, 6L, 5L, impairment_formulas)

# Missing data checks are simple, external guards around the arithmetic.
# Unlike the previous nested IF(ISNUMBER(...),...,0), a missing impairment
# cannot silently become zero for a Canadian airport.
write_column_formulas(sheet_names$profit, 7L, 5L,
  sprintf('IF(COUNT(D%d:F%d)=3,D%d-E%d-F%d,"")',
          mr, mr, mr, mr, mr))
write_column_formulas(sheet_names$profit, 9L, 5L,
  sprintf('IF(AND(COUNT(D%d,H%d)=2,H%d>0),100*D%d/H%d,"")',
          mr, mr, mr, mr, mr))
write_column_formulas(sheet_names$profit, 10L, 5L,
  sprintf('IF(AND(COUNT(G%d,H%d)=2,H%d>0),100*G%d/H%d,"")',
          mr, mr, mr, mr, mr))
write_column_formulas(sheet_names$profit, 16L, 5L,
  sprintf('IF(AND(COUNT(N%d:O%d)=2,O%d>0),100*N%d/O%d,"")',
          mr, mr, mr, mr, mr))

# Pooled profit margin numerators/denominators are selected with exactly
# the same key-based Excel lookup as pooled financial amounts. The required
# source measure is *visible* in the two columns next to the margin name.
# Australian SCACH net revenue is selected by net_income_revenue_basis.
mp <- seq.int(profit_pooled_start, length.out = nrow(profit_pooled))
for (j in seq_along(2023:2025)) {
  yr <- (2023:2025)[[j]]
  numerator_formulas <- vapply(mp, function(row) {
    excel_lookup('AnnualFinancialCalculations', 'Amount in 2025 CAD',
                 c(Country = sprintf('$A%d', row),
                   Airport = sprintf('$B%d', row),
                   Year = as.character(yr),
                   Measure = sprintf('$D%d', row)))
  }, character(1))
  denominator_formulas <- vapply(mp, function(row) {
    excel_lookup('AnnualFinancialCalculations', 'Amount in 2025 CAD',
                 c(Country = sprintf('$A%d', row),
                   Airport = sprintf('$B%d', row),
                   Year = as.character(yr),
                   Measure = sprintf('$E%d', row)))
  }, character(1))
  write_column_formulas(sheet_names$profit, 5L + j,
                        profit_pooled_start, numerator_formulas)
  write_column_formulas(sheet_names$profit, 9L + j,
                        profit_pooled_start, denominator_formulas)
}
write_column_formulas(sheet_names$profit, 9L, profit_pooled_start,
  sprintf('IF(COUNT(F%d:H%d)=3,SUM(F%d:H%d),"")', mp, mp, mp, mp))
write_column_formulas(sheet_names$profit, 13L, profit_pooled_start,
  sprintf('IF(COUNT(J%d:L%d)=3,SUM(J%d:L%d),"")', mp, mp, mp, mp))
write_column_formulas(sheet_names$profit, 14L, profit_pooled_start,
  sprintf('IF(AND(COUNT(I%d,M%d)=2,M%d>0),100*I%d/M%d,"")',
          mp, mp, mp, mp, mp))

# RECONCILIATION --------------------------------------------------------
# A single reader-facing sheet verifies the Excel implementation against
# independent, unrounded values imported from the four authoritative R CSVs.
# The calculated-value cells retrieve from the *Excel calculation tables*,
# never from those benchmarks or from the static final presentation sheets.
# All four blocks use the same simple benchmark -> Excel -> difference ->
# status pattern. In the annual margin block, metrics occupy parallel columns
# because the annual Profit & margin calculations table is itself wide.
recon_sheet <- sheet_names$reconciliation
recon_tolerance <- 1e-6  # In the displayed units: CAD m, CAD/pax, or margin points.

# Both final tables print exactly the same margin percentages, rather than
# different 'per-passenger margins'. They need only one Excel comparison each.
assert_same_margins <- function(absolute, per_passenger, keys, label) {
  abs_m <- absolute |> select(all_of(c(keys, margin_vars)))
  pp_m <- per_passenger |> select(all_of(c(keys, margin_vars)))
  joined <- abs_m |> left_join(pp_m, by = keys, suffix = c('_total', '_pp'))
  if (nrow(joined) != nrow(absolute) ||
      !all(vapply(margin_vars, function(metric) {
        within_tolerance(joined[[paste0(metric, '_total')]],
                         joined[[paste0(metric, '_pp')]], tol = 1e-8)
      }, logical(nrow(joined))))) {
    stop('Total and per-passenger R margins differ in ', label, '.')
  }
}
assert_same_margins(annual_abs, annual_pp,
                    c('country', 'airport', 'year'), 'annual CSVs')
assert_same_margins(pooled_abs, pooled_pp,
                    c('country', 'airport'), 'pooled CSVs')

recon_annual_source <- annual_abs |>
  pivot_longer(all_of(amount_vars), names_to = 'variable',
               values_to = 'benchmark_total') |>
  left_join(
    annual_pp |>
      pivot_longer(all_of(amount_vars), names_to = 'variable',
                   values_to = 'benchmark_pp'),
    by = c('country', 'airport', 'year', 'variable')
  ) |>
  arrange(country, airport, year, match(variable, amount_vars))
assert_unique(recon_annual_source,
              c('country', 'airport', 'year', 'variable'),
              'Annual reconciliation benchmarks')
recon_annual <- recon_annual_source |>
  transmute(
    Country = country, Airport = airport, Year = year,
    Measure = workbook_measure_key(variable),
    `R total` = benchmark_total, `Excel total` = NA_real_,
    `Total difference` = NA_real_, `Total status` = NA_character_,
    `R per passenger` = benchmark_pp, `Excel per passenger` = NA_real_,
    `Per-passenger difference` = NA_real_,
    `Per-passenger status` = NA_character_
  )

recon_pooled_source <- pooled_abs |>
  pivot_longer(all_of(amount_vars), names_to = 'variable',
               values_to = 'benchmark_total') |>
  left_join(
    pooled_pp |>
      pivot_longer(all_of(amount_vars), names_to = 'variable',
                   values_to = 'benchmark_pp'),
    by = c('country', 'airport', 'variable')
  ) |>
  arrange(country, airport, match(variable, amount_vars))
assert_unique(recon_pooled_source, c('country', 'airport', 'variable'),
              'Pooled reconciliation benchmarks')
recon_pooled <- recon_pooled_source |>
  transmute(
    Country = country, Airport = airport,
    Measure = workbook_measure_key(variable),
    `R total` = benchmark_total, `Excel total` = NA_real_,
    `Total difference` = NA_real_, `Total status` = NA_character_,
    `R per passenger` = benchmark_pp, `Excel per passenger` = NA_real_,
    `Per-passenger difference` = NA_real_,
    `Per-passenger status` = NA_character_
  )

# Four checks per annual airport-year: the separately computed EBIT bridge
# plus the three margins. Each lookup column always selects a named column
# from the original (wide) AnnualProfitMargins Excel table.
recon_profit_annual <- annual_abs |>
  arrange(country, airport, year) |>
  transmute(
    Country = country, Airport = airport, Year = year,
    `R EBIT` = ebit, `Excel EBIT` = NA_real_,
    `EBIT difference` = NA_real_, `EBIT status` = NA_character_,
    `R EBITDA margin` = ebitda_margin_pct,
    `Excel EBITDA margin` = NA_real_,
    `EBITDA margin difference` = NA_real_,
    `EBITDA margin status` = NA_character_,
    `R EBIT margin` = ebit_margin_pct,
    `Excel EBIT margin` = NA_real_,
    `EBIT margin difference` = NA_real_,
    `EBIT margin status` = NA_character_,
    `R net margin` = net_profit_margin_pct,
    `Excel net margin` = NA_real_,
    `Net margin difference` = NA_real_,
    `Net margin status` = NA_character_
  )

recon_profit_pooled <- pooled_abs |>
  pivot_longer(all_of(margin_vars),
               names_to = 'variable', values_to = 'benchmark') |>
  arrange(country, airport, match(variable, margin_vars)) |>
  transmute(
    Country = country, Airport = airport,
    Margin = recode(variable,
      ebitda_margin_pct = 'EBITDA margin',
      ebit_margin_pct = 'EBIT margin',
      net_profit_margin_pct = 'Net profit margin'),
    `R margin` = benchmark, `Excel margin` = NA_real_,
    Difference = NA_real_, Status = NA_character_
  )

if (nrow(recon_annual) != nrow(annual_abs) * length(amount_vars) ||
    nrow(recon_pooled) != nrow(pooled_abs) * length(amount_vars) ||
    nrow(recon_profit_annual) != nrow(annual_abs) ||
    nrow(recon_profit_pooled) != nrow(pooled_abs) * length(margin_vars) ||
    anyNA(recon_annual[, c('Country', 'Airport', 'Year', 'Measure')]) ||
    anyNA(recon_pooled[, c('Country', 'Airport', 'Measure')])) {
  stop('Incomplete reconciliation key coverage.')
}

openxlsx::addWorksheet(wb, recon_sheet, gridLines = FALSE)
openxlsx::writeData(wb, recon_sheet,
                    'Reconciliation | Excel calculations against R results',
                    startRow = 1L, startCol = 1L)
openxlsx::mergeCells(wb, recon_sheet, rows = 1L, cols = 1:19)
openxlsx::addStyle(wb, recon_sheet, style_title, rows = 1L,
                   cols = 1:19, gridExpand = TRUE)
openxlsx::setRowHeights(wb, recon_sheet, rows = 1L, heights = 34)
openxlsx::writeData(wb, recon_sheet, 'Tolerance',
                    startRow = 3L, startCol = 1L)
openxlsx::writeData(wb, recon_sheet, recon_tolerance,
                    startRow = 3L, startCol = 2L)
openxlsx::addStyle(wb, recon_sheet,
                   openxlsx::createStyle(numFmt = '0.000000'),
                   rows = 3L, cols = 2L)
openxlsx::writeData(wb, recon_sheet,
  'PASS = within tolerance; FAIL = numeric difference; MISSING MISMATCH = only one side has a value; BOTH MISSING = both blank.',
  startRow = 4L, startCol = 1L)
openxlsx::mergeCells(wb, recon_sheet, rows = 4L, cols = 1:19)
openxlsx::addStyle(wb, recon_sheet, style_subtitle,
                   rows = 4L, cols = 1:19, gridExpand = TRUE)
openxlsx::writeData(wb, recon_sheet,
  'The R values are fixed benchmarks; the Excel values are live lookups. Open and recalculate in Excel to populate these results.',
  startRow = 5L, startCol = 1L)
openxlsx::mergeCells(wb, recon_sheet, rows = 5L, cols = 1:19)
openxlsx::addStyle(wb, recon_sheet, style_subtitle,
                   rows = 5L, cols = 1:19, gridExpand = TRUE)
openxlsx::setRowHeights(wb, recon_sheet, rows = 4:5, heights = 24)

# Tables are deliberately separate, keeping their formulas uniform and
# avoiding SWITCH/IF logic that would mix lookup sources in one column.
recon_sections <- list()
style_difference <- openxlsx::createStyle(numFmt = '0.000000')
recon_next_row <- 8L
add_recon_section <- function(key, label, data, table_name) {
  heading <- recon_next_row
  header <- heading + 2L
  first <- header + 1L
  last <- first + nrow(data) - 1L
  openxlsx::writeData(wb, recon_sheet, label,
                      startRow = heading, startCol = 1L)
  openxlsx::mergeCells(wb, recon_sheet, rows = heading,
                       cols = seq_len(ncol(data)))
  openxlsx::addStyle(wb, recon_sheet, section_style, rows = heading,
                     cols = seq_len(ncol(data)), gridExpand = TRUE)
  openxlsx::setRowHeights(wb, recon_sheet, heading, heights = 24)
  openxlsx::writeDataTable(wb, recon_sheet, data,
                           startRow = header, startCol = 1L,
                           tableStyle = 'TableStyleMedium2',
                           tableName = table_name, withFilter = TRUE)
  # Benchmark and calculated financial values use the same 2025 CAD units.
  # Show differences to six decimals so small discrepancies are not hidden.
  for (j in seq_along(data)) {
    if (!is.numeric(data[[j]])) next
    nm <- names(data)[[j]]
    st <- if (nm == 'Year') style_year else
      if (grepl('difference', nm, ignore.case = TRUE) ||
          nm == 'Difference') style_difference else
        style_number
    openxlsx::addStyle(wb, recon_sheet, st,
                       rows = seq.int(first, last), cols = j,
                       gridExpand = TRUE, stack = TRUE)
  }
  recon_sections[[key]] <<- list(start = first, end = last,
                                 columns = names(data))
  recon_next_row <<- last + 5L
  invisible(NULL)
}
add_recon_section('annual', '1. Annual financial amounts | 2025 CAD millions and CAD per passenger',
                  recon_annual, 'ReconAnnualAmounts')
add_recon_section('pooled', '2. Three-year pooled amounts | 2025 CAD millions and CAD per passenger',
                  recon_pooled, 'ReconPooledAmounts')
add_recon_section('profit_annual', '3. Annual operating EBIT (2025 CAD millions) and margins (%)',
                  recon_profit_annual, 'ReconAnnualProfit')
add_recon_section('profit_pooled', '4. Three-year pooled margins (%)',
                  recon_profit_pooled, 'ReconPooledProfit')

# Highlight only reader-facing calculated outputs. Intermediate lookup and
# inflation columns retain ordinary headings; the final analysis tables are
# left exactly as before.
highlight_output_headers(
  sheet_names$calculations, names(calc_annual),
  c('adjusted_amount_m', 'per_passenger_cad_2025')
)
highlight_output_headers(
  sheet_names$calculations, names(calc_pooled),
  c('pooled_amount_m', 'pooled_per_passenger'),
  header_row = calc_pooled_start - 1L
)
highlight_output_headers(
  sheet_names$profit, names(profit_annual),
  c('operating_ebit_m', 'ebitda_margin_pct', 'ebit_margin_pct',
    'net_profit_margin_pct')
)
highlight_output_headers(
  sheet_names$profit, names(profit_pooled), 'pooled_margin_pct',
  header_row = profit_pooled_start - 1L
)
for (section_key in names(recon_sections)) {
  section <- recon_sections[[section_key]]
  highlighted <- grep('status$', section$columns, ignore.case = TRUE,
                      value = TRUE)
  highlight_output_headers(
    recon_sheet, section$columns,
    highlighted, header_row = section$start - 1L
  )
}

# Positional REFERENCES within a reconciliation row are only for adjacent
# arithmetic. Retrieving a financial observation always uses identifying
# keys displayed in the row, not R-preselected source row numbers.
write_recon_lookup <- function(section, excel_col, source_table,
                               source_value, keys) {
  rows <- seq.int(section$start, section$end)
  expressions <- vapply(rows, function(r) {
    conditions <- vapply(keys, function(entry) {
      if (is.function(entry)) return(entry(r))
      as.character(entry)
    }, character(1))
    excel_lookup(source_table, source_value, conditions,
                 presence_col = source_value)
  }, character(1))
  write_column_formulas(recon_sheet,
                        match(excel_col, section$columns),
                        section$start, expressions)
}

write_recon_diffs <- function(section, reference_col, excel_col,
                              difference_col, status_col) {
  rows <- seq.int(section$start, section$end)
  col_letter <- function(name) {
    openxlsx::int2col(match(name, section$columns))
  }
  r_ref <- col_letter(reference_col)
  r_excel <- col_letter(excel_col)
  r_diff <- col_letter(difference_col)
  diff_formula <- sprintf(
    'IF(COUNT(%s%d,%s%d)=2,%s%d-%s%d,"")',
    r_ref, rows, r_excel, rows, r_excel, rows, r_ref, rows)
  # The only conditional logic here belongs to the explicit QA status test,
  # not to any financial calculation or selected-source lookup.
  status_formula <- sprintf(
    paste0('IF(COUNT(%s%d,%s%d)=0,"BOTH MISSING",',
           'IF(COUNT(%s%d,%s%d)=1,"MISSING MISMATCH",',
           'IF(ABS(%s%d)<=$B$3,"PASS","FAIL")))'),
    r_ref, rows, r_excel, rows,
    r_ref, rows, r_excel, rows,
    r_diff, rows)
  write_column_formulas(recon_sheet,
                        match(difference_col, section$columns),
                        section$start, diff_formula)
  write_column_formulas(recon_sheet,
                        match(status_col, section$columns),
                        section$start, status_formula)
}

# Annual and pooled monetary amounts: each block shows both absolute
# and per-passenger comparisons side by side, avoiding duplicated keys.
annual_sec <- recon_sections$annual
annual_keys <- list(
  Country = function(r) sprintf('$A%d', r),
  Airport = function(r) sprintf('$B%d', r),
  Year = function(r) sprintf('$C%d', r),
  Measure = function(r) sprintf('$D%d', r)
)
write_recon_lookup(annual_sec, 'Excel total',
  'AnnualFinancialCalculations', 'Amount in 2025 CAD', annual_keys)
write_recon_lookup(annual_sec, 'Excel per passenger',
  'AnnualFinancialCalculations', 'CAD per passenger', annual_keys)
write_recon_diffs(annual_sec, 'R total', 'Excel total',
                  'Total difference', 'Total status')
write_recon_diffs(annual_sec, 'R per passenger', 'Excel per passenger',
                  'Per-passenger difference', 'Per-passenger status')

pooled_sec <- recon_sections$pooled
pooled_keys <- list(
  Country = function(r) sprintf('$A%d', r),
  Airport = function(r) sprintf('$B%d', r),
  Measure = function(r) sprintf('$C%d', r)
)
write_recon_lookup(pooled_sec, 'Excel total',
  'PooledFinancialCalculations', 'Three-year total', pooled_keys)
write_recon_lookup(pooled_sec, 'Excel per passenger',
  'PooledFinancialCalculations', 'Pooled per passenger', pooled_keys)
write_recon_diffs(pooled_sec, 'R total', 'Excel total',
                  'Total difference', 'Total status')
write_recon_diffs(pooled_sec, 'R per passenger', 'Excel per passenger',
                  'Per-passenger difference', 'Per-passenger status')

profit_sec <- recon_sections$profit_annual
profit_keys <- list(
  Country = function(r) sprintf('$A%d', r),
  Airport = function(r) sprintf('$B%d', r),
  Year = function(r) sprintf('$C%d', r)
)
profit_metrics <- list(
  c('R EBIT', 'Excel EBIT', 'EBIT difference', 'EBIT status',
    'Operating EBIT'),
  c('R EBITDA margin', 'Excel EBITDA margin',
    'EBITDA margin difference', 'EBITDA margin status', 'EBITDA margin (%)'),
  c('R EBIT margin', 'Excel EBIT margin',
    'EBIT margin difference', 'EBIT margin status', 'EBIT margin (%)'),
  c('R net margin', 'Excel net margin',
    'Net margin difference', 'Net margin status', 'Net profit margin (%)')
)
for (metric in profit_metrics) {
  write_recon_lookup(profit_sec, metric[[2]],
                     'AnnualProfitMargins', metric[[5]], profit_keys)
  write_recon_diffs(profit_sec, metric[[1]], metric[[2]],
                    metric[[3]], metric[[4]])
}

pooled_profit_sec <- recon_sections$profit_pooled
pooled_profit_keys <- list(
  Country = function(r) sprintf('$A%d', r),
  Airport = function(r) sprintf('$B%d', r),
  Margin = function(r) sprintf('$C%d', r)
)
write_recon_lookup(pooled_profit_sec, 'Excel margin',
                   'PooledProfitMargins', 'Pooled margin (%)',
                   pooled_profit_keys)
write_recon_diffs(pooled_profit_sec, 'R margin', 'Excel margin',
                  'Difference', 'Status')

# At the top, one compact count of each QA status over all tested values.
status_ranges <- c(
  sprintf('%s%d:%s%d', 'H', annual_sec$start, 'H', annual_sec$end),
  sprintf('%s%d:%s%d', 'L', annual_sec$start, 'L', annual_sec$end),
  sprintf('%s%d:%s%d', 'G', pooled_sec$start, 'G', pooled_sec$end),
  sprintf('%s%d:%s%d', 'K', pooled_sec$start, 'K', pooled_sec$end),
  vapply(c('EBIT status', 'EBITDA margin status',
           'EBIT margin status', 'Net margin status'), function(col) {
    ltr <- openxlsx::int2col(match(col, profit_sec$columns))
    sprintf('%s%d:%s%d', ltr, profit_sec$start, ltr, profit_sec$end)
  }, character(1)),
  sprintf('F%d:F%d', pooled_profit_sec$start, pooled_profit_sec$end)
)
status_summary <- c('PASS', 'FAIL', 'MISSING MISMATCH', 'BOTH MISSING')
for (i in seq_along(status_summary)) {
  label_col <- c(4L, 6L, 8L, 10L)[[i]]
  metric <- status_summary[[i]]
  openxlsx::writeData(wb, recon_sheet, metric,
                      startRow = 3L, startCol = label_col)
  terms <- sprintf('COUNTIF(%s,"%s")', status_ranges, metric)
  openxlsx::writeFormula(wb, recon_sheet,
                         paste(terms, collapse = '+'),
                         startRow = 3L, startCol = label_col + 1L)
}
# One numeric tolerance is used for all comparisons in their display units.
# The expected total includes both absolute and per-passenger financial
# checks, one EBIT bridge and three annual margins, and pooled margins.
expected_checks <- 2L * (nrow(recon_annual) + nrow(recon_pooled)) +
  4L * nrow(recon_profit_annual) + nrow(recon_profit_pooled)
if (expected_checks != length(amount_vars) * (nrow(annual_abs) +
                                                nrow(pooled_abs)) * 2L +
                       4L * nrow(annual_abs) +
                       length(margin_vars) * nrow(pooled_abs)) {
  stop('Unexpected number of reconciliation cells.')
}
openxlsx::writeData(wb, recon_sheet, 'Total checks',
                    startRow = 3L, startCol = 13L)
openxlsx::writeData(wb, recon_sheet, expected_checks,
                    startRow = 3L, startCol = 14L)

# Keep the sheet compact despite four sections sharing a common grid.
openxlsx::setColWidths(wb, recon_sheet, cols = 1:2, widths = 15)
openxlsx::setColWidths(wb, recon_sheet, cols = 3, widths = 17)
openxlsx::setColWidths(wb, recon_sheet, cols = 4, widths = 32)
openxlsx::setColWidths(wb, recon_sheet, cols = 5:19, widths = 18)
openxlsx::setColWidths(wb, recon_sheet,
                       cols = c(8, 12, 15, 19), widths = 22)
openxlsx::freezePane(wb, recon_sheet,
                     firstActiveRow = 7L, firstActiveCol = 3L)

# Color flags are purely visual; statuses remain plain-text Excel formulas.
style_fail <- openxlsx::createStyle(fgFill = '#FDE9E7',
                                   fontColour = '#A62222')
style_pass <- openxlsx::createStyle(fgFill = '#EAF4EA',
                                   fontColour = '#246B39')
for (sec_name in names(recon_sections)) {
  sec <- recon_sections[[sec_name]]
  which_status <- which(grepl('status$', sec$columns, ignore.case = TRUE))
  for (j in which_status) {
    ltr <- openxlsx::int2col(j)
    rows <- seq.int(sec$start, sec$end)
    openxlsx::conditionalFormatting(wb, recon_sheet, cols = j,
      rows = rows, type = 'expression',
      rule = sprintf('%s%d="PASS"', ltr, sec$start), style = style_pass)
    openxlsx::conditionalFormatting(wb, recon_sheet, cols = j,
      rows = rows, type = 'expression',
      rule = sprintf('OR(%s%d="FAIL",%s%d="MISSING MISMATCH")',
                     ltr, sec$start, ltr, sec$start), style = style_fail)
  }
}

add_data_sheet(sheet_names$totals, final_totals,
               'Airport financial comparison | total values',
               paste0('Final analysis table: millions of 2025 CAD; margins: percent. ',
                      'Annual and pooled observations are stacked.'),
               'FinalComparisonTotals', show_gridlines = TRUE,
               preserve_headers = TRUE)
add_data_sheet(sheet_names$pp, final_pp,
               'Airport financial comparison | per passenger',
               paste0('Final analysis table: 2025 CAD per passenger; margins: percent. ',
                      'Annual and pooled observations are stacked.'),
               'FinalComparisonPerPax', show_gridlines = TRUE,
               preserve_headers = TRUE)

# Write worksheets in actual reading order, regardless of the order in
# which the builder creates them. openxlsx::worksheetOrder changes only the
# order saved to Excel; existing sheet-name formula references are unaffected.
reading_order <- unlist(sheet_names[c(
  'totals', 'pp',
  'calculations', 'profit',
  'canada', 'australia', 'regulatory', 'traffic',
  'methodology', 'reconciliation'
)], use.names = FALSE)
saved_sheet_order <- match(reading_order, names(wb))
if (anyNA(saved_sheet_order) || anyDuplicated(saved_sheet_order) ||
    length(saved_sheet_order) != length(names(wb))) {
  stop('Could not establish the requested ten-worksheet reading order.')
}
worksheetOrder(wb) <- saved_sheet_order

# openxlsx writes formulas but does not evaluate them. Reconciliation
# results and the summary populate when Excel recalculates the workbook.
openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
message('Saved documented airport financial workbook: ', output_file)
message('Sheets: ', paste(reading_order, collapse = ' | '))
message('Annual comparison rows: ', nrow(annual_abs),
        '; pooled comparison rows: ', nrow(pooled_abs),
        '; annual amount rows: ', nrow(calc_annual),
        '; pooled amount rows: ', nrow(calc_pooled),
        '; annual profit rows: ', nrow(profit_annual),
        '; pooled margin rows: ', nrow(profit_pooled),
        '; reconciliation checks: ', expected_checks,
        '; unique source documents: ', nrow(source_catalog))
