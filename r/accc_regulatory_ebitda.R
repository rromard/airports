# ACCC regulatory accounts: EBITDA and income-statement results, 2022-23 to 2024-25
# Monetary amounts: nominal AUD millions; margins: percentages.
# Readable functional version: same extraction labels and accounting formulas.
# Missing-expense imputation tolerance is now consistently in AUD millions.
# Includes zero-total / two-blank activity expense lines (PER 2024-25).
# Operating profit is extracted for all three activities. Reported EBIT is
# retained where published (total and aeronautical). Other lower income-
# statement lines are total-only, reflecting their availability in the ACCC.
# Expense-positive standardisation: interest_expense = -reported interest;
# income_tax_expense = -reported tax charge. Credits therefore remain negative.
# Blanks (including dividends) stay NA rather than being silently set to zero.
library(readxl)
library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(stringr)
library(here)
library(tidyr)

# Preserve V4's input directory; no change to the existing project layout.
input_dir <- here('data', 'australia', 'regulatory_accounts')
output_dir <- here('output')
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

files <- c(
  `2022-23` = '2022-23-airport-monitoring-report-regulatory-accounts.xlsx',
  `2023-24` = 'accc-airport-monitoring-report-2023-24-regulatory-accounts.xlsx',
  `2024-25` = 'accc-airport-monitoring-report-2024-25-regulatory-accounts_0.xlsx'
)
perth_revision <- 'accc-airport-monitoring-report-2023-24-regulatory-accounts-revised-perth-data_0.xlsx'
# Older workbook uses full airport names; newer workbooks use airport codes.
sheets_2022 <- c(BNE = 'Brisbane', MEL = 'Melbourne', PER = 'Perth', SYD = 'Sydney')

# The Table 1.1 description column is A or B (depending on workbook/sheet).
# The next three columns contain audited total, aeronautical, non-aeronautical values.
find_line <- function(x, label, required = TRUE) {
  a <- as.matrix(x[, 1:min(2, ncol(x)), drop = FALSE])
  # Preserve matrix dimensions: as.character(a) would flatten it.
  a[] <- str_to_lower(str_squish(as.character(a)))
  hit <- which(a == label, arr.ind = TRUE)
  if (nrow(hit) == 0L && !required) return(rep(NA_real_, 3))
  if (nrow(hit) != 1L) stop('Expected one row for: ', label, '; found ', nrow(hit))
  i <- hit[1, 'row']; j <- hit[1, 'col']
  as.numeric(unlist(x[i, (j + 1):(j + 3)], use.names = FALSE))
}

# ACCC expense cells can be blank when the amount is effectively zero.
# Replace a single blank with zero only if the other two activity amounts imply
# a zero allocation, within AUD 0.02 million after unit conversion.
resolve_expense_blank <- function(values, measure, airport, fy, divisor) {
  if (!anyNA(values)) return(values)

  # Some expense lines report a zero airport total but leave both activity
  # allocations blank (e.g. PER 2024-25, Other costs). Treat both as zero;
  # otherwise they would unnecessarily turn the expense subtotals into NA.
  if (is.finite(values[1]) && abs(values[1] / divisor) < 1e-9 &&
      all(is.na(values[2:3]))) {
    values[2:3] <- 0
  }

  if (sum(is.na(values)) == 1L &&
      all(is.finite(values[!is.na(values)]))) {
    missing <- which(is.na(values))
    # Positions: total, aeronautical, non-aeronautical.
    implied <- c(
      values[2] + values[3],
      values[1] - values[3],
      values[1] - values[2]
    )[missing]

    if (is.finite(implied) && abs(implied / divisor) < 0.02) {
      values[missing] <- 0
    }
  }

  if (anyNA(values)) {
    warning('Unresolved missing expense allocation: ', airport, ', ', fy, ', ', measure)
  }
  values
}

# Read one ACCC Table 1.1. Each extracted measure has three positions:
# audited airport total, aeronautical services, non-aeronautical services.
# Only the opening income statement is scanned, avoiding repeated labels later.
read_sheet_metrics <- function(path, sheet) {
  x <- read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = 'minimal', n_max = 34)
  metrics <- list(
    revenue = find_line(x, 'total revenue'),
    expenditure = find_line(x, 'total expenditure'),
    salaries_wages = find_line(x, 'salaries and wages'),
    services_utilities = find_line(x, 'services and utilities'),
    property_maintenance = find_line(x, 'property/leasing maintenance'),
    security_costs = find_line(x, 'security costs'),
    consultants_advisors = find_line(x, 'consultants and advisors'),
    general_administration = find_line(x, 'general administration'),
    other_costs = find_line(x, 'other costs'),
    depreciation_land = find_line(x, 'depreciation/amortisation of land'),
    depreciation_other = find_line(x, 'depreciation (other)'),
    amortisation_intangibles = find_line(x, 'amortisation of intangibles'),
    # Operating profit has all three allocations; reported EBIT has two.
    operating_profit_reported = find_line(x, 'operating profit/(loss)'),
    abnormal_items = find_line(x, 'abnormal items', required = FALSE),
    ebit_reported = find_line(x, 'earnings before interest and tax (ebit)'),
    interest_reported = find_line(x, 'interest'),
    profit_before_tax = find_line(x, 'earnings before tax (ebt)'),
    tax_charge_reported = find_line(x, 'tax charge'),
    net_profit_after_tax = find_line(x, 'profit/(loss) after tax'),
    dividends_paid = find_line(x, 'dividends paid', required = FALSE)
  )
  metrics
}

# Standardize source units and create the three activity rows. Preserve each
# source value at the level where the ACCC actually reports it; do not infer
# allocations for income-statement lines published only at airport level.
build_activity_rows <- function(metrics, path, sheet, fy, airport) {
  # Melbourne's source amounts are in dollars; the other airports use $'000.
  # Convert everything to AUD millions before applying monetary tolerances.
  divisor <- if (metrics$revenue[1] > 1e8) 1e6 else 1e3

  # Expense line-item blanks denote nil allocations in the ACCC tables
  # (e.g. Perth 2022-23 aeronautical "Other costs" is blank while both
  # total and non-aeronautical entries are zero). Avoid propagating NA
  # into cost groupings and activity reconciliation checks.
  expense_items <- c(
    'salaries_wages', 'services_utilities', 'property_maintenance',
    'security_costs', 'consultants_advisors', 'general_administration',
    'other_costs', 'depreciation_land', 'depreciation_other',
    'amortisation_intangibles'
  )
  # Apply the same reconciliation rule independently to each expense series.
  metrics[expense_items] <- imap(
    metrics[expense_items], resolve_expense_blank,
    airport = airport, fy = fy, divisor = divisor
  )

  # Operating profit has total/aero/non-aero values. Reported EBIT also
  # has an aeronautical value, but its non-aeronautical cell is blank in
  # the ACCC source; preserve the former and leave the latter missing.
  # Remaining financing, taxation and dividend lines are airport totals.
  # The ACCC "Interest" line is signed, not a separately itemised gross
  # expense and income split; do not invent a financing allocation.
  total_only <- c(
    'abnormal_items', 'interest_reported', 'profit_before_tax',
    'tax_charge_reported', 'net_profit_after_tax', 'dividends_paid'
  )
  audited <- vapply(total_only, function(m) metrics[[m]][1], numeric(1))
  metrics[total_only] <- NULL

  # Each metric now has three entries: total, aeronautical, non-aeronautical.
  # Create the three activity rows together instead of repeating each column.
  dat <- as_tibble(metrics) |>
    mutate(across(everything(), ~ .x / divisor)) |>
    mutate(activity = c('total', 'aeronautical', 'non_aeronautical'), .before = 1)

  # Lower income-statement lines are kept only for the audited total row.
  total_value <- function(name) c(audited[[name]] / divisor, NA_real_, NA_real_)

  dat <- dat |>
    mutate(
      depreciation_amortisation = depreciation_land + depreciation_other + amortisation_intangibles,
      purchased_services_other = services_utilities + property_maintenance +
        security_costs + consultants_advisors + general_administration + other_costs,
      expenditure_components = salaries_wages + purchased_services_other +
        depreciation_amortisation,
      expenditure_reconciliation = expenditure - expenditure_components,
      operating_profit = revenue - expenditure,
      ebitda = operating_profit + depreciation_amortisation,
      ebitda_margin = 100 * ebitda / revenue,
      airport = airport,
      fyear = fy,
      year = as.integer(str_sub(fy, 1, 4)) + 1L,
      # Retain audited total-only earnings/charges, converting signed interest
      # and tax charges to the positive-expense convention.
      abnormal_items = total_value('abnormal_items'),
      interest_reported = total_value('interest_reported'),
      interest_expense = -interest_reported,
      # Name the published ACCC interest line without assuming it is gross.
      interest_financing_costs = -interest_reported,
      interest_income = NA_real_,
      net_financing_costs = NA_real_,
      unusual_interest_flag = if_else(activity == 'total',
                           interest_expense > 2 * revenue, NA),
      profit_before_tax = total_value('profit_before_tax'),
      tax_charge_reported = total_value('tax_charge_reported'),
      income_tax_expense = -tax_charge_reported,
      net_profit_after_tax = total_value('net_profit_after_tax'),
      dividends_paid = abs(total_value('dividends_paid')),
      # Operating EBIT excludes abnormal items. Reported ACCC EBIT includes them.
      ebit = operating_profit,
      ebit_margin = 100 * operating_profit / revenue,
      ebit_reported_margin = 100 * ebit_reported / revenue,
      net_profit_margin = if_else(activity == 'total',
                                  100 * net_profit_after_tax / revenue, NA_real_),
      # Source operating profit can be reconciled for all three activities;
      # lower income-statement identities are available only for airport totals.
      operating_profit_reconciliation = operating_profit_reported - operating_profit,
      ebit_reconciliation = if_else(activity == 'total',
                     ebit_reported - operating_profit_reported - abnormal_items,
                     NA_real_),
      pretax_reconciliation = if_else(activity == 'total',
                        profit_before_tax - ebit_reported - interest_reported, NA_real_),
      net_profit_reconciliation = if_else(activity == 'total',
                   net_profit_after_tax - profit_before_tax - tax_charge_reported, NA_real_),
      source_file = basename(path),
      source_sheet = sheet,
      .before = 1
    )
  dat
}

# Warn about discrepancies in published allocations and accounting identities.
# This function does not change source values or calculated results.
validate_activity_rows <- function(dat, airport, fy) {
  # Compare all activity splits at once: total - aero - non-aero.
  split_vars <- c('revenue', 'expenditure', 'depreciation_amortisation',
                  'salaries_wages', 'purchased_services_other')
  split_values <- as.matrix(select(dat, all_of(split_vars)))
  split_residuals <- split_values[1, ] - colSums(split_values[-1, , drop = FALSE])
  bad_splits <- split_residuals[is.finite(split_residuals) & abs(split_residuals) > 0.02]

  iwalk(bad_splits, function(residual, measure) {
    warning('Activity split does not sum to airport total: ', airport, ', ', fy,
            ', ', measure, ' (difference ', round(residual, 3), ' AUD m)')
  })
  if (any(abs(dat$expenditure_reconciliation) > 0.03, na.rm = TRUE))
    warning('Expenditure components do not reconcile: ', airport, ', ', fy)
  # EBITDA - D&A = operating EBIT (depreciation is already in expenditure).
  if (any(abs(dat$ebitda - dat$depreciation_amortisation - dat$ebit) > 1e-7,
          na.rm = TRUE)) stop('EBIT/EBITDA mismatch: ', airport, ', ', fy)
  # 0.03 AUD million tolerance covers workbook rounding; do NOT repair source.
  if (any(abs(dat$operating_profit_reconciliation) > 0.03, na.rm = TRUE)) {
    warning('Reported operating profit does not reconcile for all activities: ',
            airport, ', ', fy)
  }
  reconciliation_vars <- c('ebit_reconciliation', 'pretax_reconciliation',
                           'net_profit_reconciliation')
  residuals <- unlist(dat[1, reconciliation_vars], use.names = TRUE)
  bad_reconciliations <- residuals[is.finite(residuals) & abs(residuals) > 0.03]

  iwalk(bad_reconciliations, function(residual, measure) {
    warning('Income-statement reconciliation: ', airport, ', ', fy, ', ',
            measure, ' = ', round(residual, 3), ' AUD m')
  })
  # Sydney regulatory-account Interest has very large source-reported values.
  # Preserve the audited cells, but alert the analyst before cross-airport use.
  if (is.finite(dat$interest_expense[1]) &&
      dat$interest_expense[1] > 2 * dat$revenue[1])
    warning('UNUSUAL SOURCE INTEREST: ', airport, ', ', fy,
            ': interest expense = ', round(dat$interest_expense[1], 2),
            ' AUD m versus revenue = ', round(dat$revenue[1], 2),
            ' AUD m. Verify audited line before interpretation.')
  invisible(dat)
}

# Read, calculate and validate a single airport-year worksheet.
parse_sheet <- function(path, sheet, fy, airport) {
  metrics <- read_sheet_metrics(path, sheet)
  dat <- build_activity_rows(metrics, path, sheet, fy, airport)
  validate_activity_rows(dat, airport, fy)
  dat
}

# One input row for each airport/year combination (3 years x 4 airports).
# All years from 2023-24 onward use the airport codes as sheet names.
input_files <- expand_grid(
  fyear = names(files),
  airport = c('BNE', 'MEL', 'PER', 'SYD')
) |>
  mutate(
    path = file.path(input_dir, unname(files[fyear])),
    sheet = if_else(fyear == '2022-23',
                    unname(sheets_2022[airport]), airport)
  )

# ACCC issued a corrected Perth worksheet for 2023-24. Use it when available.
revision_path <- file.path(input_dir, perth_revision)
if (file.exists(revision_path)) {
  input_files <- input_files |>
    mutate(
      path = if_else(fyear == '2023-24' & airport == 'PER', revision_path, path),
      sheet = if_else(fyear == '2023-24' & airport == 'PER', 'PER - revised', sheet)
    )
}

# Each row of input_files identifies one sheet to read. pmap() passes the
# four columns as named arguments to read_input_sheet(); list_rbind() stacks
# the resulting three-row data frames into one dataset.
read_input_sheet <- function(fyear, airport, path, sheet) {
  actual_sheets <- excel_sheets(path)
  sheet_idx <- match(str_trim(sheet), str_trim(actual_sheets))
  if (is.na(sheet_idx)) {
    stop('Sheet not found in ', basename(path), ': ', sheet)
  }
  parse_sheet(path, actual_sheets[[sheet_idx]], fyear, airport)
}

results <- input_files |>
  pmap(read_input_sheet) |>
  list_rbind() |>
  arrange(airport, year, factor(activity,
                               levels = c('total', 'aeronautical', 'non_aeronautical')))


# Source provenance ----------------------------------------------------

accc_regulatory_base_url <- "https://www.accc.gov.au/system/files/"

results <- results |>
  mutate(
    source_url = paste0(accc_regulatory_base_url, source_file),
    .after = source_sheet
  )


# Each source sheet must contribute one row for each of the three activities.
stopifnot(nrow(results) == nrow(input_files) * 3L,
          !anyDuplicated(results[c('airport', 'fyear', 'activity')]))

# Existing V4 output filename retained; original columns and EBITDA calculation unchanged.
write_csv(results, file.path(output_dir, 'accc_airport_ebitda_by_activity_2022_2025.csv'))

# Whole-airport income-statement export for Canadian comparisons: one row per
# airport/year by design. Activity-level operating profit and reported EBIT
# are available in the full 'results' / EBITDA-by-activity CSV above.
# Values remain NOMINAL AUD millions; convert to 2025 CAD downstream.
income_statement <- results |>
  filter(activity == 'total') |>
  select(airport, fyear, year, source_file, source_sheet, source_url,
         revenue, expenditure, operating_profit, depreciation_land,
         depreciation_other, amortisation_intangibles,
         depreciation_amortisation, ebitda, ebitda_margin,
         operating_profit_reported, abnormal_items, ebit, ebit_margin,
         ebit_reported, ebit_reported_margin, interest_reported,
         interest_expense, interest_financing_costs, interest_income,
         net_financing_costs, unusual_interest_flag,
         profit_before_tax, tax_charge_reported, income_tax_expense,
         net_profit_after_tax, net_profit_margin, dividends_paid,
         ends_with('_reconciliation'))
write_csv(income_statement,
          file.path(output_dir, 'accc_airport_income_statement_2022_2025.csv'))

# Long-format expenditure detail, with original types retained for Australia-only
# analysis and a grouped purchased-services aggregate for Canada comparisons.
expense_vars <- c(
  'salaries_wages', 'services_utilities', 'property_maintenance',
  'security_costs', 'consultants_advisors', 'general_administration',
  'other_costs', 'depreciation_land', 'depreciation_other',
  'amortisation_intangibles'
)
expenses_long <- results |>
  select(airport, fyear, year, activity, source_file, source_sheet, source_url,
         all_of(expense_vars)) |>
  tidyr::pivot_longer(all_of(expense_vars), names_to = 'item',
                      values_to = 'value_aud_m') |>
  mutate(category = case_when(
    item == 'salaries_wages' ~ 'labour',
    item %in% c('depreciation_land', 'depreciation_other',
                'amortisation_intangibles') ~ 'depreciation_amortisation',
    TRUE ~ 'purchased_services_other'
  ))
write_csv(expenses_long,
          file.path(output_dir, 'accc_airport_expenditure_by_type_2022_2025.csv'))

expense_groups <- expenses_long |>
  group_by(
    airport, fyear, year, activity, category,
    source_file, source_sheet, source_url
  ) |>
  summarise(
    value_aud_m = if (all(is.na(value_aud_m))) {
      NA_real_
    } else {
      sum(value_aud_m, na.rm = TRUE)
    },
    .groups = 'drop'
  )

write_csv(expense_groups,
          file.path(output_dir, 'accc_airport_expenditure_groups_2022_2025.csv'))
print(results |> select(airport, fyear, activity, revenue, expenditure,
                        salaries_wages, purchased_services_other,
                        depreciation_amortisation, expenditure_reconciliation,
                        ebitda, ebitda_margin, abnormal_items,
                        ebit_reported, interest_expense, profit_before_tax,
                        income_tax_expense, net_profit_after_tax,
                        dividends_paid), n = Inf)


# All exported records must have usable source URLs.
stopifnot(
  !anyNA(results$source_url),
  all(startsWith(results$source_url, "https://")),
  !anyNA(income_statement$source_url),
  !anyNA(expense_groups$source_url)
)

# Revised Perth source is retained when the revised input exists.
if (file.exists(revision_path)) {
  stopifnot(
    all(
      results$source_file[
        results$airport == 'PER' & results$fyear == '2023-24'
      ] == perth_revision
    )
  )
}


# Check source availability: reported operating profit exists for all three
# activities; reported EBIT exists for total and aeronautical only.
stopifnot(
  !anyNA(results$operating_profit_reported),
  !anyNA(results$ebit_reported[results$activity %in% c('total', 'aeronautical')]),
  all(is.na(results$ebit_reported[results$activity == 'non_aeronautical']))
)

# Perth 2024-25 reports zero total Other costs but leaves both activity
# cells blank; the zero-allocation repair must preserve complete subtotals.
per_2025 <- results |>
  filter(airport == 'PER', fyear == '2024-25')
stopifnot(nrow(per_2025) == 3L,
          all(per_2025$other_costs == 0),
          !anyNA(per_2025$purchased_services_other),
          !anyNA(per_2025$expenditure_components))

# Two audited-total regression checks (AUD millions): BNE 2024-25.
# Check row count explicitly: a missing row would otherwise produce logical(0).
stopifnot(nrow(results |> filter(airport == 'BNE', fyear == '2024-25',
                               activity == 'total')) == 1L)
stopifnot(abs(results$net_profit_after_tax[results$airport == 'BNE' &
                                               results$fyear == '2024-25' &
                                               results$activity == 'total'] - 342.29019053) < 0.001)
stopifnot(abs(results$ebitda_margin[results$airport == 'BNE' &
                                     results$fyear == '2024-25' &
                                     results$activity == 'total'] - 72.04) < 0.02)