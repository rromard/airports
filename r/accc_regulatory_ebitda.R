# ACCC regulatory accounts: EBITDA and income-statement results, 2022-23 to 2024-25
# Monetary amounts: nominal AUD millions; margins: percentages.
# V5 extends V4 without changing its existing EBITDA or expenditure outputs.
# Income-statement lines after operating profit (abnormal items, reported EBIT,
# interest, tax, net profit) are audited-airport TOTALS only; segment cells are NA.
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
sheets <- list(
  `2022-23` = c(BNE = 'Brisbane', MEL = 'Melbourne', PER = 'Perth', SYD = 'Sydney'),
  `2023-24` = c(BNE = 'BNE', MEL = 'MEL', PER = 'PER', SYD = 'SYD'),
  `2024-25` = c(BNE = 'BNE', MEL = 'MEL', PER = 'PER', SYD = 'SYD')
)

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

parse_sheet <- function(path, sheet, fy, airport) {
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
    # These lines belong to the audited total, NOT the two activity allocations.
    operating_profit_reported = find_line(x, 'operating profit/(loss)'),
    abnormal_items = find_line(x, 'abnormal items', required = FALSE),
    ebit_reported = find_line(x, 'earnings before interest and tax (ebit)'),
    interest_reported = find_line(x, 'interest'),
    profit_before_tax = find_line(x, 'earnings before tax (ebt)'),
    tax_charge_reported = find_line(x, 'tax charge'),
    net_profit_after_tax = find_line(x, 'profit/(loss) after tax'),
    dividends_paid = find_line(x, 'dividends paid', required = FALSE)
  )
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
  for (m in expense_items) {
    vals <- metrics[[m]]
    if (anyNA(vals)) {
      # Only impute when the observed cells reconcile exactly:
      # audited total = aeronautical + non-aeronautical.
      if (sum(is.na(vals)) == 1L &&
          (is.na(vals[1]) || all(is.finite(vals[!is.na(vals)])))) {
        missing_idx <- which(is.na(vals))
        candidate <- switch(as.character(missing_idx),
          '1' = vals[2] + vals[3],
          '2' = vals[1] - vals[3],
          '3' = vals[1] - vals[2]
        )
        if (is.finite(candidate) && abs(candidate) < 0.02 * 1e3) {
          vals[missing_idx] <- 0
        }
      }
      metrics[[m]] <- vals
      if (anyNA(vals)) warning('Unresolved missing expense allocation: ', airport,
                               ', ', fy, ', ', m)
    }
  }

  # Whole-airport income statement: take ONLY the audited-total column.
  # The ACCC "Interest" line is a signed financial result, not a separately
  # itemised gross interest expense plus interest income. Do not fabricate an
  # interest-income split or apply an extra financing adjustment.
  # Values in the activity columns for reported EBIT, etc. are not a full
  # standalone aeronautical/non-aeronautical income statement.
  total_only <- c(
    'operating_profit_reported', 'abnormal_items', 'ebit_reported',
    'interest_reported', 'profit_before_tax', 'tax_charge_reported',
    'net_profit_after_tax', 'dividends_paid'
  )
  audited <- vapply(total_only, function(m) metrics[[m]][1], numeric(1))
  metrics[total_only] <- NULL

  # Melbourne reports actual dollars despite a workbook header sometimes indicating $'000.
  divisor <- if (metrics$revenue[1] > 1e8) 1e6 else 1e3
  dat <- tibble(
    activity = c('total', 'aeronautical', 'non_aeronautical'),
    revenue = metrics$revenue / divisor,
    expenditure = metrics$expenditure / divisor,
    salaries_wages = metrics$salaries_wages / divisor,
    services_utilities = metrics$services_utilities / divisor,
    property_maintenance = metrics$property_maintenance / divisor,
    security_costs = metrics$security_costs / divisor,
    consultants_advisors = metrics$consultants_advisors / divisor,
    general_administration = metrics$general_administration / divisor,
    other_costs = metrics$other_costs / divisor,
    depreciation_land = metrics$depreciation_land / divisor,
    depreciation_other = metrics$depreciation_other / divisor,
    amortisation_intangibles = metrics$amortisation_intangibles / divisor
  ) |>
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
      # Preserve positive earnings and negative charges as reported in audited
      # accounts; convert the two cost categories to positive-expense convention.
      operating_profit_reported = if_else(activity == 'total',
                                          audited['operating_profit_reported'] / divisor, NA_real_),
      abnormal_items = if_else(activity == 'total',
                               audited['abnormal_items'] / divisor, NA_real_),
      ebit_reported = if_else(activity == 'total',
                             audited['ebit_reported'] / divisor, NA_real_),
      interest_reported = if_else(activity == 'total',
                                  audited['interest_reported'] / divisor, NA_real_),
      interest_expense = -interest_reported,
      # Name the published ACCC interest line without assuming it is gross.
      interest_financing_costs = -interest_reported,
      interest_income = NA_real_,
      net_financing_costs = NA_real_,
      unusual_interest_flag = if_else(activity == 'total',
                           interest_expense > 2 * revenue, NA),
      profit_before_tax = if_else(activity == 'total',
                                  audited['profit_before_tax'] / divisor, NA_real_),
      tax_charge_reported = if_else(activity == 'total',
                                    audited['tax_charge_reported'] / divisor, NA_real_),
      income_tax_expense = -tax_charge_reported,
      net_profit_after_tax = if_else(activity == 'total',
                                     audited['net_profit_after_tax'] / divisor, NA_real_),
      dividends_paid = if_else(activity == 'total',
                               abs(audited['dividends_paid'] / divisor), NA_real_),
      # Operating EBIT excludes abnormal items. Reported ACCC EBIT includes them.
      ebit = operating_profit,
      ebit_margin = 100 * operating_profit / revenue,
      ebit_reported_margin = if_else(activity == 'total',
                                      100 * ebit_reported / revenue, NA_real_),
      net_profit_margin = if_else(activity == 'total',
                                  100 * net_profit_after_tax / revenue, NA_real_),
      # Reconciliation residuals: should be approximately zero at audited total.
      operating_profit_reconciliation = if_else(activity == 'total',
                                   operating_profit_reported - operating_profit, NA_real_),
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
  # Total should equal the sum of the two business segments, modulo source rounding.
  component <- dat |> filter(activity != 'total')
  for (m in c('revenue', 'expenditure', 'depreciation_amortisation',
              'salaries_wages', 'purchased_services_other')) {
    if (all(is.finite(dat[[m]])) &&
        abs(dat[[m]][1] - sum(component[[m]])) > 0.02)
      warning('Activity split does not sum to airport total: ', airport, ', ', fy, ', ', m)
  }
  if (any(abs(dat$expenditure_reconciliation) > 0.03, na.rm = TRUE))
    warning('Expenditure components do not reconcile: ', airport, ', ', fy)
  # EBITDA - D&A = operating EBIT (depreciation is already in expenditure).
  if (any(abs(dat$ebitda - dat$depreciation_amortisation - dat$ebit) > 1e-7,
          na.rm = TRUE)) stop('EBIT/EBITDA mismatch: ', airport, ', ', fy)
  # 0.03 AUD million tolerance covers workbook rounding; do NOT repair source.
  for (m in c('operating_profit_reconciliation', 'ebit_reconciliation',
              'pretax_reconciliation', 'net_profit_reconciliation')) {
    if (is.finite(dat[[m]][1]) && abs(dat[[m]][1]) > 0.03)
      warning('Income-statement reconciliation: ', airport, ', ', fy, ', ',
              m, ' = ', round(dat[[m]][1], 3), ' AUD m')
  }
  # A blank abnormal-items cell stays NA. The observed difference between
  # reported EBIT and operating profit remains independently auditable.
  if (is.finite(dat$ebit_reported[1]) &&
      is.finite(dat$operating_profit_reported[1]) &&
      is.finite(dat$abnormal_items[1]) &&
      abs(dat$ebit_reconciliation[1]) > 0.03) {
    warning('Unexpected EBIT/abnormal-items bridge: ', airport, ', ', fy)
  }
  # Sydney regulatory-account Interest has very large source-reported values.
  # Preserve the audited cells, but alert the analyst before cross-airport use.
  if (is.finite(dat$interest_expense[1]) &&
      dat$interest_expense[1] > 2 * dat$revenue[1])
    warning('UNUSUAL SOURCE INTEREST: ', airport, ', ', fy,
            ': interest expense = ', round(dat$interest_expense[1], 2),
            ' AUD m versus revenue = ', round(dat$revenue[1], 2),
            ' AUD m. Verify audited line before interpretation.')
  dat
}

results <- imap_dfr(files, function(file, fy) {
  path <- file.path(input_dir, file)
  actual <- excel_sheets(path)
  imap_dfr(sheets[[fy]], function(sheet, airport) {
    use_path <- path
    if (fy == '2023-24' && airport == 'PER' && file.exists(file.path(input_dir, perth_revision))) {
      use_path <- file.path(input_dir, perth_revision)
      sheet <- 'PER - revised'
    } else {
      sheet <- actual[match(str_trim(sheet), str_trim(actual))]
    }
    parse_sheet(use_path, sheet, fy, airport)
  })
}) |>
  arrange(airport, year, factor(activity, levels = c('total', 'aeronautical', 'non_aeronautical')))

# Existing V4 output filename retained; original columns and EBITDA calculation unchanged.
write_csv(results, file.path(output_dir, 'accc_airport_ebitda_by_activity_2022_2025.csv'))

# Convenience subset for Canadian income-statement joins: 1 row / airport / year.
# Values remain in NOMINAL AUD millions; convert to 2025 CAD downstream.
income_statement <- results |>
  filter(activity == 'total') |>
  select(airport, fyear, year, source_file, source_sheet,
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
  select(airport, fyear, year, activity, source_file, source_sheet,
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
  group_by(airport, fyear, year, activity, category) |>
  summarise(value_aud_m = if (all(is.na(value_aud_m))) NA_real_ else sum(value_aud_m, na.rm = TRUE), .groups = 'drop')
write_csv(expense_groups,
          file.path(output_dir, 'accc_airport_expenditure_groups_2022_2025.csv'))
print(results |> select(airport, fyear, activity, revenue, expenditure,
                        salaries_wages, purchased_services_other,
                        depreciation_amortisation, expenditure_reconciliation,
                        ebitda, ebitda_margin, abnormal_items,
                        ebit_reported, interest_expense, profit_before_tax,
                        income_tax_expense, net_profit_after_tax,
                        dividends_paid), n = Inf)

# Two audited-total regression checks (AUD millions): BNE 2024-25.
stopifnot(abs(results$net_profit_after_tax[results$airport == 'BNE' &
                                               results$fyear == '2024-25' &
                                               results$activity == 'total'] - 342.29019053) < 0.001)
stopifnot(abs(results$ebitda_margin[results$airport == 'BNE' &
                                     results$fyear == '2024-25' &
                                     results$activity == 'total'] - 72.04) < 0.02)