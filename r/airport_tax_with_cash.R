xfun::pkg_attach('tidyverse', 'here', 'janitor', 'openxlsx')

urls <- c(
"https://data.gov.au/data/dataset/c2524c87-cea4-4636-acac-599a82048a26/resource/15bf4eaa-7a14-48e9-8a3f-092df32a644e/download/2024-25-corporate-report-of-entity-tax-information.xlsx",
"https://data.gov.au/data/dataset/c2524c87-cea4-4636-acac-599a82048a26/resource/491b366b-aa6f-4b1c-b39d-cebaeeb6f874/download/2023-24-corporate-report-of-entity-tax-information.xlsx",
"https://data.gov.au/data/dataset/c2524c87-cea4-4636-acac-599a82048a26/resource/3281f733-6f53-431b-84f5-d824c6bd9e60/download/2022-23-corporate-report-of-entity-tax-information.xlsx",
"https://data.gov.au/data/dataset/c2524c87-cea4-4636-acac-599a82048a26/resource/6eaf8203-a79b-43f0-9671-feeb1a44cbff/download/2021-22-corporate-report-of-entity-tax-information.xlsx",
"https://data.gov.au/data/dataset/c2524c87-cea4-4636-acac-599a82048a26/resource/80a01133-4281-43ce-b5ef-5535d61e1c1f/download/2020-21-corporate-report-of-entity-tax-information.xlsx",
"https://data.gov.au/data/dataset/c2524c87-cea4-4636-acac-599a82048a26/resource/db83e667-819f-41f3-a8b7-fcb13366105b/download/2019-20-corporate-report-of-entity-tax-information.xlsx",
"https://data.gov.au/data/dataset/c2524c87-cea4-4636-acac-599a82048a26/resource/827f68ea-83c0-440e-bb6d-4118644b7efd/download/2018-19-corporate-report-of-entity-tax-information.xlsx",
"https://data.gov.au/data/dataset/c2524c87-cea4-4636-acac-599a82048a26/resource/69b1061c-3769-48bd-b5a1-05e725543f6c/download/2017-18-corporate-report-of-entity-tax-information.xlsx",
"https://data.gov.au/data/dataset/c2524c87-cea4-4636-acac-599a82048a26/resource/5eb443b4-d0bb-4a88-a189-4523dbcd7f15/download/2016-17-corporate-report-of-entity-tax-information.xlsx",
"https://data.gov.au/data/dataset/c2524c87-cea4-4636-acac-599a82048a26/resource/b84c2b8d-c595-4219-987d-fc1add7f00f0/download/2015-16-corporate-report-of-entity-tax-information.xlsx",
"https://data.gov.au/data/dataset/c2524c87-cea4-4636-acac-599a82048a26/resource/1e8c8ae0-81d1-4780-a669-9e4a2a6ba1a4/download/2014-15-corporate-report-of-entity-tax-information.xlsx",
"https://data.gov.au/data/dataset/c2524c87-cea4-4636-acac-599a82048a26/resource/237d7ede-3a63-4b9b-9434-2f79b9d70ce8/download/2013-14corporate-report-of-entity-tax-information.xlsx"
)

# Download ATO corporate tax transparency workbooks -----------------------

input_dir <- here("data", "australia", "ato")
dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)

files <- file.path(input_dir, basename(urls))

walk2(urls, files, function(url, file) {
  if (!file.exists(file)) {
    download.file(url, file, mode = "wb", quiet = TRUE)
  }
})

# Import and combine local workbooks --------------------------------------

sheet_lookup <- tribble(~year, ~sheet,
  c(2025:2019, 2017), "Income tax details",
  2018, "Income tax",
  2016, "2015-16",
  2015, "2014-15",
  2014, "Combined"
) |> 
  unnest(year) |> 
  mutate(year = year - 1) |> 
  arrange(year)

ato_tax <- tibble(file = files, year = as.integer(str_extract(basename(files), "^\\d{4}"))) |>
  left_join(sheet_lookup, by = "year") |>
  pmap_dfr(function(file, year, sheet) {
    openxlsx::read.xlsx(file, sheet = sheet) |>
      clean_names() |>
      mutate(
        across(everything(), as.character),
        fyear = paste0(year, "-", str_sub(year + 1, -2)),
        .before = 1
      )
  }) |>
  as_tibble() |>
  transmute(fyear,
            income_year,
            year_end = parse_number(fyear)+1,
            company = name,
            abn,
            total_income,
            taxable_income,
            tax_payable) |> 
  mutate(
    across(total_income:tax_payable, parse_number),
    # ATO suppresses amounts of zero or less. Keep flags for source blanks;
    # zero is a non-positive-income proxy, NOT a measured taxable loss.
    # https://www.ato.gov.au/businesses-and-organisations/corporate-tax-measures-and-assurance/large-business/corporate-tax-transparency/report-of-entity-tax-information
    taxable_income_nonpositive = is.na(taxable_income),
    tax_payable_nonpositive = is.na(tax_payable),
    across(c(taxable_income, tax_payable), ~replace_na(.x, 0))
  ) |> 
  filter(!is.na(company))

# Export ------------------------------------------------------------------

## Airport only subset
dir.create(here("output", "taxation"), recursive = TRUE, showWarnings = FALSE)

airport_lookup <- tribble(
  ~company, ~airport,
  "ADELAIDE AIRPORT LIMITED", "Adelaide",
  "AIRPORT DEVELOPMENT GROUP PTY LIMITED", "Darwin / Alice Springs / Tennant Creek",
  "AUSTRALIA PACIFIC AIRPORTS CORPORATION LIMITED", "Melbourne / Launceston",
  "CANBERRA AIRPORT HOLDINGS PTY LTD", "Canberra",
  "CAPITAL PROPERTY FINANCE", "Canberra",
  "PERTH AIRPORT DEVELOPMENT GROUP PTY LTD", "Perth",
  "QUEENSLAND AIRPORTS LIMITED", "Gold Coast / Townsville / Mount Isa / Longreach",
  "SYDNEY AIRPORT LIMITED", "Sydney",
  "SYDNEY AVIATION ALLIANCE HOLDINGS PTY LTD", "Sydney",
  "BAC HOLDINGS LIMITED", "Brisbane",
  "BAC HOLDINGS LIMTED", "Brisbane",
  "ESSENDON AIRPORT HOLDINGS PTY LTD", "Essendon Fields",
  "TRUSTEE FOR NORTH QUEENSLAND AIRPORTS NO. 2 (MACKAY) TRUST", "Mackay",
)

airports_tax <- ato_tax |>
  filter(company %in% airport_lookup$company) |>
  left_join(airport_lookup) |>
  arrange(airport, fyear) |>
  transmute(airport,
            big4 = airport %in% c("Brisbane", "Melbourne / Launceston", "Perth", "Sydney"),
            company,
            abn,
            fyear,
            year_end,
            total_income,
            taxable_income,
            tax_payable,
            taxable_income_nonpositive,
            tax_payable_nonpositive)


write_csv(
  airports_tax,
  here("output", "taxation", "ato_airport_tax_2013_2025.csv")
)

## Main data

ato_tax <- ato_tax |> 
  left_join(airport_lookup) |> 
  mutate(
    airport_select = company %in% airport_lookup$company,
    big4 = ifelse(
      airport %in% c("Brisbane", "Melbourne / Launceston", "Perth", "Sydney"),
      airport, "All other companies")
  )

write_csv(
  ato_tax,
  here("output", "taxation", "ato_corporate_tax_2013_2025.csv")
)

# Airport effective tax rates --------------------------------------------

library(rvest)

# Download ACCC regulatory accounts
fyears <- sprintf("%d-%02d", 2015:2024, (2016:2025) %% 100)

reg_dir <- here("data", "australia", "regulatory_accounts")
dir.create(reg_dir, recursive = TRUE, showWarnings = FALSE)

get_reg_url <- function(fyear) {
  page <- paste0(
    "https://www.accc.gov.au/about-us/publications/",
    "serial-publications/airport-monitoring-reports/",
    "airport-monitoring-report-", fyear
  )
  
  links <- rvest::read_html(page) |>
    rvest::html_elements("a")
  
  labels <- rvest::html_text2(links)
  hrefs <- rvest::html_attr(links, "href")
  
  i <- which(
    str_detect(labels, regex("regulatory accounts", ignore_case = TRUE)) &
      str_detect(hrefs, regex("\\.xlsx$", ignore_case = TRUE))
  )
  
  if (!length(i)) stop("Regulatory accounts not found for ", fyear)
  
  xml2::url_absolute(hrefs[i[1]], page)
}

reg_files <- file.path(reg_dir, paste0("accc_", fyears, ".xlsx"))

walk2(fyears, reg_files, function(fyear, file) {
  if (!file.exists(file)) {
    download.file(get_reg_url(fyear), file, mode = "wb")
  }
})

# Extract ACCC profits, tax expense and cash tax payments -----------------

airport_names <- c(
  BNE = "Brisbane|^BNE$",
  MEL = "Melbourne|^MEL$",
  PER = "Perth|^PER$",
  SYD = "Sydney|^SYD$"
)

reg_profit <- map2_dfr(reg_files, fyears, function(file, fyear) {
  
  sheets <- openxlsx::getSheetNames(file)
  
  map_dfr(names(airport_names), function(airport) {
    
    sheet <- sheets[str_detect(
      sheets,
      regex(airport_names[[airport]], ignore_case = TRUE)
    )]
    
    if (length(sheet) != 1) {
      stop("Cannot identify worksheet: ", airport, " ", fyear)
    }
    
    x <- openxlsx::read.xlsx(
      file,
      sheet = sheet,
      colNames = FALSE,
      skipEmptyCols = FALSE,
      skipEmptyRows = FALSE
    ) |>
      as.matrix()
    
    # Find the label in the first two columns, then its first numeric value
    get_value <- function(pattern) {
      
      labels <- str_to_lower(str_squish(as.character(x[, 1:2])))
      dim(labels) <- c(nrow(x), 2)
      
      hit <- which(
        matrix(
          str_detect(labels, regex(pattern, ignore_case = TRUE)),
          nrow = nrow(x)
        ),
        arr.ind = TRUE
      )
      
      if (nrow(hit) != 1) {
        stop("Cannot identify ", pattern, ": ", airport, " ", fyear)
      }
      
      values <- suppressWarnings(as.numeric(
        x[hit[1, 1], (hit[1, 2] + 1):ncol(x)]
      ))
      
      values[which(!is.na(values))[1]]
    }

    # Cash-flow statement labels may appear anywhere on the worksheet.
    get_cash_tax <- function() {
      labels <- matrix(
        str_detect(
          str_squish(as.character(x)),
          regex("^(income tax paid|income and other taxes paid)$", ignore_case = TRUE)
        ),
        nrow = nrow(x)
      )

      hit <- which(labels, arr.ind = TRUE)
      if (nrow(hit) != 1) {
        stop("Cannot identify cash tax: ", airport, " ", fyear)
      }

      values <- suppressWarnings(as.numeric(
        x[hit[1, 1], (hit[1, 2] + 1):ncol(x)]
      ))
      if (all(is.na(values))) return(NA_real_)
      values[which(!is.na(values))[1]]
    }
    
    # Existing unit heuristic retained pending verification of every source sheet.
    # Export the applied factor and sheet name for review.
    multiplier <- if (get_value("^total revenue$") > 1e8) 1 else 1000
    
    cash_tax <- get_cash_tax()

    tibble(
      airport = airport,
      fyear = fyear,
      total_revenue = get_value("^total revenue$") * multiplier,
      operating_profit = get_value("^earnings before interest and tax \\(ebit\\)$") * multiplier,
      profit_before_tax =
        get_value("^earnings before tax \\(ebt\\)$") * multiplier,
      tax_expense =
        -get_value("^(tax charge|income tax benefit)$") * multiplier,
      # Source cash-flow outflows are negative; refunds are negative tax paid.
      # Do not infer a zero cash payment from a blank disclosure.
      cash_tax_paid = -cash_tax * multiplier,
      reg_cash_tax_missing = is.na(cash_tax),
      reg_source_file = basename(file),
      reg_source_sheet = sheet,
      reg_unit_multiplier = multiplier
    )
  })
})

# Match the ATO reporting entities by ABN
ato_four <- ato_tax |>
  select(-airport) |>
  mutate(
    abn = as.character(abn),
    tax_payable = as.numeric(tax_payable)
  ) |>
  inner_join(
    tribble(
      ~abn, ~airport,
      "67108568038", "BNE",
      "89069775266", "MEL",
      "53076286630", "PER",
      "18165056360", "SYD",
      "54654912197", "SYD"
    ),
    by = "abn"
  ) |>
  filter(
    airport != "SYD" |
      if_else(fyear >= "2022-23",
              abn == "54654912197",
              abn == "18165056360")
  ) |>
  transmute(
    airport, fyear,
    ato_abn = abn, ato_company = company, ato_income_year = income_year,
    ato_total_income = total_income,
    ato_taxable_income = taxable_income,
    ato_tax_payable = tax_payable,
    ato_taxable_income_nonpositive = taxable_income_nonpositive,
    ato_tax_payable_nonpositive = tax_payable_nonpositive
  )

# Calculate tax rates -----------------------------------------------------
# Only the ATO payable/taxable-income ratio is entirely tax-return based.
# Cross-source ratios are diagnostic: the legal entities may differ.
positive_ratio <- function(numerator, denominator) {
  if_else(!is.na(denominator) & denominator > 0,
          numerator / denominator, NA_real_)
}

assert_unique_tax_keys <- function(x, label) {
  if (anyNA(x$airport) || anyNA(x$fyear) ||
      anyDuplicated(x[c("airport", "fyear")])) {
    stop("Missing or duplicate airport-year keys: ", label)
  }
}

assert_unique_tax_keys(reg_profit, "ACCC")
assert_unique_tax_keys(ato_four, "ATO")

tax_rates <- reg_profit |>
  rename(
    reg_total_revenue = total_revenue,
    reg_ebit = operating_profit,
    reg_profit_before_tax = profit_before_tax,
    reg_tax_expense = tax_expense,
    reg_cash_tax_paid = cash_tax_paid
  ) |>
  left_join(ato_four, by = c("airport", "fyear"), relationship = "one-to-one") |>
  mutate(
    year_end = parse_number(fyear) + 1,
    period = if_else(year_end %in% 2016:2020, "2015-2020", "2021-2025"),
    ato_matched = !is.na(ato_abn),
    statutory_rate = 0.30,
    ato_statutory_tax = statutory_rate * ato_taxable_income,
    ato_statutory_less_payable = ato_statutory_tax - ato_tax_payable,
    # Tolerate one dollar of rounding, but flag rather than rewrite source data.
    ato_payable_above_statutory = ato_tax_payable > ato_statutory_tax + 1,
    ato_payable_rate = positive_ratio(ato_tax_payable, ato_taxable_income),
    accounting_etr = positive_ratio(reg_tax_expense, reg_profit_before_tax),
    cash_accounting_etr = positive_ratio(reg_cash_tax_paid, reg_profit_before_tax),
    payable_accounting_etr = positive_ratio(ato_tax_payable, reg_profit_before_tax),
    cash_taxable_income_rate = positive_ratio(reg_cash_tax_paid, ato_taxable_income),
    taxable_accounting_income_ratio = if_else(
      ato_taxable_income_nonpositive %in% FALSE,
      positive_ratio(ato_taxable_income, reg_profit_before_tax), NA_real_
    ),
    cash_tax_revenue_ratio = positive_ratio(reg_cash_tax_paid, reg_total_revenue),
    # Uniform benchmarks, kept separate from each other. Neither is a
    # measure of tax avoidance or a same-year cash tax liability.
    accounting_expense_less_cash = reg_tax_expense - reg_cash_tax_paid,
    statutory_ebt_less_cash = pmax(reg_profit_before_tax, 0) * statutory_rate - reg_cash_tax_paid,
    statutory_ebit_less_cash = pmax(reg_ebit, 0) * statutory_rate - reg_cash_tax_paid
  ) |>
  arrange(airport, fyear) |>
  relocate(airport, fyear, year_end, period)

if (nrow(tax_rates) != length(fyears) * length(airport_names)) {
  stop("Unexpected number of ACCC airport-year records")
}
if (any(!tax_rates$ato_matched)) {
  warning("Unmatched ATO airport-years; see ato_matched in tax export")
}
if (any(tax_rates$ato_payable_above_statutory, na.rm = TRUE)) {
  warning("ATO tax payable exceeds 30% of ATO taxable income; review flagged records")
}
if (any(tax_rates$reg_cash_tax_missing)) {
  warning("Missing ACCC cash tax observations retained as NA")
}

# Regression diagnostic for the Perth FY2018-19 observation that motivated
# this revision. Warn if published inputs have since been revised.
per_check <- tax_rates |> filter(airport == "PER", fyear == "2018-19")
if (nrow(per_check) != 1L ||
    !isTRUE(abs(per_check$ato_taxable_income - 198150762) < 1) ||
    !isTRUE(abs(per_check$ato_tax_payable - 58978216) < 1)) {
  warning("Perth FY2018-19 ATO values differ from the verified reference; review source")
}

# Period ratios use sums, not averages of annual ratios. Missing cash or
# unmatched ATO records propagate NA rather than silently understating totals.
# Non-positive ATO taxable income is a zero proxy, not a net loss estimate.
tax_periods <- tax_rates |>
  summarise(
    n_years = n(),
    n_ato_matched = sum(ato_matched),
    n_cash_reported = sum(!reg_cash_tax_missing),
    n_ato_taxable_nonpositive = sum(ato_taxable_income_nonpositive, na.rm = TRUE),
    across(c(reg_total_revenue, reg_ebit, reg_profit_before_tax,
             reg_tax_expense, reg_cash_tax_paid, ato_total_income,
             ato_taxable_income, ato_tax_payable, ato_statutory_tax,
             ato_statutory_less_payable, accounting_expense_less_cash,
             statutory_ebt_less_cash, statutory_ebit_less_cash), sum),
    .by = c(period, airport)
  ) |>
  mutate(
    ato_payable_rate = positive_ratio(ato_tax_payable, ato_taxable_income),
    accounting_etr = positive_ratio(reg_tax_expense, reg_profit_before_tax),
    cash_accounting_etr = positive_ratio(reg_cash_tax_paid, reg_profit_before_tax),
    payable_accounting_etr = positive_ratio(ato_tax_payable, reg_profit_before_tax),
    cash_taxable_income_rate = positive_ratio(reg_cash_tax_paid, ato_taxable_income),
    cash_tax_revenue_ratio = positive_ratio(reg_cash_tax_paid, reg_total_revenue)
  )

write_csv(tax_rates, here("output", "taxation", "aus_airport_tax_rates.csv"))
write_csv(tax_periods, here("output", "taxation", "aus_airport_tax_periods.csv"))
