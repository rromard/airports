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

      row <- hit[1, 1]
      col <- hit[1, 2] + 1L
      # Apply the same magnitude convention as the income statement,
      # independently to gross operating receipts in the cash-flow table.
      # Excel display formats and unit headers can conceal raw-value scaling.
      labels_col <- str_to_lower(str_squish(as.character(x[, col - 1L])))
      receipts_rows <- which(str_detect(labels_col,
        "^receipts from customers$") & seq_len(nrow(x)) < row)
      if (!length(receipts_rows)) {
        stop("Cannot identify cash-flow receipts: ", airport, " ", fyear)
      }
      receipts <- suppressWarnings(as.numeric(x[max(receipts_rows), col]))
      if (!is.finite(receipts) || receipts <= 0) {
        stop("Invalid cash-flow receipts: ", airport, " ", fyear)
      }
      cash_multiplier <- if (receipts > 1e8) 1 else 1000
      raw <- str_squish(as.character(x[row, col]))
      value <- suppressWarnings(as.numeric(raw))
      status <- "reported"
      if (is.na(value)) {
        # A blank contributes zero only if the operating cash-flow subtotal
        # reconciles without it. Do not silently discard an unknown payment.
        labels_col <- str_to_lower(str_squish(as.character(x[, col - 1L])))
        starts <- which(labels_col == "cash flows from operating activities" &
                          seq_len(nrow(x)) < row)
        ends <- which(labels_col == "net cash flows from operating activities" &
                        seq_len(nrow(x)) > row)
        if (length(starts) && length(ends) &&
            (is.na(raw) || raw %in% c("", "-", "–", "—"))) {
          first <- max(starts)
          last <- min(ends)
          if (last > first + 1L) {
            detail_rows <- setdiff(seq.int(first + 1L, last - 1L), row)
            details <- suppressWarnings(as.numeric(x[detail_rows, col]))
            total <- suppressWarnings(as.numeric(x[last, col]))
            # At most one source reporting unit of rounding.
            if (is.finite(total) && any(is.finite(details)) &&
                abs(total - sum(details, na.rm = TRUE)) <= 1 + 1e-6) {
              value <- 0
              status <- "zero_reconciled_blank"
            }
          }
        }
        if (is.na(value)) status <- "missing"
      }
      list(value = value, multiplier = cash_multiplier, status = status)
    }

    # Income statement unit heuristic is unchanged. Cash flows use their
    # independently inferred raw-value scale.
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
      # Blank cells are zero only when the cash-flow subtotal reconciles.
      cash_tax_paid = -cash_tax$value * cash_tax$multiplier,
      reg_cash_tax_missing = is.na(cash_tax$value),
      reg_cash_tax_status = cash_tax$status,
      reg_cash_unit_multiplier = cash_tax$multiplier,
      reg_source_file = basename(file),
      reg_source_sheet = sheet,
      reg_unit_multiplier = multiplier
    )
  })
})

# Regression checks against the published Melbourne cash-flow statements.
mel_cash_check <- reg_profit |> filter(airport == "MEL", fyear %in% c("2022-23", "2023-24", "2024-25")) |>
  arrange(fyear)
stopifnot(nrow(mel_cash_check) == 3L,
          isTRUE(all.equal(mel_cash_check$cash_tax_paid, c(0, 13557000, 94013000))),
          all(mel_cash_check$reg_cash_unit_multiplier == 1000))

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

# Zero payments and net refunds remain valid with positive EBT.
stopifnot(isTRUE(all.equal(positive_ratio(c(0, -10, 10), c(100, 100, 100)),
                           c(0, -0.1, 0.1))))

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
    # Broad Corporate Knights / Toronto Star (2017) tax gap:
    # https://corporateknights.com/responsible-investing/the-high-cost-of-low-corporate-taxes/
    # Statutory tax on accounting EBT, less cash income tax paid.
    # Retain signed EBT so loss years offset profits in cumulative windows.
    # This is a benchmark, not an assessed liability or proof of noncompliance.
    reg_expected_tax_on_accounting_profit = statutory_rate * reg_profit_before_tax,
    reg_tax_gap = reg_expected_tax_on_accounting_profit - reg_cash_tax_paid,
    # ATO payable less cash is a distinct, cross-source liability/payment
    # comparison; timing and entity scope can differ.
    ato_payable_less_cash = ato_tax_payable - reg_cash_tax_paid,
    # Additional diagnostic benchmarks, not interchangeable with tax_gap.
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

# Reconcile the overlapping ACCC observations against the EXISTING production
# extraction. This is a read-only diagnostic, not a second financial model.
# The production file is in nominal AUD millions; this tax panel is in AUD.
# Compare reported EBIT (including abnormal items), NOT operating EBIT or
# adjusted SCACH profits. Both extractors standardize tax expense to positive.
production_reg_path <- here("output", "accc_airport_ebitda_by_activity_2022_2025.csv")
overlap_fyears <- c("2022-23", "2023-24", "2024-25")
reconciliation_tolerance_aud <- 1000

tax_overlap <- tax_rates |>
  filter(fyear %in% overlap_fyears) |>
  transmute(
    airport, fyear, reg_source_file, reg_source_sheet,
    total_revenue = reg_total_revenue,
    reported_ebit = reg_ebit,
    profit_before_tax = reg_profit_before_tax,
    accounting_tax_expense = reg_tax_expense
  ) |>
  pivot_longer(
    cols = c(total_revenue, reported_ebit, profit_before_tax, accounting_tax_expense),
    names_to = "metric", values_to = "tax_extract_aud"
  )

# Typed empty reference ensures a fresh diagnostic is written even when the
# production extract is unavailable. Never report missing comparisons as matches.
production_overlap <- tibble(
  airport = character(), fyear = character(), metric = character(),
  production_source_file = character(), production_source_sheet = character(),
  production_extract_aud = double()
)
reference_status <- "available"
if (!file.exists(production_reg_path)) {
  reference_status <- "reference_file_missing"
} else {
  production_reg <- read_csv(production_reg_path, show_col_types = FALSE)
  required_reg_cols <- c(
    "airport", "fyear", "activity", "source_file", "source_sheet",
    "revenue", "ebit_reported", "profit_before_tax", "income_tax_expense"
  )
  if (!all(required_reg_cols %in% names(production_reg))) {
    reference_status <- "reference_schema_invalid"
  } else if (!all(vapply(
    production_reg[c("revenue", "ebit_reported", "profit_before_tax", "income_tax_expense")],
    function(x) is.numeric(x) || all(is.na(x)), logical(1)
  ))) {
    reference_status <- "reference_values_invalid"
  } else {
    production_reg <- production_reg |>
      filter(activity == "total", fyear %in% overlap_fyears,
             airport %in% names(airport_names))
    if (anyDuplicated(production_reg[c("airport", "fyear")])) {
      reference_status <- "reference_duplicate_keys"
    } else {
      production_overlap <- production_reg |>
        transmute(
          airport, fyear,
          production_source_file = source_file,
          production_source_sheet = source_sheet,
          total_revenue = revenue * 1e6,
          reported_ebit = ebit_reported * 1e6,
          profit_before_tax = profit_before_tax * 1e6,
          accounting_tax_expense = income_tax_expense * 1e6
        ) |>
        pivot_longer(
          cols = c(total_revenue, reported_ebit, profit_before_tax, accounting_tax_expense),
          names_to = "metric", values_to = "production_extract_aud"
        )
    }
  }
}

reg_reconciliation <- tax_overlap |>
  left_join(production_overlap, by = c("airport", "fyear", "metric"),
            relationship = "one-to-one") |>
  mutate(
    difference_aud = tax_extract_aud - production_extract_aud,
    absolute_difference_aud = abs(difference_aud),
    tolerance_aud = reconciliation_tolerance_aud,
    status = case_when(
      reference_status != "available" ~ reference_status,
      is.na(production_source_file) ~ "reference_row_missing",
      !is.finite(tax_extract_aud) | !is.finite(production_extract_aud) ~ "value_missing",
      absolute_difference_aud > tolerance_aud + 1e-6 ~ "discrepancy",
      TRUE ~ "within_tolerance"
    )
  ) |>
  arrange(airport, fyear, metric)

reconciliation_path <- here("output", "taxation", "accc_tax_extraction_reconciliation.csv")
write_csv(reg_reconciliation, reconciliation_path)
reconciliation_issues <- reg_reconciliation |> filter(status != "within_tolerance")
if (nrow(reg_reconciliation) != length(airport_names) * length(overlap_fyears) * 4L) {
  warning("Unexpected ACCC tax reconciliation coverage; review the airport-year panel")
}
if (nrow(reconciliation_issues)) {
  print(reconciliation_issues |>
          select(airport, fyear, metric, tax_extract_aud,
                 production_extract_aud, difference_aud, status),
        n = Inf)
  warning("ACCC tax extraction reconciliation: ", nrow(reconciliation_issues),
          " observations require review; see ", reconciliation_path,
          ". Neither source has been changed.")
} else {
  message("ACCC tax extraction reconciliation: ", nrow(reg_reconciliation),
          " observations within AUD ", reconciliation_tolerance_aud, " tolerance.")
}

# Regression diagnostic for the Perth FY2018-19 observation that motivated
# this revision. Warn if published inputs have since been revised.
per_check <- tax_rates |> filter(airport == "PER", fyear == "2018-19")
if (nrow(per_check) != 1L ||
    !isTRUE(abs(per_check$ato_taxable_income - 198150762) < 1) ||
    !isTRUE(abs(per_check$ato_tax_payable - 58978216) < 1)) {
  warning("Perth FY2018-19 ATO values differ from the verified reference; review source")
}

# Principal corporate tax gap: existing ACCC regulatory accounts for BNE/MEL/PER;
# SCACH adjusted accounting EBT for Sydney only. All output amounts nominal AUD.
# Parent-level Sydney cash tax is unknown; zero is an explicit scenario, not data.
sydney_assume_zero_parent_cash_tax <- TRUE

scach_path <- here("preserved_data", "scach_tax_inputs_2016_2025.csv")
if (!file.exists(scach_path)) stop("Missing Sydney tax inputs: ", scach_path)
scach <- read_csv(scach_path, show_col_types = FALSE,
  col_types = cols(
    airport = col_character(), year_end = col_integer(),
    reporting_entity = col_character(), reporting_period_end = col_date(),
    reported_ebt_aud_m = col_double(), accounting_tax_expense_aud_m = col_double(),
    cash_tax_paid_aud_m = col_double(), cash_tax_status = col_character(),
    shareholder_interest_aud_m = col_double(),
    rps_debt_cost_amortisation_aud_m = col_double(),
    exceptional_financing_adjustment_aud_m = col_double(),
    source_file = col_character(), source_url = col_character(),
    income_statement_pdf_page = col_integer(), finance_costs_pdf_page = col_integer(),
    cash_flow_pdf_page = col_integer(), notes = col_character()
  ))
required_scach <- c(
  "airport", "year_end", "reporting_entity", "reporting_period_end",
  "reported_ebt_aud_m", "accounting_tax_expense_aud_m", "cash_tax_paid_aud_m",
  "cash_tax_status", "shareholder_interest_aud_m", "rps_debt_cost_amortisation_aud_m",
  "exceptional_financing_adjustment_aud_m", "source_file", "source_url",
  "income_statement_pdf_page", "finance_costs_pdf_page", "cash_flow_pdf_page", "notes"
)
if (!all(required_scach %in% names(scach))) stop("Sydney tax input schema incomplete")
if (nrow(problems(scach))) stop("Parsing errors in Sydney tax inputs")
if (anyNA(scach[c("airport", "year_end")]) ||
    any(scach$airport != "SYD") ||
    !identical(sort(scach$year_end), 2016:2025)) {
  stop("Sydney tax inputs must contain exactly one row per year, 2016–2025")
}
required_nonmissing <- setdiff(required_scach, c("cash_tax_paid_aud_m", "source_url", "notes"))
if (anyNA(scach[required_nonmissing]) ||
    any(scach$cash_tax_status != "not_separately_disclosed_at_scach") ||
    any(!is.na(scach$cash_tax_paid_aud_m)) ||
    any(scach$reporting_period_end != as.Date(paste0(scach$year_end, "-12-31")))) {
  stop("Unexpected SCACH missingness, cash-tax disclosure status or reporting dates")
}
scach <- scach |>
  transmute(
    airport, year_end,
    scach_reporting_entity = reporting_entity,
    scach_reporting_period_end = reporting_period_end,
    scach_reported_ebt = reported_ebt_aud_m * 1e6,
    scach_tax_expense = accounting_tax_expense_aud_m * 1e6,
    scach_cash_tax_paid = cash_tax_paid_aud_m * 1e6,
    scach_cash_tax_status = cash_tax_status,
    shareholder_interest = shareholder_interest_aud_m * 1e6,
    rps_debt_cost_amortisation = rps_debt_cost_amortisation_aud_m * 1e6,
    exceptional_financing_adjustment = exceptional_financing_adjustment_aud_m * 1e6,
    scach_source_file = source_file, scach_source_url = source_url,
    scach_income_statement_pdf_page = income_statement_pdf_page,
    scach_finance_costs_pdf_page = finance_costs_pdf_page,
    scach_cash_flow_pdf_page = cash_flow_pdf_page,
    scach_source_notes = notes
  )

tax_rates <- tax_rates |>
  left_join(scach, by = c("airport", "year_end"), relationship = "one-to-one") |>
  mutate(
    tax_gap_method = if_else(airport == "SYD",
      "Illustrative SCACH adjusted corporate tax gap",
      "ACCC regulatory accounting EBT and reported cash taxes"),
    analysis_period_end = if_else(airport == "SYD", scach_reporting_period_end,
                                  as.Date(paste0(year_end, "-06-30"))),
    analysis_reported_ebt = if_else(airport == "SYD", scach_reported_ebt,
                                    reg_profit_before_tax),
    analysis_tax_expense = if_else(airport == "SYD", scach_tax_expense, reg_tax_expense),
    analysis_ebt = if_else(airport == "SYD",
      scach_reported_ebt + shareholder_interest + rps_debt_cost_amortisation +
        exceptional_financing_adjustment, reg_profit_before_tax),
    analysis_cash_tax_paid = if_else(airport == "SYD", scach_cash_tax_paid, reg_cash_tax_paid),
    analysis_accounting_etr = positive_ratio(analysis_tax_expense, analysis_reported_ebt),
    cash_etr = positive_ratio(analysis_cash_tax_paid, analysis_ebt),
    cash_tax_assumed = airport == "SYD" & is.na(analysis_cash_tax_paid) &
      sydney_assume_zero_parent_cash_tax,
    cash_tax_for_gap = if_else(cash_tax_assumed, 0, analysis_cash_tax_paid),
    expected_tax_on_accounting_profit = statutory_rate * analysis_ebt,
    tax_gap = expected_tax_on_accounting_profit - cash_tax_for_gap
  )
if (anyNA(tax_rates$analysis_ebt[tax_rates$airport == "SYD"])) {
  stop("Missing adjusted Sydney EBT after join")
}
if (any(tax_rates$cash_tax_assumed)) {
  warning("Sydney tax gap is hypothetical: zero corporate cash tax assumed; ",
          "actual payments unverified. Investor-level taxation is outside scope.")
}

# Period ratios use sums, not averages of annual ratios. Missing cash or
# unmatched ATO records propagate NA rather than silently understating totals.
# Non-positive ATO taxable income is a zero proxy, not a net loss estimate.
tax_periods <- tax_rates |>
  summarise(
    n_years = n(),
    n_ato_matched = sum(ato_matched),
    n_cash_reported = sum(!reg_cash_tax_missing),
    n_analysis_cash_reported = sum(!is.na(analysis_cash_tax_paid)),
    n_cash_assumed = sum(cash_tax_assumed),
    tax_gap_method = first(tax_gap_method),
    n_ato_taxable_nonpositive = sum(ato_taxable_income_nonpositive, na.rm = TRUE),
    across(c(reg_total_revenue, reg_ebit, reg_profit_before_tax,
             reg_tax_expense, reg_cash_tax_paid, ato_total_income,
             ato_taxable_income, ato_tax_payable, ato_statutory_tax,
             ato_statutory_less_payable, reg_expected_tax_on_accounting_profit,
             reg_tax_gap, analysis_reported_ebt, analysis_tax_expense,
             analysis_cash_tax_paid, analysis_ebt, cash_tax_for_gap,
             expected_tax_on_accounting_profit,
             tax_gap, ato_payable_less_cash, accounting_expense_less_cash,
             statutory_ebt_less_cash, statutory_ebit_less_cash), sum),
    .by = c(period, airport)
  ) |>
  mutate(
    analysis_accounting_etr = positive_ratio(analysis_tax_expense, analysis_reported_ebt),
    cash_etr = positive_ratio(analysis_cash_tax_paid, analysis_ebt),
    ato_payable_rate = positive_ratio(ato_tax_payable, ato_taxable_income),
    accounting_etr = positive_ratio(reg_tax_expense, reg_profit_before_tax),
    cash_accounting_etr = positive_ratio(reg_cash_tax_paid, reg_profit_before_tax),
    payable_accounting_etr = positive_ratio(ato_tax_payable, reg_profit_before_tax),
    cash_taxable_income_rate = positive_ratio(reg_cash_tax_paid, ato_taxable_income),
    cash_tax_revenue_ratio = positive_ratio(reg_cash_tax_paid, reg_total_revenue)
  )

# Check annual and cumulative gap identities and the previously reconciled
# SCACH CY2025 EBT: 97.4 + 136.3 + 19.4 + 471.8 = 724.9 million.
for (panel in list(tax_rates, tax_periods)) {
  residual <- with(panel, tax_gap -
    (0.30 * analysis_ebt - cash_tax_for_gap))
  if (any(abs(residual) > 0.01, na.rm = TRUE)) stop("Tax gap identity failed")
}
syd_2025 <- tax_rates |> filter(airport == "SYD", year_end == 2025)
stopifnot(nrow(syd_2025) == 1L,
          abs(syd_2025$analysis_ebt - 724.9e6) < 1,
          is.na(syd_2025$analysis_cash_tax_paid),
          is.na(syd_2025$cash_etr))
if (nrow(tax_periods) != 8L || any(tax_periods$n_years != 5L)) {
  stop("Incomplete five-year tax periods")
}
# Ensure the principal measure has not changed ACCC data for the other airports.
for (panel in list(tax_rates, tax_periods)) {
  other <- panel |> filter(airport != "SYD")
  stopifnot(isTRUE(all.equal(other$analysis_ebt, other$reg_profit_before_tax)),
            isTRUE(all.equal(other$analysis_cash_tax_paid, other$reg_cash_tax_paid)),
            isTRUE(all.equal(other$tax_gap, other$reg_tax_gap)))
}
stopifnot(all(is.na(tax_rates$cash_etr[tax_rates$airport == "SYD"])),
          all(is.na(tax_periods$cash_etr[tax_periods$airport == "SYD"])))
if (sydney_assume_zero_parent_cash_tax) {
  stopifnot(all(tax_periods$n_cash_assumed[tax_periods$airport == "SYD"] == 5L))
} else {
  stopifnot(all(is.na(tax_periods$tax_gap[tax_periods$airport == "SYD"])))
}

write_csv(tax_rates, here("output", "taxation", "aus_airport_tax_rates.csv"))
write_csv(tax_periods, here("output", "taxation", "aus_airport_tax_periods.csv"))
