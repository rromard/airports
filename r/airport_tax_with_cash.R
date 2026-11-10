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
            tax_payable) 


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
    
    # Some Melbourne figures are in dollars rather than $'000
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
      # Source cash-flow outflows are negative; report positive tax paid.
      # Treat Sydney's blank disclosures as zero, as established separately.
      cash_tax_paid = if_else(
        airport == "SYD" & is.na(cash_tax),
        0,
        -cash_tax * multiplier
      )
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
  select(airport, fyear, tax_payable)

# Calculate tax rates
tax_rates <- reg_profit |>
  left_join(ato_four, by = c("airport", "fyear")) |>
  mutate(
    year_end = parse_number(fyear) + 1,
    period = ifelse(year_end %in% 2016:2020, "2015-2020", "2021-2025"),
    
    cash_tax_paid = replace_na(cash_tax_paid, 0),
    
    benchmark_profit = if_else(
      airport == "SYD",
      operating_profit,
      profit_before_tax
    ),
    
    benchmark_tax = if_else(
      airport == "SYD",
      pmax(operating_profit, 0) * 0.30,
      tax_expense
    ),
    
    gross_tax_expense_rate = if_else(
      profit_before_tax > 0,
      tax_expense / profit_before_tax,
      NA_real_
    ),
    net_tax_payable_rate = if_else(
      profit_before_tax > 0,
      tax_payable / profit_before_tax,
      NA_real_
    ),
    cash_tax_profit_rate = if_else(
      benchmark_profit > 0,
      cash_tax_paid / benchmark_profit,
      NA_real_
    ),
    cash_tax_revenue_ratio = cash_tax_paid / total_revenue,
    
    tax_gap = benchmark_tax - cash_tax_paid
  ) |>
  arrange(airport, fyear) |>
  relocate(fyear, year_end, period)

write_csv(tax_rates, here("output", "taxation", "aus_airport_tax_rates.csv"))

tax_rates |> view()
  ggplot(aes(year_end, tax_gap, fill = airport)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~airport)

tax_rates |> 
  summarise(across(c(total_revenue:benchmark_tax,tax_gap), sum), .by = c(period, airport)) |> 
  mutate(gross_tax_rate = tax_expense / profit_before_tax,
         cash_tax_rate = cash_tax_paid / profit_before_tax,
         )

