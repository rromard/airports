# Shared observed historical ACCC/FAC aeronautical yield preparation.
# Both the optional counterfactual and standard presentation stage use
# exactly the same CPI calculation and historical airport panel.
build_observed_australian_history <- function(historical_path, aus_airports = c("BNE", "MEL", "PER", "SYD")) {
  # Australian CPI, aggregated to financial years ending June
  historical_cpi <- readabs::read_cpi() |>
    mutate(
      year = as.integer(format(date, "%Y")) +
        as.integer(format(date, "%m") >= "07")
    ) |>
    group_by(year) |>
    summarise(cpi = mean(cpi), .groups = "drop") |>
    filter(year %in% c(1998:2007, 2025L))
  
  # Use the same CPI vintage and index base for historical and target prices.
  # Scaling the existing real yield by this constant does not alter growth.
  historical_cpi_2025 <- historical_cpi$cpi[historical_cpi$year == 2025L]
  if (length(historical_cpi_2025) != 1L ||
      !is.finite(historical_cpi_2025) || historical_cpi_2025 <= 0)
    stop("Missing valid Australian FY2025 CPI for historical dollar yields.")
  
  history <- readr::read_csv(historical_path, show_col_types = FALSE) |>
    filter(airport %in% aus_airports, as.character(chart) == '2.2',
           measure == 'aero_revenue_per_pax',
           adjustment %in% c('ansett_adjusted', 'adjusted') |
             (airport == 'SYD' & year == 1998L &
                adjustment == 'FAC_reported'),
           year %in% 1998:2007) |>
    transmute(airport, year = as.integer(year),
              nominal_aud_pp = as.numeric(value)) |>
    left_join(historical_cpi, by = 'year') |>
    mutate(real_yield = nominal_aud_pp / cpi)
  
  # Require all 40 historical airport-years, with unique keys and
  # positive real yields after the CPI join.
  if (nrow(history) != 4L * 10L ||
      anyDuplicated(history[c('airport', 'year')]) ||
      any(!is.finite(history$real_yield)) ||
      any(history$real_yield <= 0)) {
    stop('Historical ACCC 1998-2007 four-airport panel is incomplete.')
  }

  history_chart <- history |>
    dplyr::group_by(airport) |>
    dplyr::mutate(
      index_1998 = 100 * real_yield / real_yield[year == 1998L],
      real_aud_pp_2025 = real_yield * historical_cpi_2025,
      source = dplyr::if_else(airport == 'SYD' & year == 1998L,
        'FAC 1997-98 source (different adjustment scope)',
        'ACCC 2006-07 Figure 2.2'),
      regulatory_period = dplyr::if_else(year <= 2002L,
        'Price-cap period', 'Price monitoring after cap removal')
    ) |>
    dplyr::ungroup() |>
    dplyr::select(airport, australian_fy_end = year, nominal_aud_pp,
                  real_yield, real_aud_pp_2025, index_1998, source, regulatory_period)
  history_pooled_chart <- history_chart |>
    dplyr::group_by(australian_fy_end, regulatory_period) |>
    dplyr::summarise(index_1998 = mean(index_1998),
                     nominal_aud_pp = mean(nominal_aud_pp),
                     real_yield = mean(real_yield),
                     real_aud_pp_2025 = mean(real_aud_pp_2025),
                     .groups = 'drop') |>
    dplyr::mutate(airport = 'BIG4',
                  source = 'Unweighted means of four airport yields and separately normalized indexes') |>
    dplyr::select(dplyr::all_of(names(history_chart)))

  list(history = history,
       chart = dplyr::bind_rows(history_chart, history_pooled_chart))
}
