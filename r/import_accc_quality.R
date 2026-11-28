# Published ACCC quality charts: independent of the supplementary-data parser.
# Run from project root: source(here::here("r", "import_accc_quality.R"))
# Optional flags in the sourcing environment: refresh_published_quality,
# quality_cache_only. ACCC_QUALITY_PYTHON may specify the Python executable.

read_accc_published_quality <- function(path) {
  q <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(
    airport = readr::col_character(), year = readr::col_integer(),
    fyear = readr::col_character(), metric = readr::col_character(),
    rating = readr::col_double(), rating_vector = readr::col_double(),
    rating_printed = readr::col_double(), validation_error = readr::col_double(),
    value_method = readr::col_character(), report = readr::col_character(),
    figure = readr::col_character(), pdf_page = readr::col_integer(),
    printed_page = readr::col_integer(), .default = readr::col_double()
  ))
  readr::stop_for_problems(q)
  required <- c("airport", "year", "fyear", "metric", "rating",
    "rating_vector", "rating_printed", "validation_error", "value_method",
    "report", "figure", "pdf_page", "printed_page", "x_pdf_pt", "y_pdf_pt",
    "axis_top_pdf_pt", "axis_bottom_pdf_pt")
  if (!all(required %in% names(q)))
    stop("Incomplete published ACCC quality schema.", call. = FALSE)

  airports <- c("BNE", "MEL", "PER", "SYD")
  metrics <- c("overall_quality", "passenger_quality", "airline_quality")
  keys <- c("airport", "year", "metric")
  expected <- dplyr::bind_rows(
    tidyr::expand_grid(airport = airports, year = 1998:2019,
      metric = "aeronautical_quality") |>
      dplyr::filter(!(airport == "SYD" & year == 1998L)),
    tidyr::expand_grid(airport = airports,
      year = c(2016:2019, 2023:2025), metric = metrics)
  )
  if (nrow(q) != 171L || anyNA(q[keys]) || anyDuplicated(q[keys]) ||
      nrow(dplyr::anti_join(expected, q, by = keys)) ||
      any(!is.finite(q$rating)) || any(!is.finite(q$rating_vector)) ||
      any(!dplyr::between(q$rating, 1, 5)) ||
      any(!dplyr::between(q$rating_vector, 1, 5)) ||
      anyNA(q$fyear) ||
      any(q$fyear != sprintf("%d-%02d", q$year - 1L, q$year %% 100L)))
    stop("Published ACCC quality coverage, keys or rating bounds invalid.",
      call. = FALSE)

  historical <- q$metric == "aeronautical_quality"
  labels <- q$year == 2025L
  expected_figure <- c(aeronautical_quality = "4.2", overall_quality = "3.5",
    passenger_quality = "3.6", airline_quality = "3.7")
  expected_pdf_page <- c(aeronautical_quality = 67L, overall_quality = 34L,
    passenger_quality = 36L, airline_quality = 38L)
  expected_printed_page <- c(aeronautical_quality = 58L, overall_quality = 23L,
    passenger_quality = 25L, airline_quality = 27L)
  if (anyNA(q[c("report", "figure", "pdf_page", "printed_page", "value_method")]) ||
      any(q$report != ifelse(historical, "accc_monitor_2021.pdf",
        "accc_monitor_2025.pdf")) ||
      any(q$figure != unname(expected_figure[q$metric])) ||
      any(q$pdf_page != unname(expected_pdf_page[q$metric])) ||
      any(q$printed_page != unname(expected_printed_page[q$metric])) ||
      sum(labels) != 12L ||
      any(q$value_method != ifelse(labels, "published_label", "pdf_vector")) ||
      any(!is.finite(q$rating_printed[labels])) ||
      any(!is.finite(q$validation_error[labels])) ||
      any(abs(q$validation_error[labels]) > 0.025) ||
      any(abs(q$rating_vector[labels] - q$rating_printed[labels] -
        q$validation_error[labels]) > 0.000002) ||
      any(q$rating[labels] != q$rating_printed[labels]) ||
      any(!is.na(q$rating_printed[!labels])) ||
      any(!is.na(q$validation_error[!labels])) ||
      any(abs(q$rating[!labels] - q$rating_vector[!labels]) > 0.005001))
    stop("Published ACCC quality provenance or label validation failed.",
      call. = FALSE)

  dplyr::arrange(q, metric, airport, year)
}

# Chart-ready wide format; empty cells are explicit missing observations,
# not zero or interpolations. Never combine the distinct composite measures.
accc_published_quality_wide <- function(q, measure) {
  airports <- c("BNE", "MEL", "PER", "SYD")
  if (length(measure) != 1L || !measure %in% unique(q$metric))
    stop("Unknown published ACCC quality measure.", call. = FALSE)
  series <- q[q$metric == measure, c("airport", "year", "rating")]
  tidyr::expand_grid(year = seq.int(min(series$year), max(series$year)),
    airport = airports) |>
    dplyr::left_join(series, by = c("airport", "year")) |>
    tidyr::pivot_wider(names_from = airport, values_from = rating) |>
    dplyr::mutate(financial_year = sprintf("%d-%02d", year - 1L, year %% 100L)) |>
    dplyr::select(year, financial_year, dplyr::all_of(airports)) |>
    dplyr::arrange(year)
}

load_accc_published_quality <- function(root = here::here(), refresh = FALSE,
                                        cache_only = FALSE) {
  input_dir <- file.path(root, "preserved_data")
  output_dir <- file.path(root, "output")
  cache <- file.path(output_dir, "accc_quality_vector_ratings.csv")
  pdfs <- file.path(input_dir, c("accc_monitor_2021.pdf", "accc_monitor_2025.pdf"))
  if (!refresh && file.exists(cache))
    return(read_accc_published_quality(cache))

  if (cache_only) {
    if (refresh) stop("Cannot refresh quality in cache-only mode.", call. = FALSE)
    return(NULL)
  }
  if (!all(file.exists(pdfs))) {
    msg <- paste("Published ACCC quality source PDFs missing:",
      paste(pdfs[!file.exists(pdfs)], collapse = ", "))
    if (refresh) stop(msg, call. = FALSE)
    warning(msg, "; skipping optional quality extraction.", call. = FALSE)
    return(NULL)
  }

  executable <- Sys.getenv("ACCC_QUALITY_PYTHON", unset = "python")
  python <- Sys.which(executable)
  if (!nzchar(python) && executable == "python") python <- Sys.which("python3")
  if (!nzchar(python))
    stop("Python not found; set ACCC_QUALITY_PYTHON or install Python and PyMuPDF.",
      call. = FALSE)
  script <- file.path(root, "r", "extract_accc_quality_vectors.py")
  # Validate in a temporary directory before replacing the existing cache.
  staging <- tempfile("accc-quality-")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  status <- system2(python, c(shQuote(script), "--input-dir", shQuote(input_dir),
    "--output-dir", shQuote(staging)))
  if (status != 0L) stop("ACCC quality vector extraction failed.", call. = FALSE)
  staged <- file.path(staging, basename(cache))
  result <- read_accc_published_quality(staged)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(staged, cache, overwrite = TRUE))
    stop("Could not write published ACCC quality cache.", call. = FALSE)
  result
}

# Keep control flags local to the stage; defaults preserve standalone use.
quality_accc <- load_accc_published_quality(
  refresh = isTRUE(get0("refresh_published_quality", inherits = FALSE)),
  cache_only = isTRUE(get0("quality_cache_only", inherits = FALSE))
)
