# Rebuild 'Big 4 long' from the authoritative 'Big 4' worksheet.
# Run before per_unit_comparison.R, or independently with:
# source(here::here('r', 'reshape_big4_long.R'))
#
# Expected wide layout: an airport header above the 'Item' / year header;
# four airports (Pearson, Calgary, Montreal, Vancouver), three years each.
# Figures are nominal CAD millions. No netting or other financial adjustment.
# The source workbook is read-only; derived data are saved as a CSV in output/.

if (!requireNamespace('readxl', quietly = TRUE) ||
    !requireNamespace('dplyr', quietly = TRUE) ||
    !requireNamespace('stringr', quietly = TRUE) ||
    !requireNamespace('purrr', quietly = TRUE) ||
    !requireNamespace('tidyr', quietly = TRUE) ||
    !requireNamespace('here', quietly = TRUE) ||
    !requireNamespace('readr', quietly = TRUE)) {
  stop('Required packages: readxl, dplyr, stringr, purrr, tidyr, here.')
}

xlsx_path <- here::here('output', 'Airport privatization CCPA sheet.xlsx')
if (!file.exists(xlsx_path)) stop('Source workbook not found: ', xlsx_path)
if (!'Big 4' %in% readxl::excel_sheets(xlsx_path)) {
  stop("Expected 'Big 4' worksheet in: ", xlsx_path)
}

# An explicit range preserves any blank rows before the header. We inspect
# only A:M; if the wide table grows past 120 rows, increase this range.
wide <- readxl::read_excel(
  xlsx_path, sheet = 'Big 4 standard', range = 'A1:M120',
  col_names = FALSE, .name_repair = 'minimal',
  col_types = 'text'
)
wide <- as.data.frame(wide, stringsAsFactors = FALSE)
normalize <- function(x) stringr::str_squish(as.character(x))
items_col <- normalize(wide[[1]])
header_rows <- which(items_col == 'Item')
if (length(header_rows) != 1L) {
  stop("Expected exactly one 'Item' heading in column A of 'Big 4'.")
}
header_row <- header_rows[[1]]

names_expected <- c('Pearson', 'Calgary', 'Montreal', 'Vancouver')
# Match each airport against its three-column group in any row ABOVE 'Item'.
# This works with ordinary Excel merged cells, where only the leftmost cell
# contains the airport name.
header_candidates <- which(vapply(seq_len(header_row - 1L), function(i) {
  all(vapply(seq_along(names_expected), function(k) {
    cols <- (2L + 3L * (k - 1L)):(4L + 3L * (k - 1L))
    any(normalize(unlist(wide[i, cols], use.names = FALSE)) == names_expected[k],
        na.rm = TRUE)
  }, logical(1)))
}, logical(1)))
if (!length(header_candidates)) {
  stop('Could not find airport group headings above Item, in B:D, E:G, H:J, K:M.')
}

parse_numeric <- function(x) {
  x <- normalize(x)
  x[is.na(x) | x %in% c('', '-', '–', 'NA', 'N/A')] <- NA_character_
  # Treat parenthesized financial figures as negatives; retain ordinary '-' signs.
  x <- ifelse(grepl('^\\(.*\\)$', x),
              paste0('-', sub('^\\((.*)\\)$', '\\1', x)), x)
  x <- gsub('[,$ ]', '', x)
  suppressWarnings(as.numeric(x))
}

# Read the actual year headers rather than assuming their position or order.
yrs <- parse_numeric(unlist(wide[header_row, 2:13], use.names = FALSE))
if (anyNA(yrs) || !identical(as.integer(yrs), rep(2025:2023, 4L))) {
  stop('Unexpected year headers. Expected 2025, 2024, 2023 for each airport.')
}

first_row <- header_row + 1L
end_hits <- which(items_col == 'Net Income' & seq_along(items_col) >= first_row)
if (length(end_hits) != 1L) stop("Expected exactly one 'Net Income' row after Item.")
last_row <- end_hits[[1]]
source_rows <- first_row:last_row
items <- items_col[source_rows]
if (anyNA(items) || any(!nzchar(items)) || anyDuplicated(items)) {
  stop('Missing or duplicate item labels in the source table; inspect Big 4.')
}

revenue_end <- match('Total Revenues', items)
expense_end <- match('Total Expenses', items)
if (is.na(revenue_end) || is.na(expense_end) || revenue_end >= expense_end) {
  stop('Cannot locate revenue/expense sections in the wide table.')
}
category <- ifelse(seq_along(items) <= revenue_end, 'revenue',
                   ifelse(seq_along(items) <= expense_end, 'expense', 'result'))

long <- purrr::map_dfr(seq_len(12L), function(j) {
  values_raw <- wide[[j + 1L]][source_rows]
  values <- parse_numeric(values_raw)
  blanks <- is.na(values) & !is.na(values_raw) &
    nzchar(normalize(values_raw)) & !normalize(values_raw) %in% c('-', '–', 'NA', 'N/A')
  if (any(blanks)) {
    stop('Non-numeric value(s) in wide table: ', names_expected[ceiling(j / 3)],
         ' ', yrs[j], ' / ', paste(items[blanks], collapse = '; '))
  }
  data.frame(
    airport = names_expected[ceiling(j / 3)], year = as.integer(yrs[j]),
    item = items, category = category, value = values,
    stringsAsFactors = FALSE
  )
}) |>
  dplyr::arrange(match(airport, names_expected), dplyr::desc(year),
                 match(item, items))

# The revenue equation must hold on a gross-AIF basis, within the published
# precision of one decimal place. This catches accidental net-fee inputs.
rev <- long |>
  dplyr::filter(item %in% c('Aeronautical Revenues',
                            'Commercial (non-aeronautical) Revenues',
                            'Airport Improvement Fees', 'Total Revenues')) |>
  tidyr::pivot_wider(names_from = item, values_from = value)
if (nrow(rev) != 12L || anyNA(rev) || any(abs(
    rev[['Aeronautical Revenues']] +
      rev[['Commercial (non-aeronautical) Revenues']] +
      rev[['Airport Improvement Fees']] - rev[['Total Revenues']]
  ) > 0.25)) {
  stop('Wide source does not reconcile: aero + commercial + gross AIF != total revenue.')
}

# Write a standalone derived dataset; never rewrite the source workbook.
# This is safe to rerun and avoids Excel comment/XML corruption.
out_path <- here::here('output', 'big4_long.csv')
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(long, out_path)
message(sprintf("Rebuilt Big 4 long-form data: %d rows (%d items x 12 airport-years).",
                nrow(long), length(items)))
message('Saved: ', out_path)
