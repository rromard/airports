# Formatting pass for output/airport_counterfactual/airport_counterfactual.xlsx
# ---------------------------------------------------------------------------
# Source this file immediately AFTER the existing writexl::write_xlsx() call
# in r/airport_revenue_counterfactual.R, or run it by itself after the model.
#
# Only cell DISPLAY formats are changed; underlying values keep full precision.
# Revenue and EBITDA columns are in 2025 CAD (usually millions). Percentage
# columns already store numbers such as 38.4, not Excel proportions (0.384).

if (!requireNamespace('openxlsx', quietly = TRUE)) {
  stop("The counterfactual Excel formatter requires the 'openxlsx' package. ",
       "It is listed in the project's renv.lock; run renv::restore() if needed.")
}

excel_path <- here::here('output', 'airport_counterfactual',
                         'airport_counterfactual.xlsx')
if (!file.exists(excel_path)) {
  stop('Counterfactual workbook not found. Run airport_revenue_counterfactual.R first: ',
       excel_path)
}

wb_format <- openxlsx::loadWorkbook(excel_path)
worksheet_names <- openxlsx::getSheetNames(excel_path)

fmt_header <- openxlsx::createStyle(
  textDecoration = 'bold', fgFill = '#ECEFF3',
  wrapText = TRUE, valign = 'center'
)
fmt_number <- openxlsx::createStyle(numFmt = '#,##0.0')
fmt_money <- openxlsx::createStyle(numFmt = '"$"#,##0.0;("$"#,##0.0)')
fmt_percent <- openxlsx::createStyle(numFmt = '0.0"%"')
fmt_pp <- openxlsx::createStyle(numFmt = '0.0" pp"')
fmt_year <- openxlsx::createStyle(numFmt = '0')
fmt_integer <- openxlsx::createStyle(numFmt = '#,##0')
fmt_notes <- openxlsx::createStyle(wrapText = TRUE, valign = 'top')

# Explicitly set year fields to '0' rather than leaving them unformatted.
# This clears any previous '#,##0' format when re-running the formatter on
# a workbook that has already been formatted (which otherwise shows 2,015).
is_year_column <- function(name) {
  nm <- tolower(name)
  grepl('(^|_)year($|_)|^fy$|^fyear$', nm) && nm != 'years'
}

# Use column names rather than fixed positions, so added or reordered columns
# automatically receive the appropriate number format.
column_style <- function(name) {
  nm <- tolower(name)
  if (is_year_column(nm)) return(fmt_year)
  if (nm %in% c('years', 'passengers', 'n_airports')) return(fmt_integer)
  # EBITDA margin change is expressed in percentage POINTS, not percent growth.
  if (grepl('margin_change_pp', nm, fixed = TRUE)) return(fmt_pp)
  # These are already on a 0-100 scale: use a literal % to avoid multiplying by 100.
  if (grepl('pct', nm, fixed = TRUE) || grepl('(%)', nm, fixed = TRUE)) {
    return(fmt_percent)
  }
  # Both descriptive spreadsheet headers ($, $m) and machine-readable columns.
  if (grepl('$', nm, fixed = TRUE) ||
      grepl('cad_2025|revenue|ebitda|^pp_', nm)) {
    return(fmt_money)
  }
  fmt_number
}

for (sheet_name in worksheet_names) {
  # Read only for header names and column types; never rewrite the values.
  sheet_data <- openxlsx::read.xlsx(
    excel_path, sheet = sheet_name, colNames = TRUE,
    skipEmptyRows = FALSE, check.names = FALSE
  )
  nc <- ncol(sheet_data)
  nr <- nrow(sheet_data)
  if (nc == 0L) next

  openxlsx::addStyle(wb_format, sheet = sheet_name, style = fmt_header,
                     rows = 1L, cols = seq_len(nc), gridExpand = TRUE,
                     stack = TRUE)
  openxlsx::setRowHeights(wb_format, sheet = sheet_name,
                          rows = 1L, heights = 39)

  for (j in seq_len(nc)) {
    nm <- tolower(names(sheet_data)[j])
    is_numeric <- is.numeric(sheet_data[[j]])

    if (is_numeric && nr > 0L) {
      openxlsx::addStyle(
        wb_format, sheet = sheet_name,
        style = column_style(names(sheet_data)[j]),
        rows = seq.int(2L, nr + 1L), cols = j,
        gridExpand = TRUE, stack = FALSE
      )
    }

    width <- if (sheet_name == 'Notes') {
      95
    } else if (is_year_column(nm) || nm == 'years') {
      11
    } else if (nm == 'passengers') {
      17
    } else if (grepl('scenario|label|source_status|^type$', nm)) {
      29
    } else if (nm %in% c('airport', 'country')) {
      17
    } else if (is_numeric) {
      22
    } else {
      25
    }
    openxlsx::setColWidths(wb_format, sheet = sheet_name,
                           cols = j, widths = width)
  }

  if (sheet_name == 'Notes' && nr > 0L) {
    openxlsx::addStyle(wb_format, sheet = sheet_name, style = fmt_notes,
                       rows = seq.int(2L, nr + 1L), cols = 1L,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(wb_format, sheet = sheet_name,
                            rows = seq.int(2L, nr + 1L), heights = 48)
  } else {
    openxlsx::freezePane(wb_format, sheet = sheet_name, firstRow = TRUE)
  }
}

openxlsx::saveWorkbook(wb_format, excel_path, overwrite = TRUE)
message('Formatted workbook (one-decimal display; calendar years ungrouped; full values retained): ',
        excel_path)
