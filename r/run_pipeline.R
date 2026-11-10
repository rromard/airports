# Airport research pipeline | run from the airports project
# ----------------------------------------------------------
# Usual refresh (e.g. after correcting a Canadian source workbook):
#   source(here::here('r', 'run_pipeline.R'))
#
# The default reruns Canadian/Australian comparisons, the counterfactual,
# the sensitivity diagnostics, workbook formatting, and all 15 plots.
# It does NOT re-extract unchanged Australian source files.
# To rebuild those files too, set rebuild_australian_sources <- TRUE below.
#
# This script orchestrates existing R files. It changes neither their
# calculations nor the selected gradual_mode in the main counterfactual.

# CONFIGURATION -------------------------------------------------------------
rebuild_australian_sources <- FALSE  # TRUE only after changing source data/parser
run_sensitivity          <- TRUE
format_excel             <- TRUE
run_plots                <- TRUE

# PROJECT ROOT --------------------------------------------------------------
if (!requireNamespace('here', quietly = TRUE)) {
  stop("Missing R package 'here'. Restore project dependencies first.")
}
root <- here::here()
if (!file.exists(file.path(root, 'airports.Rproj')) ||
    !file.exists(file.path(root, 'r', 'per_unit_comparison.R'))) {
  stop('Run from the airports project root (the directory containing airports.Rproj).\n',
       'Currently detected root: ', root)
}

# Each component runs in a fresh environment to avoid collisions between
# variables such as `source`, `summary`, `years`, and `out_dir`.
run_stage <- function(label, script, expected_files = character()) {
  path <- file.path(root, 'r', script)
  if (!file.exists(path)) {
    stop('Missing pipeline script: ', path, call. = FALSE)
  }
  message('\n', strrep('=', 66L), '\n', label, '\n', strrep('=', 66L))
  started <- Sys.time()
  tryCatch(
    base::sys.source(path, envir = new.env(parent = globalenv()), chdir = FALSE),
    error = function(e) {
      stop('Pipeline stopped at [', label, '] (', script, '):\n',
           conditionMessage(e), call. = FALSE)
    }
  )
  for (relative_path in expected_files) {
    output <- file.path(root, relative_path)
    if (!file.exists(output)) {
      stop('Pipeline stopped at [', label, ']: expected output missing:\n',
           output, call. = FALSE)
    }
  }
  message('Completed ', label, ' in ',
          round(as.numeric(difftime(Sys.time(), started, units = 'secs')), 1),
          ' seconds.')
}

# 1. AUSTRALIAN INPUTS (OPTIONAL) -------------------------------------------
# These stages are stable and do not need re-running when only the Canadian
# Big 4 workbook is updated. The long-run parser creates the ACCC workbook
# and passenger CSV; the regulatory and legacy scripts create their outputs.
if (rebuild_australian_sources) {
  run_stage('Parse ACCC supplementary data', 'parse_accc_airports.R', c(
    'output/accc_airport_financial_analysis_2007_2025.xlsx',
    'data/australia/accc_airport_passengers_2007_2025.csv'
  ))
  run_stage('Extract ACCC regulatory accounts', 'accc_regulatory_ebitda.R', c(
    'output/accc_airport_ebitda_by_activity_2022_2025.csv',
    'output/accc_airport_expenditure_groups_2022_2025.csv'
  ))
  run_stage('Extract historical ACCC series', 'accc_legacy_extraction.R',
            'output/accc_legacy_vector_selected.csv')
} else {
  required_inputs <- c(
    'output/accc_airport_financial_analysis_2007_2025.xlsx',
    'data/australia/accc_airport_passengers_2007_2025.csv',
    'output/accc_airport_ebitda_by_activity_2022_2025.csv',
    'output/accc_airport_expenditure_groups_2022_2025.csv',
    'output/accc_legacy_vector_selected.csv'
  )
  missing <- required_inputs[!file.exists(file.path(root, required_inputs))]
  if (length(missing)) {
    stop('Missing prepared Australian input(s):\n  ',
         paste(missing, collapse = '\n  '),
         '\nSet rebuild_australian_sources <- TRUE if raw source files are present.',
         call. = FALSE)
  }
}

# 2. REBUILD RECENT CANADIAN LONG-FORM SOURCE ------------------------------
# Always generate a CSV from the authoritative 'Big 4' wide worksheet.
# The Excel workbook is never rewritten.
run_stage('Refresh Canadian Big 4 long-form source',
          'reshape_big4_long.R',
          'output/big4_long.csv')

# 3. REGENERATE COMPARABLE REVENUE AND FINANCIAL INPUTS ----------------------
# Uses the newly regenerated output/big4_long.csv.
run_stage('Compare Canadian and Australian airport finances',
          'per_unit_comparison.R', c(
            'output/airport_comparison/aero_revenue_growth_2015_2025.csv',
            'output/airport_comparison/comparison_annual_absolute_2023_2025.csv'
          ))

# 4. COUNTERFACTUAL (including selected GRADUAL interpolation mode) ---------
run_stage('Build three main counterfactual scenarios',
          'airport_revenue_counterfactual.R', c(
            'output/airport_counterfactual/scenario_growth_paths.csv',
            'output/airport_counterfactual/actual_growth_canada_australia.csv',
            'output/airport_counterfactual/counterfactual_annual.csv',
            'output/airport_counterfactual/counterfactual_summary.csv',
            'output/airport_counterfactual/airport_counterfactual.xlsx'
          ))

# 5. OPTIONAL FINAL OUTPUTS -------------------------------------------------
if (run_sensitivity) {
  run_stage('Calculate interpolation and timing sensitivities',
            'airport_counterfactual_sensitivity.R')
}
if (format_excel) {
  # Some local versions of the main model already call this formatter.
  # A second formatting pass is safe; it changes Excel cell styles only.
  run_stage('Format final counterfactual workbook',
            'format_airport_counterfactual_excel.R',
            'output/airport_counterfactual/airport_counterfactual.xlsx')
}
if (run_plots) {
  run_stage('Regenerate counterfactual charts',
            'airport_counterfactual_plots.R',
            'output/airport_counterfactual/plots/15_pooled_ebitda_margin_change.png')
}

message('\nPipeline completed successfully.')
message('Counterfactual files: ', file.path(root, 'output', 'airport_counterfactual'))
