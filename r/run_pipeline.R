# Airport research pipeline | refreshed control script (2026-11-18)
# Run from the airports project:
#   source(here::here('r', 'run_pipeline.R'))
#
# Default: prepare financial comparisons, run the acquisition counterfactual,
# then build both documented and integrated production workbooks.
# Earlier revenue-growth counterfactual / sensitivity / plotting stages are
# retained as an OPTIONAL, separate branch (disabled by default).
#
# This orchestrator does not change the formulas in any component script.
# The workbook builder consumes finished acquisition CSVs; generated XLSX files
# are never read back into any modeling stage.

# ---- CONFIGURATION --------------------------------------------------------
rebuild_australian_sources <- FALSE
refresh_canadian_sources  <- TRUE
run_financial_comparison <- TRUE
run_acquisition_model    <- TRUE

# Older revenue-growth counterfactual branch, unrelated to private acquisition.
run_legacy_counterfactual <- FALSE
run_legacy_sensitivity    <- TRUE
format_legacy_excel      <- TRUE
run_legacy_plots         <- TRUE

# ---- PROJECT ROOT ---------------------------------------------------------
if (!requireNamespace('here', quietly = TRUE)) {
  stop("Missing R package 'here'.", call. = FALSE)
}
root <- here::here()
if (!file.exists(file.path(root, 'airports.Rproj')) ||
    !file.exists(file.path(root, 'r', 'per_unit_comparison.R'))) {
  stop('Run from the airports project root (containing airports.Rproj).\n',
       'Detected root: ', root, call. = FALSE)
}

if (run_acquisition_model && !run_financial_comparison) {
  stop('The acquisition model requires updated upstream comparison data.',
       call. = FALSE)
}

# Run each stage in its own environment. Pass objects in explicitly when a
# downstream stage needs them, without leaking or overwriting another stage's
# variables (notably `wb`, `years`, `source` and `out_dir`).
run_stage <- function(label, script, expected_files = character(),
                      bindings = list(), return_object = NULL) {
  path <- file.path(root, 'r', script)
  if (!file.exists(path)) {
    stop('Missing pipeline script: ', path, call. = FALSE)
  }
  message('\n', strrep('=', 66L), '\n', label, '\n', strrep('=', 66L))
  started <- Sys.time()
  stage_env <- new.env(parent = globalenv())
  if (length(bindings)) list2env(bindings, envir = stage_env)
  tryCatch(
    base::sys.source(path, envir = stage_env, chdir = FALSE),
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
  if (is.null(return_object)) return(invisible(NULL))
  if (!exists(return_object, envir = stage_env, inherits = FALSE)) {
    stop('Stage [', label, '] did not create `', return_object, '`.',
         call. = FALSE)
  }
  get(return_object, envir = stage_env, inherits = FALSE)
}

# ---- 1. AUSTRALIAN SOURCE EXTRACTION (OPTIONAL) ---------------------------
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
         '\nSet rebuild_australian_sources = TRUE if raw files are present.',
         call. = FALSE)
  }
}

# ---- 2. CANADIAN SOURCE REFRESH ------------------------------------------
if (refresh_canadian_sources) {
  run_stage('Refresh Canadian Big 4 long-form source',
            'reshape_big4_long.R', 'output/big4_long.csv')
} else if (!file.exists(file.path(root, 'output', 'big4_long.csv'))) {
  stop('Missing output/big4_long.csv; enable refresh_canadian_sources.',
       call. = FALSE)
}

# ---- 3. PER-UNIT AND FINANCIAL COMPARISON ---------------------------------
if (run_financial_comparison) {
  run_stage('Compare Canadian and Australian airport finances',
            'per_unit_comparison.R', c(
              'output/airport_comparison/aero_revenue_growth_2015_2025.csv',
              'output/airport_comparison/comparison_annual_absolute_2023_2025.csv',
              'output/airport_comparison/comparison_annual_pp_2023_2025.csv',
              'output/airport_comparison/comparison_pooled_pp_2023_2025.csv'
            ))

}
# ---- 4. PRIVATE-ACQUISITION COUNTERFACTUAL -------------------------------
if (run_acquisition_model) {
  run_stage('Run three-case airport acquisition counterfactual',
            'airport_acquisition_counterfactual.R', c(
              'output/airport_purchase_counterfactual/airport_acquisition_counterfactual_audit.xlsx',
              'output/airport_purchase_counterfactual/acquisition_per_passenger_annual.csv',
              'output/airport_purchase_counterfactual/acquisition_per_passenger_pooled.csv',
              'output/airport_purchase_counterfactual/scenario_assumptions.csv',
              'output/airport_purchase_counterfactual/acquisition_workbook_sources.rds',
              'output/airport_purchase_counterfactual/acquisition_presentation_amounts.csv',
              'output/airport_purchase_counterfactual/acquisition_presentation_per_passenger.csv'
            ))
}

# ---- 5. FINAL WORKBOOKS ---------------------------------------------------
if (run_financial_comparison) {
  run_stage('Build documented and acquisition comparison workbooks',
    'build_financial_comparison_workbook.R',
    c('output/airport_comparison/airport_financial_comparison_documented_2023_2025.xlsx',
      if (run_acquisition_model)
        'output/airport_comparison/airport_financial_comparison_with_acquisition_2023_2025.xlsx'),
    bindings = list(include_acquisition = run_acquisition_model))
}

# ---- 5. ORIGINAL REVENUE-GROWTH COUNTERFACTUAL (OPTIONAL) -----------------
# These scripts are NOT the acquisition-financing analysis. They are retained
# so earlier growth-path figures can be regenerated when needed.
if (run_legacy_counterfactual) {
  run_stage('Build historical revenue-growth counterfactual',
            'airport_revenue_counterfactual.R', c(
              'output/airport_counterfactual/scenario_growth_paths.csv',
              'output/airport_counterfactual/counterfactual_annual.csv',
              'output/airport_counterfactual/airport_counterfactual.xlsx'
            ))
  if (run_legacy_sensitivity) {
    run_stage('Calculate revenue-growth interpolation sensitivities',
              'airport_counterfactual_sensitivity.R')
  }
  if (format_legacy_excel) {
    run_stage('Format revenue-growth counterfactual workbook',
              'format_airport_counterfactual_excel.R',
              'output/airport_counterfactual/airport_counterfactual.xlsx')
  }
  if (run_legacy_plots) {
    run_stage('Regenerate revenue-growth counterfactual charts',
              'airport_counterfactual_plots.R',
              'output/airport_counterfactual/plots/15_pooled_ebitda_margin_change.png')
  }
}

message('\nPipeline completed successfully.')
message('Financial comparison: ', file.path(root, 'output', 'airport_comparison'))
if (run_acquisition_model) {
  message('Acquisition results: ',
          file.path(root, 'output', 'airport_purchase_counterfactual'))
}
