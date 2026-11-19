# =============================================================================
# CANADIAN AIRPORT PRIVATIZATION: ACQUISITION AND PROFITABILITY COUNTERFACTUAL
# =============================================================================
#
# PURPOSE
#
# Estimate the financial implications of a hypothetical private acquisition
# of Canada's four largest airports: Calgary (YYC), Montreal (YUL),
# Vancouver (YVR), and Toronto Pearson (YYZ).
#
# The model examines two related questions:
#
# 1. How much would a private purchaser need to borrow, and what additional
#    financing costs would arise from acquiring each airport?
#
# 2. How much additional revenue would the airport need to generate to
#    cover those costs, restore its historical profitability, or achieve
#    net profit margins comparable to those observed at private airports?
#
# The analysis is intended to illustrate the financial pressures that
# privatization could introduce, particularly the potential tension
# between acquisition financing, private profitability, and continued
# investment in airport infrastructure.
#
#
# METHODOLOGY
#
# The model uses observed financial results for 2023, 2024, and 2025.
# Each airport-year is treated as an independent acquisition scenario.
# Three-year pooled results are also calculated.
#
# Three alternative acquisition packages are examined:
#   - Low-cost:  20x EBITDA valuation, 50% new debt, 4.5% interest.
#   - Central:   22.5x EBITDA valuation, 60% new debt, 5.5% interest.
#   - High-cost: 25x EBITDA valuation, 70% new debt, 6.5% interest.
#
# These are illustrative assumptions, not estimated transaction terms.
#
# The acquisition model:
#   - Estimates the airport's sale value using EBITDA adjusted for the
#     elimination of federal ground rent.
#   - Accounts for existing airport debt and cash when estimating the
#     amount payable to acquire the airport.
#   - Estimates new acquisition borrowing and associated interest costs.
#   - Retains existing airport debt and its observed financing costs.
#   - Calculates post-acquisition profitability, including estimated
#     corporate income taxes.
#
# The profitability model then estimates the additional revenue needed to:
#   - Break even on a pre-tax basis.
#   - Restore historical pre-tax profitability.
#   - Achieve total after-tax net profit margins of 15%, 20%, 25%, or 30%.
#
# These are six independent objectives, not cumulative requirements.
# Additional revenue is assumed to contribute to pre-tax profit at a
# specified rate (100% by default). The model does not determine which
# airport charges or commercial activities would generate that revenue.
#
# Results are calculated separately for each airport. BIG4 aggregates
# combine the individual calculations, including airport-specific taxes
# and revenue requirements, rather than modelling a single purchaser
# or taxpayer for all four airports.
#
#
# INPUTS
#
# 1. output/airport_comparison/
#      financial_comparison_audit_2023_2025.rds
#    Observed Canadian airport revenue, EBITDA, net income, financing
#    costs, federal ground rent, passenger volumes, and CPI factors.
#
# 2. data/canada/
#      airport_acquisition_inputs_2023_2025.xlsx
#    Supplemental gross debt, unrestricted cash, and capital expenditure
#    data for the four airports, 2023–2025.
#
#
# OUTPUTS
#
# All files are saved under:
#   output/airport_purchase_counterfactual/
#
# Main workbook:
#   airport_acquisition_counterfactual_audit.xlsx
#     Acquisition and profitability results, financing comparisons,
#     calculation audits, methodology notes, and sensitivity analysis.
#
# Complete model datasets:
#   acquisition_scenarios.csv
#   profitability_scenarios.csv
#
# Central-scenario presentation tables:
#   central_acquisition_financing.csv
#   central_profitability_requirements.csv
#
# Financing assumptions and scenario comparisons:
#   scenario_assumptions.csv
#   scenario_comparison.csv
#
# Per-passenger comparison datasets:
#   acquisition_per_passenger_annual.csv
#   acquisition_per_passenger_pooled.csv
#
# Additional presentation tables:
#   acquisition_presentation_amounts.csv
#   acquisition_presentation_per_passenger.csv
#
# Figures:
#   chart_net_financing_pressure_2025.png
#   chart_revenue_restore_2025.png
#
#
# INTERPRETATION AND LIMITATIONS
#
# This is a static financial counterfactual, not a forecast of airport
# privatization outcomes, a proposed acquisition price, or an assessment
# of whether the assumed financing arrangements would be available.
#
# The model assumes that federal ground rent is eliminated following
# privatization, but does not assume improvements in operating efficiency.
#
# Additional acquisition debt creates new financing costs. Existing
# airport debt and its financing costs are retained. The model does not
# include acquisition fees, principal repayments, refinancing, or changes
# in depreciation and amortization.
#
# Corporate income taxes are estimated using statutory rates applied to
# positive accounting pre-tax income. Actual taxable income and tax
# liabilities may differ.
#
# Revenue requirements are not predictions of airport fees, passenger
# charges, traffic growth, or commercial revenue. They do not account
# for demand responses or changes in operating costs unless explicitly
# introduced through the revenue-to-profit contribution assumption.
#
# Capital expenditures are included in descriptive financing-pressure
# measures but are not deducted as expenses in the profitability model.
# The analysis therefore does not constitute a complete cash-flow model
# or establish the funds available for future infrastructure investment.
#
# Annual financial observations are nominal CAD. Pooled financial flows
# are adjusted to 2025 CAD, with acquisition valuations based on
# annualized earnings. Per-passenger comparison outputs use 2025 CAD.
#
#
# EXECUTION
#
# Run from the project root:
#   source(here::here('r', 'airport_acquisition_counterfactual.R'))
#
# Required packages: tidyverse, readxl, here, openxlsx.
#
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(here)
  library(openxlsx)
})

# ---- 1. EDIT ASSUMPTIONS HERE ---------------------------------------------
years <- 2023:2025
can_airports <- c('YYC', 'YUL', 'YVR', 'YYZ')

# Project paths (confirmed against existing financial comparison pipeline).
input_path <- here('output', 'airport_comparison',
                   'financial_comparison_audit_2023_2025.rds')
supplement_path <- here('data', 'canada',
                        'airport_acquisition_inputs_2023_2025.xlsx')
# Versioned directory keeps previous outputs separate from v21 results.
out_dir <- here('output', 'airport_purchase_counterfactual')

# Three paired packages are calculated for every observed airport-year.
# These are analyst-defined valuation and financing scenarios, NOT estimates.
# Three COMPLETE, paired acquisition packages (not a 3 x 3 x 3 grid).
# These are ordered by their incremental annual acquisition-interest burden.
# The source/evidence workbook separately documents why the ranges were chosen.
scenario_assumptions <- tibble::tribble(
  ~scenario_name, ~purchase_multiple, ~acquisition_debt_share, ~acquisition_interest_rate,
  'Low-cost',                   20.0,                    0.50,                      0.045,
  'Central',                    22.5,                    0.60,                      0.055,
  'High-cost',                  25.0,                    0.70,                      0.065
)
# v21's operating sensitivity is preserved as an OPTIONAL analyst appendix.
# The default v22 workbook presents only the three acquisition scenarios.
include_operating_sensitivities <- TRUE

# Private-profit targets are TOTAL after-tax NET margins, benchmarked against
# observed Australian airport margins (not additions to observed Canadian profit).
# Importantly, each target applies to counterfactual revenue AFTER its increase.
# These six independent profit thresholds are calculated within EACH of the three financing cases.
private_net_margin_targets <- c(0.15, 0.20, 0.25, 0.30)

# Illustrative combined federal/provincial general corporate tax rates.
# Statutory rates approximate tax on positive ACCOUNTING pre-tax earnings;
# no taxable-income adjustments, loss carryforwards, EIFEL limitations, etc.
# BIG4 is NEVER treated as a single taxpayer; airport-level estimates are summed.
statutory_tax_rates <- c(YYC = 0.23, YUL = 0.265, YVR = 0.27, YYZ = 0.265)

# Extra revenue -> additional PRE-TAX profit, holding other costs constant.
# A value of 1.0 is a lower-bound estimate of needed revenue, assuming every
# new revenue dollar translates into a dollar of pre-tax earnings. It is not
# a forecast of prices/fees or a cash conversion rate.
incremental_revenue_to_profit <- 1.00

# Acquisition debt shares refer to NEW borrowing as a fraction of the
# amount paid to acquire the airport (equity purchase consideration):
#   equity purchase consideration = estimated sale value - existing gross debt + cash.
# Existing airport debt is RETAINED separately, not credited toward this share.
# Thus 60% means new acquisition debt = 60% of purchase consideration.
# Consolidated post-sale gross debt / sale value will usually be higher than 60%.
# The buyer is assumed to retain the operator's unrestricted cash.

# ---- 2. LOAD OBSERVED 2023–2025 CANADIAN FINANCIAL INPUTS ----------------
assert_unique <- function(d, keys, label) {
  bad <- d |> count(across(all_of(keys))) |> filter(n != 1L)
  if (nrow(bad)) stop('Duplicate keys in ', label, call. = FALSE)
  invisible(d)
}

if (!file.exists(input_path)) {
  stop('Missing financial comparison audit RDS: ', input_path, call. = FALSE)
}
comparison_audit <- readRDS(input_path)
if (!is.list(comparison_audit) ||
    !all(c('financial_source_annual', 'cost_groups_annual') %in%
         names(comparison_audit))) {
  stop('Financial comparison audit RDS is incomplete.', call. = FALSE)
}
# The upstream comparison audit is the authoritative Canadian source.
# Extract nominal financial flows, passengers and CPI before scenario modeling.
financial_core <- comparison_audit$financial_source_annual |>
  filter(country == 'Canada', airport %in% can_airports, year %in% years) |>
  transmute(airport = as.character(airport), year = as.integer(year),
            revenue_m = as.numeric(ebitda_ebit_revenue_basis),
            ebitda_m = as.numeric(ebitda),
            net_income_m = as.numeric(net_income),
            net_financing_cost_m = as.numeric(net_financing_costs),
            passengers = as.numeric(passengers),
            cpi_factor = as.numeric(adjustment_to_2025))
assert_unique(financial_core, c('airport', 'year'), 'financial comparison audit')
ground_rent <- comparison_audit$cost_groups_annual |>
  filter(country == 'Canada', airport %in% can_airports,
         year %in% years, variable == 'federal_ground_rent') |>
  transmute(airport = as.character(airport), year = as.integer(year),
            ground_rent_m = as.numeric(value_m))
assert_unique(ground_rent, c('airport', 'year'), 'federal ground rent')
observed <- financial_core |>
  select(-cpi_factor) |>
  left_join(ground_rent, by = c('airport', 'year'))
if (nrow(financial_core) != 12L || nrow(ground_rent) != 12L ||
    nrow(observed) != 12L || anyNA(financial_core) || anyNA(observed) ||
    any(observed$ebitda_m <= 0) || any(observed$revenue_m <= 0) ||
    any(observed$passengers <= 0) || any(observed$ground_rent_m < 0) ||
    any(financial_core$cpi_factor <= 0)) {
  stop('Incomplete or invalid Canadian financial, passenger, rent or CPI input.',
       call. = FALSE)
}

# Supplement uses the SAME units, years and airport codes as observed.
# Gross outstanding debt is face/carrying value on a CONSISTENT basis;
# cash is unrestricted cash used for the transaction valuation convention.
# Annual cash capex is optional for the acquisition/profit thresholds;
# supply it for the separate, descriptive financing-minus-capex proxy.
required_supplement_cols <- c('airport', 'year', 'gross_debt_m', 'cash_m',
                             'capex_cash_m')
if (is.na(supplement_path) || !file.exists(supplement_path)) {
  stop('Supplement missing: ', supplement_path, call. = FALSE)
}
supplement <- readxl::read_excel(supplement_path, skip = 3) |>
  janitor::clean_names() |>
  select(airport, year, gross_debt_m, cash_m, capex_cash_m) |>
  transmute(airport = as.character(airport), year = as.integer(year),
            gross_debt_m = as.numeric(gross_debt_m),
            cash_m = as.numeric(cash_m),
            capex_cash_m = as.numeric(capex_cash_m)) |>
  filter(airport %in% can_airports, year %in% years)
assert_unique(supplement, c('airport', 'year'), 'supplement')
if (nrow(supplement) != 12L ||
    anyNA(supplement[c('gross_debt_m', 'cash_m')]) ||
    any(supplement$gross_debt_m < 0) || any(supplement$cash_m < 0) ||
    any(!is.na(supplement$capex_cash_m) & supplement$capex_cash_m < 0)) {
  stop('Supplement requires 12 complete airport-years, nonnegative gross debt',
       ' and cash. Capex may be NA; then cash-surplus outputs are NA.',
       call. = FALSE)
}

baseline <- observed |>
  left_join(supplement, by = c('airport', 'year')) |>
  mutate(existing_net_debt_m = gross_debt_m - cash_m,
         existing_net_financing_coverage =
           if_else(net_financing_cost_m > 0,
                   ebitda_m / net_financing_cost_m, NA_real_))

if (!is.numeric(incremental_revenue_to_profit) ||
    length(incremental_revenue_to_profit) != 1L ||
    !is.finite(incremental_revenue_to_profit) ||
    incremental_revenue_to_profit <= 0 || incremental_revenue_to_profit > 1) {
  stop('incremental_revenue_to_profit must be in (0,1].')
}

# ---- 3. ALL SCENARIOS REUSE A SINGLE, TRANSPARENT ACQUISITION FORMULA ---
# Rent-adjusted EBITDA = reported EBITDA + federal ground rent saved.
# Estimated total sale value = annualized rent-adjusted EBITDA x multiple.
# Equity purchase consideration = estimated sale value - existing gross debt + cash.
# New debt finances a share of equity purchase consideration; existing debt remains.
# No forecast or refinancing is assumed; pooled flows cover 3 years,
# while debt, cash and the transaction valuation are annual-equivalent.

# ---- 4. ANNUAL + POOLED BASELINES, THEN BIG4 SUMMARIES -------------------
# CPI factors come from the financial comparison audit RDS. Pooled FLOW
# variables are three-year SUMS in 2025 CAD, including passengers (unadjusted
# for inflation). Debt and cash are STOCKS: use their three-year average.
# Sale value uses mean annual RENT-ADJUSTED EBITDA, not cumulative EBITDA.
price_factors <- financial_core |>
  select(airport, year, cpi_factor)
assert_unique(price_factors, c('airport', 'year'), 'CPI adjustment factors')
if (nrow(price_factors) != 12L || anyNA(price_factors) ||
    any(price_factors$cpi_factor <= 0)) stop('Missing CPI factors.')

annual_inputs <- baseline |>
  left_join(price_factors, by = c('airport', 'year')) |>
  mutate(period = 'Annual', year_label = as.character(year))

flow_vars <- c('revenue_m', 'ebitda_m', 'net_income_m',
               'net_financing_cost_m', 'capex_cash_m', 'ground_rent_m')
stock_vars <- c('gross_debt_m', 'cash_m')
pooled_vars <- c(flow_vars, stock_vars)
pooled_years <- length(years)
pooled_inputs <- annual_inputs |>
  mutate(across(all_of(pooled_vars), ~ .x * cpi_factor)) |>
  group_by(airport) |>
  summarise(
    across(all_of(flow_vars), ~ if (all(!is.na(.x))) sum(.x) else NA_real_),
    across(all_of(stock_vars), ~ if (all(!is.na(.x))) mean(.x) else NA_real_),
    passengers = sum(passengers), .groups = 'drop'
  ) |>
  mutate(year = NA_integer_, period = 'Pooled', year_label = '2023-25',
         cpi_factor = 1.0)

# BIG4 inputs are the sum of the four constituent airport baselines.
# Pooled BIG4 flows are three-year totals; debt/cash are average stocks.
make_big4_input <- function(df) {
  df |>
    group_by(period, year_label, year) |>
    summarise(across(all_of(c(pooled_vars, 'passengers')),
                     ~ if (all(!is.na(.x))) sum(.x) else NA_real_),
              cpi_factor = first(cpi_factor), .groups = 'drop') |>
    mutate(airport = 'BIG4')
}
# ---- 5. ACQUISITION MODEL AND PRE-TAX PROFIT THRESHOLDS ------------------
# Acquisition financing stays PRE-TAX. Illustrative corporate tax is calculated
# separately for the post-acquisition baseline and the private-profit benchmarks,
# for presentation on the Profitability requirements worksheet only.
# No assumed equity cash-distribution model.
# Existing debt and its observed financing expense are retained.
# Exactly three paired scenarios: low-cost, central, high-cost.
inputs_all <- bind_rows(annual_inputs, pooled_inputs,
                       make_big4_input(annual_inputs),
                       make_big4_input(pooled_inputs)) |>
  select(airport, year, period, year_label, cpi_factor,
         all_of(pooled_vars), passengers) |>
  mutate(statutory_tax_rate = unname(statutory_tax_rates[airport]))

scenarios <- scenario_assumptions |>
  mutate(scenario_id = sprintf('M%g-D%g-R%g', purchase_multiple,
                               100 * acquisition_debt_share,
                               100 * acquisition_interest_rate)) |>
  select(scenario_id, everything())
stopifnot(nrow(scenarios) == 3L,
          !anyDuplicated(scenarios$scenario_id),
          identical(scenarios$scenario_name, c('Low-cost','Central','High-cost')))
# The centre is not the midpoint of financing COST in every airport: it is
# the middle set of independent input assumptions. Keep case IDs consistent.
central_id <- scenarios$scenario_id[scenarios$scenario_name == 'Central']
stopifnot(identical(central_id, 'M22.5-D60-R5.5'))
# With too little contribution from additional revenue, a positive net margin
# cannot be sustained as revenue expands. Fail explicitly rather than divide
# by zero or return a misleading negative revenue requirement.
if (incremental_revenue_to_profit <=
    max(private_net_margin_targets / (1 - statutory_tax_rates))) {
  stop('Incremental revenue-to-profit conversion is too low to sustain ',
       'the highest after-tax net margin target at all four tax rates.',
       call. = FALSE)
}

model <- tidyr::crossing(inputs_all, scenarios) |>
  mutate(
    # Add in N years in the pooled period (should be three)
    years_in_period = if_else(period == 'Pooled', pooled_years, 1L),

    # Optional adjustment for operating expenses to grow along with revenue
    incremental_revenue_to_profit = !!incremental_revenue_to_profit,

    # Adjust EBITDA by adding ground rent back in
    rent_adjusted_ebitda_m = ebitda_m + ground_rent_m,

    # Annualized rent adjusted EBITDA, so calculations can also be applied to the pooled
    valuation_ebitda_m = rent_adjusted_ebitda_m / years_in_period,

    # Estimated enterprise value, including existing net debt
    estimated_sale_value_m = valuation_ebitda_m * purchase_multiple,

    # Equity purchase price after deducting existing debt and adding cash
    equity_purchase_consideration_m = estimated_sale_value_m - gross_debt_m + cash_m,

    # Check for invalid purchase prices
    feasible_purchase_price = equity_purchase_consideration_m > 0,

    # Purchase amount funded by new debt, calculated with the debt share parameter
    new_acquisition_debt_m = if_else(feasible_purchase_price,
      equity_purchase_consideration_m * acquisition_debt_share, NA_real_),

    # Purchase amount funded by new equity, residual of the debt funded share
    buyer_equity_contribution_m = equity_purchase_consideration_m - new_acquisition_debt_m,

    # Total gross airport related debts of the purchaser after airport acquisition
    total_gross_debt_m = gross_debt_m + new_acquisition_debt_m,

    # Ratio of total gross debts after purchase to the sale value
    actual_total_gross_debt_to_sale_value = total_gross_debt_m / estimated_sale_value_m,

    # Interest costs added by the purchase debts calculated using the interest rate parameter
    additional_acquisition_interest_m =
      new_acquisition_debt_m * acquisition_interest_rate * years_in_period,

    # Interest costs, sum of current observed costs and those added by the purchase
    combined_net_financing_cost_proxy_m = net_financing_cost_m + additional_acquisition_interest_m,

    # Interest costs added by purchase per passenger
    extra_interest_per_passenger_cad = additional_acquisition_interest_m * 1e6 / passengers,

    # Earnings before tax, depreciation and amortization, AFTER financing costs.
    # Unlike EBITDA, EBTDA includes the effect of existing net interest expense.
    observed_ebtda_m = ebitda_m - net_financing_cost_m,

    # Observed EBTDA margin
    observed_ebtda_margin_pct = observed_ebtda_m / revenue_m,

    # EBTDA after eliminating federal ground rent
    rent_adjusted_ebtda_m = rent_adjusted_ebitda_m - net_financing_cost_m,

    # EBTDA after the added interest costs of the airport purchase
    purchase_adjusted_ebtda_m = rent_adjusted_ebtda_m - additional_acquisition_interest_m,

    # Acquisition adjusted EBTDA margin
    purchase_adjusted_ebtda_margin_pct = purchase_adjusted_ebtda_m / revenue_m,

    # Observed pre-tax net profit margin
    # Equivalent to their net result since CAN airports don't pay income tax currently
    observed_pre_tax_margin_pct = net_income_m / revenue_m,

    # Pre-tax profit adjusted for federal ground rent (added back in)
    rent_adjusted_pre_tax_income_m = net_income_m + ground_rent_m,

    # Purchase-adjusted pre-tax profit:
    # Rent adjusted pre-tax income minus additional interest costs
    purchase_adjusted_pre_tax_income_m =
      rent_adjusted_pre_tax_income_m - additional_acquisition_interest_m,

    # Purchase adjusted pre-tax profit margin
    purchase_adjusted_pre_tax_margin_pct = purchase_adjusted_pre_tax_income_m / revenue_m,

    # Illustrative corporate tax at the acquisition-adjusted starting position,
    # before adding revenue. This is a reference for the profitability exercise,
    # not a deduction in the Acquisition financing presentation.
    estimated_tax_purchase_adjusted_m =
      pmax(0, purchase_adjusted_pre_tax_income_m) * statutory_tax_rate,
    purchase_adjusted_after_tax_income_m =
      purchase_adjusted_pre_tax_income_m - estimated_tax_purchase_adjusted_m,
    purchase_adjusted_after_tax_margin_pct =
      purchase_adjusted_after_tax_income_m / revenue_m,

    # Calculating profit thresholds
    # Step 1: recover nonnegative pre-tax earnings.
    required_profit_break_even_m = pmax(0, -purchase_adjusted_pre_tax_income_m),

    # Step 2: recover historical pre-tax income; never accept a loss as the
    # target if an observed year was already loss-making.
    target_pre_tax_income_restore_m = pmax(0, net_income_m),
    required_profit_restore_m = pmax(0, target_pre_tax_income_restore_m -
                                       purchase_adjusted_pre_tax_income_m),

    # Revenue proxies for steps 1 and 2: extra pre-tax profit / contribution rate.
    required_revenue_break_even_m = required_profit_break_even_m /
                                      incremental_revenue_to_profit,
    required_revenue_restore_m = required_profit_restore_m /
                                   incremental_revenue_to_profit,

    # Step 3: TOTAL after-tax net-margin targets of 15%, 20%, 25%, and 30%.
    # The net margin denominator is current total revenue + extra revenue.
    # For each target m: [(P + c*x)*(1-t)] / (R+x) = m, where
    #   P = purchase-adjusted pre-tax earnings, R = observed revenue,
    #   c = incremental-revenue-to-profit contribution, t = statutory tax rate,
    #   x = additional revenue. The positive target implies positive taxable
    #   profit, so (1-t) applies at the target. A zero lift is permissible if
    #   the operator already exceeds the net margin target.
    # 15% after-tax net margin on revenue AFTER the modeled uplift.
    #
    # Incremental revenue-to-profit contribution:
    # Additional revenue may also increase operating costs, so not every
    # additional dollar necessarily translates into pre-tax profit.
    # This parameter specifies the share of additional revenue retained
    # as pre-tax profit. The default of 1.0 assumes a 100% contribution,
    # meaning no additional operating costs.
    
    target_net_margin_15pct = private_net_margin_targets[[1]],
    
    # Additional revenue required to achieve a 15% after-tax net profit margin.
    #
    # Numerator: The target profit margin multiplied by existing revenue,
    # minus the airport's estimated after-tax profit following acquisition.
    # This is the amount of additional profit needed to reach the target
    # if the airport's total revenue remained unchanged.
    #
    # Denominator: The share of each additional revenue dollar retained
    # as profit after corporate income tax, minus the target profit margin.
    # This is the amount by which each new revenue dollar reduces the
    # profit shortfall, accounting for the fact that additional revenue
    # also increases the total revenue on which the margin is calculated.
    #
    # Dividing the profit shortfall by the effective contribution of each
    # additional revenue dollar gives the required revenue increase.
    # The zero floor applies if the target is already achieved.
    required_revenue_net_15pct_m = pmax(0,
      (target_net_margin_15pct * revenue_m -
        (1 - statutory_tax_rate) * purchase_adjusted_pre_tax_income_m) /
      ((1 - statutory_tax_rate) * incremental_revenue_to_profit -
         target_net_margin_15pct)),
    
    # Adjustment to profit requirements using incremental revenue parameter
    required_profit_net_15pct_m = required_revenue_net_15pct_m * incremental_revenue_to_profit,
    
    # Amount needed to hit target: total revenue + required for profit threshold
    counterfactual_revenue_net_15pct_m = revenue_m + required_revenue_net_15pct_m,
    
    # Calculate the pre-tax income required to achieve the target net margin.
    # First, multiply counterfactual revenue by the target margin to get
    # the desired after-tax profit. Then divide by the share of pre-tax
    # income remaining after corporate taxes to obtain the pre-tax amount.
    target_pre_tax_income_net_15pct_m =
      target_net_margin_15pct * counterfactual_revenue_net_15pct_m /
        (1 - statutory_tax_rate),
    
    # New counter factual pre-tax income at the specified after-tax profit threshold
    counterfactual_pre_tax_income_net_15pct_m =
      purchase_adjusted_pre_tax_income_m + required_profit_net_15pct_m,
    
    # Estimate corporate income taxes on the airport's counterfactual
    # pre-tax profit. Taxes are calculated at the statutory rate, with
    # zero tax payable if the airport is operating at a loss.
    estimated_tax_net_15pct_m =
      pmax(0, counterfactual_pre_tax_income_net_15pct_m) * statutory_tax_rate,
    
    # Calculate the change in corporate income taxes between the
    # post-acquisition starting position and the 15% profit scenario.
    # This measures the additional tax liability associated with
    # increasing revenue and profit to meet the target.
    incremental_tax_net_15pct_m =
      estimated_tax_net_15pct_m - estimated_tax_purchase_adjusted_m,
    
    # Calculate net income by subtracting estimated corporate income
    # taxes from the counterfactual pre-tax profit.
    counterfactual_after_tax_income_net_15pct_m =
      counterfactual_pre_tax_income_net_15pct_m - estimated_tax_net_15pct_m,
    
    # Divide after-tax profit by counterfactual total revenue to
    # calculate the resulting net profit margin. This should equal
    # the 15% target, subject to rounding, unless the airport already
    # exceeded the target before any additional revenue was required.
    counterfactual_net_margin_net_15pct_pct =
      counterfactual_after_tax_income_net_15pct_m / counterfactual_revenue_net_15pct_m,

    # 20% after-tax net margin on revenue AFTER the modeled uplift.
    target_net_margin_20pct = private_net_margin_targets[[2]],
    required_revenue_net_20pct_m = pmax(0,
      (target_net_margin_20pct * revenue_m -
        (1 - statutory_tax_rate) * purchase_adjusted_pre_tax_income_m) /
      ((1 - statutory_tax_rate) * incremental_revenue_to_profit -
         target_net_margin_20pct)),
    required_profit_net_20pct_m = required_revenue_net_20pct_m * incremental_revenue_to_profit,
    counterfactual_revenue_net_20pct_m = revenue_m + required_revenue_net_20pct_m,
    target_pre_tax_income_net_20pct_m =
      target_net_margin_20pct * counterfactual_revenue_net_20pct_m /
        (1 - statutory_tax_rate),
    counterfactual_pre_tax_income_net_20pct_m =
      purchase_adjusted_pre_tax_income_m + required_profit_net_20pct_m,
    estimated_tax_net_20pct_m =
      pmax(0, counterfactual_pre_tax_income_net_20pct_m) * statutory_tax_rate,
    # Additional modeled corporation tax compared with the post-acquisition
    # starting point (not a claim about actual federal/provincial receipts).
    incremental_tax_net_20pct_m =
      estimated_tax_net_20pct_m - estimated_tax_purchase_adjusted_m,
    counterfactual_after_tax_income_net_20pct_m =
      counterfactual_pre_tax_income_net_20pct_m - estimated_tax_net_20pct_m,
    counterfactual_net_margin_net_20pct_pct =
      counterfactual_after_tax_income_net_20pct_m / counterfactual_revenue_net_20pct_m,

    # 25% after-tax net margin on revenue AFTER the modeled uplift.
    target_net_margin_25pct = private_net_margin_targets[[3]],
    required_revenue_net_25pct_m = pmax(0,
      (target_net_margin_25pct * revenue_m -
        (1 - statutory_tax_rate) * purchase_adjusted_pre_tax_income_m) /
      ((1 - statutory_tax_rate) * incremental_revenue_to_profit -
         target_net_margin_25pct)),
    required_profit_net_25pct_m = required_revenue_net_25pct_m * incremental_revenue_to_profit,
    counterfactual_revenue_net_25pct_m = revenue_m + required_revenue_net_25pct_m,
    target_pre_tax_income_net_25pct_m =
      target_net_margin_25pct * counterfactual_revenue_net_25pct_m /
        (1 - statutory_tax_rate),
    counterfactual_pre_tax_income_net_25pct_m =
      purchase_adjusted_pre_tax_income_m + required_profit_net_25pct_m,
    estimated_tax_net_25pct_m =
      pmax(0, counterfactual_pre_tax_income_net_25pct_m) * statutory_tax_rate,
    # Additional modeled corporation tax compared with the post-acquisition
    # starting point (not a claim about actual federal/provincial receipts).
    incremental_tax_net_25pct_m =
      estimated_tax_net_25pct_m - estimated_tax_purchase_adjusted_m,
    counterfactual_after_tax_income_net_25pct_m =
      counterfactual_pre_tax_income_net_25pct_m - estimated_tax_net_25pct_m,
    counterfactual_net_margin_net_25pct_pct =
      counterfactual_after_tax_income_net_25pct_m / counterfactual_revenue_net_25pct_m,

    # 30% after-tax net margin on revenue AFTER the modeled uplift.
    target_net_margin_30pct = private_net_margin_targets[[4]],
    required_revenue_net_30pct_m = pmax(0,
      (target_net_margin_30pct * revenue_m -
        (1 - statutory_tax_rate) * purchase_adjusted_pre_tax_income_m) /
      ((1 - statutory_tax_rate) * incremental_revenue_to_profit -
         target_net_margin_30pct)),
    required_profit_net_30pct_m = required_revenue_net_30pct_m * incremental_revenue_to_profit,
    counterfactual_revenue_net_30pct_m = revenue_m + required_revenue_net_30pct_m,
    target_pre_tax_income_net_30pct_m =
      target_net_margin_30pct * counterfactual_revenue_net_30pct_m /
        (1 - statutory_tax_rate),
    counterfactual_pre_tax_income_net_30pct_m =
      purchase_adjusted_pre_tax_income_m + required_profit_net_30pct_m,
    estimated_tax_net_30pct_m =
      pmax(0, counterfactual_pre_tax_income_net_30pct_m) * statutory_tax_rate,
    # Additional modeled corporation tax compared with the post-acquisition
    # starting point (not a claim about actual federal/provincial receipts).
    incremental_tax_net_30pct_m =
      estimated_tax_net_30pct_m - estimated_tax_purchase_adjusted_m,
    counterfactual_after_tax_income_net_30pct_m =
      counterfactual_pre_tax_income_net_30pct_m - estimated_tax_net_30pct_m,
    counterfactual_net_margin_net_30pct_pct =
      counterfactual_after_tax_income_net_30pct_m / counterfactual_revenue_net_30pct_m
  )

# BIG4 profit requirements SUM each airport's individually positive gap.
# Taxes and net-margin targets are computed at airport level, then summed;
# applying a single tax rate to consolidated BIG4 profits would be incorrect.
profit_aggregate_cols <- c(
  # BIG4 baseline tax and after-tax earnings must also be calculated per airport.
  'estimated_tax_purchase_adjusted_m',
  'purchase_adjusted_after_tax_income_m',
  'target_pre_tax_income_restore_m',
  'required_profit_break_even_m', 'required_profit_restore_m',
  'required_revenue_break_even_m', 'required_revenue_restore_m',
  'required_revenue_net_15pct_m',
  'required_profit_net_15pct_m',
  'counterfactual_revenue_net_15pct_m',
  'target_pre_tax_income_net_15pct_m',
  'counterfactual_pre_tax_income_net_15pct_m',
  'estimated_tax_net_15pct_m',
  'incremental_tax_net_15pct_m',
  'counterfactual_after_tax_income_net_15pct_m',
  'required_revenue_net_20pct_m',
  'required_profit_net_20pct_m',
  'counterfactual_revenue_net_20pct_m',
  'target_pre_tax_income_net_20pct_m',
  'counterfactual_pre_tax_income_net_20pct_m',
  'estimated_tax_net_20pct_m',
  'incremental_tax_net_20pct_m',
  'counterfactual_after_tax_income_net_20pct_m',
  'required_revenue_net_25pct_m',
  'required_profit_net_25pct_m',
  'counterfactual_revenue_net_25pct_m',
  'target_pre_tax_income_net_25pct_m',
  'counterfactual_pre_tax_income_net_25pct_m',
  'estimated_tax_net_25pct_m',
  'incremental_tax_net_25pct_m',
  'counterfactual_after_tax_income_net_25pct_m',
  'required_revenue_net_30pct_m',
  'required_profit_net_30pct_m',
  'counterfactual_revenue_net_30pct_m',
  'target_pre_tax_income_net_30pct_m',
  'counterfactual_pre_tax_income_net_30pct_m',
  'estimated_tax_net_30pct_m',
  'incremental_tax_net_30pct_m',
  'counterfactual_after_tax_income_net_30pct_m'
)
big4_profit_targets <- model |>
  filter(airport != 'BIG4') |>
  group_by(scenario_id, period, year_label) |>
  summarise(across(all_of(profit_aggregate_cols), sum), .groups = 'drop') |>
  mutate(airport = 'BIG4')

# ---- FINAL ACQUISITION AND PROFITABILITY RESULTS --------------------------
#
# Assemble the final results for each airport, financing scenario, and
# observation period (annual or pooled).
#
# The BIG4 aggregate combines the results of four independent airport
# acquisitions. Each airport has its own purchase price, financing costs,
# corporate tax rate, and profitability requirements. The BIG4 results
# therefore sum the individual airport calculations rather than applying
# a single set of assumptions to a hypothetical combined operator.
#
# Calculate the additional revenue required to break even, restore
# historical profitability, or achieve after-tax net profit margins of
# 15%, 20%, 25%, and 30%. Express these requirements both as increases
# relative to existing revenue and as additional dollars per passenger.
#
# Finally, calculate indicators of whether acquisition financing costs
# and continued capital investment could be supported by the airport's
# existing earnings.

model <- model |>
  rows_update(big4_profit_targets,
              by = c('scenario_id', 'period', 'year_label', 'airport')) |>
  mutate(
    # Recalculate the post-acquisition net margin after corporate taxes.
    purchase_adjusted_after_tax_margin_pct =
      purchase_adjusted_after_tax_income_m / revenue_m,
    
    # Express the revenue needed to break even or restore historical
    # profitability as a share of existing revenue.
    required_uplift_break_even_pct = required_revenue_break_even_m / revenue_m,
    required_uplift_restore_pct = required_revenue_restore_m / revenue_m,
    
    # Convert required additional revenue into dollars per passenger.
    required_revenue_break_even_per_pax_cad =
      required_revenue_break_even_m * 1e6 / passengers,
    required_revenue_restore_per_pax_cad =
      required_revenue_restore_m * 1e6 / passengers,
    
    # 15% net margin: calculate the achieved margin, required revenue
    # increase relative to existing revenue, and increase per passenger.
    # BIG4 margins use summed airport profits and revenues.
    counterfactual_net_margin_net_15pct_pct =
      counterfactual_after_tax_income_net_15pct_m / counterfactual_revenue_net_15pct_m,
    required_uplift_net_15pct_pct = required_revenue_net_15pct_m / revenue_m,
    required_revenue_net_15pct_per_pax_cad =
      required_revenue_net_15pct_m * 1e6 / passengers,
    
    # 20% net margin: same calculations.
    counterfactual_net_margin_net_20pct_pct =
      counterfactual_after_tax_income_net_20pct_m / counterfactual_revenue_net_20pct_m,
    required_uplift_net_20pct_pct = required_revenue_net_20pct_m / revenue_m,
    required_revenue_net_20pct_per_pax_cad =
      required_revenue_net_20pct_m * 1e6 / passengers,
    
    # 25% net margin: same calculations.
    counterfactual_net_margin_net_25pct_pct =
      counterfactual_after_tax_income_net_25pct_m / counterfactual_revenue_net_25pct_m,
    required_uplift_net_25pct_pct = required_revenue_net_25pct_m / revenue_m,
    required_revenue_net_25pct_per_pax_cad =
      required_revenue_net_25pct_m * 1e6 / passengers,
    
    # 30% net margin: same calculations.
    counterfactual_net_margin_net_30pct_pct =
      counterfactual_after_tax_income_net_30pct_m / counterfactual_revenue_net_30pct_m,
    required_uplift_net_30pct_pct = required_revenue_net_30pct_m / revenue_m,
    required_revenue_net_30pct_per_pax_cad =
      required_revenue_net_30pct_m * 1e6 / passengers,
    
    # Approximate cash remaining after financing costs and capital spending.
    # Uses EBTDA, so depreciation and amortization are not deducted.
    cash_after_net_financing_capex_proxy_m =
      purchase_adjusted_ebtda_m - capex_cash_m,
    
    # Flag scenarios where combined financing costs equal or exceed
    # rent-adjusted EBITDA.
    interest_more_than_ebitda =
      combined_net_financing_cost_proxy_m >= rent_adjusted_ebitda_m,
    
    # Flag scenarios where financing costs plus capital expenditures
    # exceed rent-adjusted EBITDA.
    investment_plus_interest_exceeds_ebitda =
      combined_net_financing_cost_proxy_m + capex_cash_m > rent_adjusted_ebitda_m
  ) |>
  # Order individual airports first, followed by the BIG4 aggregate.
  mutate(airport = factor(airport, levels = c(can_airports, 'BIG4'))) |>
  arrange(scenario_id, period, year_label, airport) |>
  mutate(airport = as.character(airport))

# ---- 6. RECONCILIATION CHECKS --------------------------------------------
pooled_check <- annual_inputs |>
  mutate(across(all_of(flow_vars), ~ .x * cpi_factor)) |>
  group_by(airport) |>
  summarise(across(all_of(flow_vars), ~ if (all(!is.na(.x))) sum(.x) else NA_real_),
            passengers = sum(passengers), .groups = 'drop') |>
  inner_join(pooled_inputs, by = 'airport', suffix = c('_sum', '_pooled'))
for (nm in c(flow_vars, 'passengers')) {
  stopifnot(isTRUE(all.equal(pooled_check[[paste0(nm, '_sum')]],
                             pooled_check[[paste0(nm, '_pooled')]],
                             tolerance = 1e-9)))
}
stopifnot(nrow(model) == 5L * (length(years) + 1L) * nrow(scenarios))
stopifnot(all(model$equity_purchase_consideration_m > 0))
stopifnot(all(abs(model$new_acquisition_debt_m / model$equity_purchase_consideration_m -
                  model$acquisition_debt_share) < 1e-8))
stopifnot(all(abs(model$rent_adjusted_ebitda_m -
                  (model$ebitda_m + model$ground_rent_m)) < 1e-8))
stopifnot(all(abs(model$estimated_sale_value_m -
                  model$rent_adjusted_ebitda_m / model$years_in_period *
                  model$purchase_multiple) < 1e-8))
stopifnot(all(abs(model$estimated_sale_value_m -
                  (model$equity_purchase_consideration_m + model$gross_debt_m - model$cash_m)) < 1e-8))
stopifnot(all(abs(model$purchase_adjusted_ebtda_m -
    (model$ebitda_m + model$ground_rent_m - model$net_financing_cost_m -
     model$additional_acquisition_interest_m)) < 1e-8))
stopifnot(all(abs(model$purchase_adjusted_pre_tax_income_m -
    (model$net_income_m + model$ground_rent_m - model$additional_acquisition_interest_m)) < 1e-8))
stopifnot(all(model$required_profit_restore_m + 1e-8 >=
              model$required_profit_break_even_m))

# At the target, additional revenues increase the denominator of the margin.
# Independently recheck the final after-tax ratio and nonnegative cash uplift.
for (p in c(15,20,25,30)) {
  tag <- paste0('net_', p, 'pct')
  amt <- model[[paste0('required_revenue_', tag, '_m')]]
  profit <- model[[paste0('required_profit_', tag, '_m')]]
  cf_rev <- model[[paste0('counterfactual_revenue_', tag, '_m')]]
  cf_pre <- model[[paste0('counterfactual_pre_tax_income_', tag, '_m')]]
  cf_tax <- model[[paste0('estimated_tax_', tag, '_m')]]
  cf_net <- model[[paste0('counterfactual_after_tax_income_', tag, '_m')]]
  cf_margin <- model[[paste0('counterfactual_net_margin_', tag, '_pct')]]
  stopifnot(all(is.finite(amt)), all(amt >= 0),
            all(abs(profit - amt * incremental_revenue_to_profit) < 1e-7),
            all(abs(cf_rev - model$revenue_m - amt) < 1e-7),
            all(abs(cf_pre - model$purchase_adjusted_pre_tax_income_m - profit) < 1e-7),
            all(abs(cf_net - (cf_pre - cf_tax)) < 1e-7),
            all(abs(cf_margin - cf_net / cf_rev) < 1e-10),
            all(cf_margin + 1e-9 >= p / 100))
  individual <- model$airport != 'BIG4'
  # When additional revenue is required, the achieved margin equals the target.
  active <- individual & amt > 1e-8
  stopifnot(all(abs(cf_margin[active] - p / 100) < 1e-9))
}

# Tax must reconcile against the separately taxed acquisition starting point.
stopifnot(all(abs(model$purchase_adjusted_after_tax_income_m -
                   (model$purchase_adjusted_pre_tax_income_m -
                      model$estimated_tax_purchase_adjusted_m)) < 1e-7),
          all(abs(model$purchase_adjusted_after_tax_margin_pct -
                   model$purchase_adjusted_after_tax_income_m / model$revenue_m) < 1e-10))
for (p in c(15,20,25,30)) {
  tag <- paste0('net_', p, 'pct')
  tax <- model[[paste0('estimated_tax_', tag, '_m')]]
  tax_increment <- model[[paste0('incremental_tax_', tag, '_m')]]
  stopifnot(all(abs(tax_increment -
                    (tax - model$estimated_tax_purchase_adjusted_m)) < 1e-7),
            all(tax_increment >= -1e-8))
}
# Aggregation consistency of the BIG4 profit targets and requirements.
for (nm in profit_aggregate_cols) {
  check <- model |>
    group_by(scenario_id, period, year_label) |>
    summarise(airport_sum = sum(.data[[nm]][airport != 'BIG4']),
              big4 = .data[[nm]][airport == 'BIG4'], .groups = 'drop')
  stopifnot(all(abs(check$airport_sum - check$big4) < 1e-8))
}

# ---- 7. PREPARE ACQUISITION AND PROFITABILITY RESULTS ---------------------
#
# Organize the acquisition and profitability calculations into consistent
# tables for presentation, validation, and export.
#
# Select the central financing scenario, identify source and calculated
# acquisition variables, and reshape the six profitability thresholds
# into a common long-format panel.
#
# Add intermediate profitability measures to make the revenue requirements
# easier to interpret and verify. Check that the reshaped results reproduce
# the original calculations and that BIG4 totals equal the sum of the
# independently modelled airports.
#
# Export the complete acquisition and profitability scenario datasets.
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

central_scenario <- function(d) {
  d |> filter(purchase_multiple == 22.5,
              acquisition_debt_share == 0.60,
              acquisition_interest_rate == 0.055)
}
central <- central_scenario(model)
# central_id was constructed directly from the scenario input table.
stopifnot(nrow(central) == 20L, all(central$scenario_id == central_id))

acquisition_audit_cols <- c(
  'scenario_id','airport','year_label',
  'purchase_multiple','acquisition_debt_share','acquisition_interest_rate',
  'years_in_period','cpi_factor','passengers','revenue_m','ebitda_m',
  'ground_rent_m','rent_adjusted_ebitda_m','valuation_ebitda_m',
  'estimated_sale_value_m','gross_debt_m','cash_m',
  'equity_purchase_consideration_m','new_acquisition_debt_m',
  'buyer_equity_contribution_m','total_gross_debt_m',
  'actual_total_gross_debt_to_sale_value','net_financing_cost_m',
  'additional_acquisition_interest_m','combined_net_financing_cost_proxy_m',
  'purchase_adjusted_ebtda_m','net_income_m',
  'rent_adjusted_pre_tax_income_m','purchase_adjusted_pre_tax_income_m',
  'purchase_adjusted_pre_tax_margin_pct'
)
acquisition_source_cols <- c('passengers','revenue_m','ebitda_m',
  'ground_rent_m','gross_debt_m','cash_m','net_financing_cost_m','net_income_m')
acquisition_formula_cols <- setdiff(acquisition_audit_cols,
  c('scenario_id','airport','year_label',
    'purchase_multiple','acquisition_debt_share','acquisition_interest_rate',
    'years_in_period','cpi_factor',acquisition_source_cols))

# ---- ORGANIZE PROFITABILITY THRESHOLDS INTO A COMMON PANEL ---------------
#
# The acquisition model calculates six alternative profitability objectives
# for each airport, year, and financing scenario:
#   1. Break even: eliminate any post-acquisition pre-tax loss.
#   2. Restore historical profit: recover the airport's observed pre-tax
#      profit, or break even if the airport originally reported a loss.
#   3-6. Achieve after-tax net profit margins of 15%, 20%, 25%, or 30%.
#
# Each objective is calculated independently from the same post-acquisition
# starting position. They represent alternative revenue requirements,
# not successive increases that must be added together.
#
# The following code takes the results calculated in Section 5 and
# reorganizes them into a common format, with one row per profitability
# objective, airport, period, and financing scenario.
#
# It also adds intermediate measures showing the initial profit shortfall
# and how much each additional dollar of revenue contributes toward
# closing that shortfall. These make the calculations easier to inspect
# and compare across the six objectives.

# For fixed pre-tax targets, each additional revenue dollar closes the
# profit shortfall by the assumed pre-tax contribution rate (c).
#
# For after-tax margin targets, the effective contribution is smaller:
# c - m/(1-t), where m is the target net margin and t is the tax rate.
# This accounts for the additional profit needed to maintain the target
# margin as total revenue increases.
profitability_net <- purrr::map_dfr(c(15,20,25,30), function(p) {
  tag <- paste0('net_',p,'pct')
  col <- function(stem,suffix='_m') paste0(stem,'_',tag,suffix)
  model |>
    transmute(
      scenario_id,airport,year,period,year_label,
      purchase_multiple,acquisition_debt_share,acquisition_interest_rate,
      cpi_factor,passengers,
      threshold_id=tag,
      threshold_order=as.integer(match(p,c(15,20,25,30))+2L),
      profitability_objective=paste0(match(p,c(15,20,25,30))+2L,
        '. ',p,'% after-tax margin'),
      target_type='After-tax margin',
      target_net_margin=p/100,
      statutory_tax_rate,incremental_revenue_to_profit,
      revenue_m,net_income_m,purchase_adjusted_pre_tax_income_m,
      estimated_tax_purchase_adjusted_m,
      purchase_adjusted_after_tax_income_m,
      # After tax, owners retain (1-t) of each dollar of pre-tax profit.
      # Thus target net margin m requires a pre-tax margin m/(1-t).
      required_pre_tax_margin=if_else(
        airport=='BIG4',NA_real_,(p/100)/(1-statutory_tax_rate)),
      # This is the profit target on ORIGINAL revenue, before the denominator
      # expands. It explains the opening profit gap, not the final target.
      pre_tax_profit_target_at_existing_revenue_m=if_else(
        airport=='BIG4',NA_real_,required_pre_tax_margin*revenue_m),
      initial_pre_tax_shortfall_m=if_else(airport=='BIG4',NA_real_,
        pmax(0,pre_tax_profit_target_at_existing_revenue_m -
          purchase_adjusted_pre_tax_income_m)),
      shortfall_closed_per_new_revenue_dollar=if_else(
        airport=='BIG4',NA_real_,
        incremental_revenue_to_profit - required_pre_tax_margin),
      required_revenue_m=.data[[col('required_revenue')]],
      required_profit_m=.data[[col('required_profit')]],
      counterfactual_revenue_m=.data[[col('counterfactual_revenue')]],
      counterfactual_pre_tax_income_m=.data[[col('counterfactual_pre_tax_income')]],
      estimated_tax_m=.data[[col('estimated_tax')]],
      incremental_tax_m=.data[[col('incremental_tax')]],
      counterfactual_after_tax_income_m=.data[[col('counterfactual_after_tax_income')]],
      achieved_net_margin=.data[[col('counterfactual_net_margin','_pct')]],
      required_uplift_pct=.data[[col('required_uplift','_pct')]],
      required_revenue_per_pax_cad=.data[[col('required_revenue','_per_pax_cad')]],
      unmet_target_pct=pmax(0,target_net_margin-achieved_net_margin)
    )
}) |>
  group_by(scenario_id,period,year_label,threshold_id) |>
  mutate(
    # Summed targets / positive shortfalls, not a fictional BIG4 tax rate.
    pre_tax_profit_target_at_existing_revenue_m=if_else(airport=='BIG4',
      sum(pre_tax_profit_target_at_existing_revenue_m[airport!='BIG4']),
      pre_tax_profit_target_at_existing_revenue_m),
    initial_pre_tax_shortfall_m=if_else(airport=='BIG4',
      sum(initial_pre_tax_shortfall_m[airport!='BIG4']),
      initial_pre_tax_shortfall_m)) |>
  ungroup()

# ---- PREPARE BREAK-EVEN AND HISTORICAL PROFIT RESTORATION RESULTS ---------
#
# Organize the two fixed pre-tax profitability objectives into the same
# table structure used for the four after-tax net margin targets above.
#
# The required revenue increases were already calculated in Section 5.
# Unlike the net margin targets, these objectives require a fixed amount
# of pre-tax profit, regardless of how much additional revenue is generated.
# Corporate income taxes therefore do not affect the revenue required
# to meet either objective.
#
# Calculate the resulting corporate taxes, after-tax income, and net
# profit margins for each objective. For BIG4, sum the taxes calculated
# separately for the four airports rather than applying a single tax rate.
profitability_recovery <- purrr::map_dfr(
  c('break_even','restore'),function(kind) {
    is_break_even <- identical(kind,'break_even')
    model |>
      transmute(
        scenario_id,airport,year,period,year_label,
        purchase_multiple,acquisition_debt_share,acquisition_interest_rate,
        cpi_factor,passengers,
        threshold_id=kind,
        threshold_order=if(is_break_even) 1L else 2L,
        profitability_objective=if(is_break_even) '1. Break even' else
          '2. Restore historical profit',
        target_type='Fixed pre-tax',
        target_net_margin=NA_real_,
        statutory_tax_rate,incremental_revenue_to_profit,
        revenue_m,net_income_m,purchase_adjusted_pre_tax_income_m,
        estimated_tax_purchase_adjusted_m,
        purchase_adjusted_after_tax_income_m,
        required_pre_tax_margin=NA_real_,
        pre_tax_profit_target_at_existing_revenue_m=
          if(is_break_even) 0 else target_pre_tax_income_restore_m,
        initial_pre_tax_shortfall_m=
          if(is_break_even) required_profit_break_even_m else required_profit_restore_m,
        shortfall_closed_per_new_revenue_dollar=if_else(
          airport=='BIG4',NA_real_,incremental_revenue_to_profit),
        required_revenue_m=
          if(is_break_even) required_revenue_break_even_m else required_revenue_restore_m,
        required_profit_m=
          if(is_break_even) required_profit_break_even_m else required_profit_restore_m,
        counterfactual_revenue_m=revenue_m+required_revenue_m,
        counterfactual_pre_tax_income_m=
          purchase_adjusted_pre_tax_income_m+required_profit_m,
        estimated_tax_m=if_else(airport=='BIG4',NA_real_,
          pmax(0,counterfactual_pre_tax_income_m)*statutory_tax_rate),
        incremental_tax_m=NA_real_,
        counterfactual_after_tax_income_m=NA_real_,
        achieved_net_margin=NA_real_,
        required_uplift_pct=required_revenue_m/revenue_m,
        required_revenue_per_pax_cad=required_revenue_m*1e6/passengers,
        unmet_target_pct=NA_real_
      )
  }) |>
  group_by(scenario_id,period,year_label,threshold_id) |>
  mutate(
    # Tax in BIG4 is the sum of separately taxed airports, even for the two
    # fixed pre-tax recovery scenarios.
    estimated_tax_m=if_else(airport=='BIG4',
      sum(estimated_tax_m[airport!='BIG4']),estimated_tax_m),
    incremental_tax_m=estimated_tax_m-estimated_tax_purchase_adjusted_m,
    counterfactual_after_tax_income_m=
      counterfactual_pre_tax_income_m-estimated_tax_m,
    achieved_net_margin=counterfactual_after_tax_income_m/counterfactual_revenue_m
  ) |>
  ungroup()

# Common six-threshold panel; every airport/period/scenario has six rows.
# BIG4 retains blank rate/denominator fields whenever an aggregate single
# formula would hide the four independently solved airport requirements.
profitability_model <- bind_rows(profitability_recovery,profitability_net) |>
  arrange(scenario_id,period,year_label,
          factor(airport,levels=c(can_airports,'BIG4')),threshold_order)
stopifnot(nrow(profitability_model)==6L*nrow(model),
  !anyDuplicated(profitability_model[c('scenario_id','airport','period',
                                     'year_label','threshold_id')]),
  all(profitability_model$threshold_order %in% 1:6),
  all(is.finite(profitability_model$required_revenue_m)),
  all(profitability_model$required_revenue_m>=0))

# ---- VALIDATE PROFITABILITY CALCULATIONS ---------------------------------
#
# Verify that the profitability panel reproduces the calculations used
# to determine the revenue required under each of the six objectives.
#
# The tests below check the relationships between profit targets,
# existing earnings, revenue requirements, corporate taxes, and
# resulting profit margins.
#
# Each test must pass for the script to continue. Small differences
# are permitted to account for floating-point rounding.

# 1. Validate the four after-tax net profit margin targets.
# Exclude BIG4 because its results combine four separately calculated
# airport requirements rather than applying a single tax rate.
individual_profits <- profitability_net |>
  filter(airport != 'BIG4')

stopifnot(
  # Each additional revenue dollar must make a positive contribution
  # toward closing the profit shortfall.
  all(individual_profits$shortfall_closed_per_new_revenue_dollar > 0),
  
  # The initial pre-tax profit target must equal existing revenue
  # multiplied by the pre-tax margin needed to achieve the net target.
  all(abs(
    individual_profits$pre_tax_profit_target_at_existing_revenue_m -
      individual_profits$required_pre_tax_margin *
      individual_profits$revenue_m
  ) < 1e-8),
  
  # The initial shortfall must equal the difference between the
  # target profit and post-acquisition profit, floored at zero.
  all(abs(
    individual_profits$initial_pre_tax_shortfall_m -
      pmax(0,
           individual_profits$pre_tax_profit_target_at_existing_revenue_m -
             individual_profits$purchase_adjusted_pre_tax_income_m)
  ) < 1e-8),
  
  # Required additional revenue must equal the initial shortfall
  # divided by the contribution of each new revenue dollar.
  all(abs(
    individual_profits$required_revenue_m -
      individual_profits$initial_pre_tax_shortfall_m /
      individual_profits$shortfall_closed_per_new_revenue_dollar
  ) < 1e-6),
  
  # After-tax income must equal pre-tax income minus corporate taxes.
  # This check applies to all six profitability objectives.
  all(abs(
    profitability_model$counterfactual_after_tax_income_m -
      (profitability_model$counterfactual_pre_tax_income_m -
         profitability_model$estimated_tax_m)
  ) < 1e-6),
  
  # The achieved net profit margin must equal after-tax income
  # divided by total revenue, including any required revenue increase.
  all(abs(
    profitability_model$achieved_net_margin -
      profitability_model$counterfactual_after_tax_income_m /
      profitability_model$counterfactual_revenue_m
  ) < 1e-10)
)

# 2. Validate the break-even and historical profit restoration targets.
# These are fixed pre-tax objectives, so the required revenue depends
# on the pre-tax profit shortfall rather than an after-tax margin.
individual_recovery <- profitability_recovery |>
  filter(airport != 'BIG4')

stopifnot(
  # The shortfall must equal the difference between the fixed
  # pre-tax profit target and post-acquisition profit, floored at zero.
  all(abs(
    individual_recovery$initial_pre_tax_shortfall_m -
      pmax(0,
           individual_recovery$pre_tax_profit_target_at_existing_revenue_m -
             individual_recovery$purchase_adjusted_pre_tax_income_m)
  ) < 1e-8),
  
  # Required revenue must equal the pre-tax profit shortfall divided
  # by the share of additional revenue retained as pre-tax profit.
  all(abs(
    individual_recovery$required_revenue_m -
      individual_recovery$initial_pre_tax_shortfall_m /
      individual_recovery$incremental_revenue_to_profit
  ) < 1e-6),
  
  # After the revenue increase, pre-tax income must meet or exceed
  # the break-even or historical profit restoration target.
  all(
    individual_recovery$counterfactual_pre_tax_income_m + 1e-8 >=
      individual_recovery$pre_tax_profit_target_at_existing_revenue_m
  )
)

# BIG4 requirements and taxes must be sums of four airport-specific values.
profit_big4_check <- profitability_model |>
  group_by(scenario_id,period,year_label,threshold_id) |>
  summarise(gap_airports=sum(initial_pre_tax_shortfall_m[airport!='BIG4']),
    gap_big4=initial_pre_tax_shortfall_m[airport=='BIG4'],
    target_airports=sum(pre_tax_profit_target_at_existing_revenue_m[airport!='BIG4']),
    target_big4=pre_tax_profit_target_at_existing_revenue_m[airport=='BIG4'],
    revenue_airports=sum(required_revenue_m[airport!='BIG4']),
    revenue_big4=required_revenue_m[airport=='BIG4'],
    tax_airports=sum(estimated_tax_m[airport!='BIG4']),
    tax_big4=estimated_tax_m[airport=='BIG4'],.groups='drop')
stopifnot(all(abs(profit_big4_check$gap_airports-profit_big4_check$gap_big4)<1e-6),
  all(abs(profit_big4_check$target_airports-profit_big4_check$target_big4)<1e-6),
  all(abs(profit_big4_check$revenue_airports-profit_big4_check$revenue_big4)<1e-6),
  all(abs(profit_big4_check$tax_airports-profit_big4_check$tax_big4)<1e-6))

# Output 2: compact acquisition/recovery data, three cases and all periods.
acquisition_export <- model |>
  select(year,period,all_of(acquisition_audit_cols),
         target_pre_tax_income_restore_m,required_profit_break_even_m,
         required_profit_restore_m,required_revenue_break_even_m,
         required_revenue_restore_m,required_revenue_break_even_per_pax_cad,
         required_revenue_restore_per_pax_cad)
readr::write_csv(acquisition_export,
  file.path(out_dir,'acquisition_scenarios.csv'))
# Output 3: all net-margin calculations, long format.
readr::write_csv(profitability_model,
  file.path(out_dir,'profitability_scenarios.csv'))

# ---- 8. WORKBOOK PRESENTATION, ASSUMPTIONS AND STYLES --------------------
wb <- openxlsx::createWorkbook()
notes <- tibble::tribble(
  ~Topic, ~Explanation,
  'Scope', 'Static, illustrative acquisition-financing counterfactual for four Canadian airports, not an acquisition offer, forecast of fees, investor IRR or complete cash-flow model.',
  'Source 1', paste0('Financial inputs: ',input_path,'; financial_source_annual and cost_groups_annual (federal_ground_rent), plus Canadian CPI factors.'),
  'Source 2', paste0('Supplemental debt/cash/capex inputs: ',supplement_path,'; first data sheet, skip 3.'),
  'Output location', out_dir,
  'Output 1 (workbook)', 'airport_acquisition_counterfactual_audit.xlsx: two central result sheets, a three-case comparison, two formula audits, and methodology notes. An operating-sensitivity appendix is optional (off by default).',
  'Output 2 (CSV)', 'acquisition_scenarios.csv: all scenarios and annual/pooled periods, with acquisition financing and pre-tax recovery thresholds.',
  'Output 3 (CSV)', 'profitability_scenarios.csv: six ordered thresholds per airport, period and financing scenario; fixed pre-tax recovery and after-tax margin targets, with a common calculation bridge.',
  'Output 4 (CSV)', 'central_acquisition_financing.csv: 20-row acquisition presentation table (including BIG4 and pooled).',
  'Output 5 (CSV)', 'central_profitability_requirements.csv: 120 central-scenario rows; Excel displays 96 individual-airport threshold rows and 24 BIG4 summary rows.',
  'Output 6 (CSV)', 'scenario_comparison.csv: three paired financing cases, for every airport and annual/pooled period.',
  'Output 7 (CSV)', 'acquisition_per_passenger_annual.csv: Canadian 2023-25 2025-CAD-per-passenger scenario series, compatible with GitHub r/per_unit_comparison.R (V10) conventions.',
  'Output 8 (CSV)', 'acquisition_per_passenger_pooled.csv: pooled 2023-25 comparison series, sums inflation-adjusted airport-year flows over passengers.',
  'Output 9 (CSV)', 'scenario_assumptions.csv: scenario names, sale multiples, debt shares and new borrowing rates.',
  'Charts', 'Two PNGs: net additional financing pressure per passenger and required revenue to restore historical profit (2025).',
  'Scenario packages', 'Only three paired cases: low-cost 20x/50%/4.5%, central 22.5x/60%/5.5%, high-cost 25x/70%/6.5%. No factorial combinations or one-at-a-time/interactions. The central case populates the two presentation sheets; all three appear in comparison and audit tabs.',
  'Annual vs pooled audits', 'Within each audit sheet the Annual table appears first and the Pooled table below it, with separate headers and formulas. Pooled source flows reference CPI-adjusted annual inputs, debt and cash use CPI-adjusted averages, and new interest covers three years.',
  'BIG4 treatment', 'Individual airports show all six targets in order, with BIG4 six-target results in a SEPARATE summary. Revenue requirements, pre-tax gaps and taxes are airport-specific calculations that are summed. BIG4 has NO single tax rate, pre-tax equivalent margin, or gap-closing rate.',
  'Valuation', 'Annualized reported EBITDA plus abolished federal ground rent, multiplied by assumed sale-value multiple. Sale value covers the business including existing debt; consideration = sale value - gross debt + cash.',
  'New acquisition financing', 'Existing debt remains outstanding at observed net financing costs. New borrowing is a share of equity purchase consideration. New interest = new debt x rate x years in period. No principal amortization, refinancing or acquisition fees.',
  'Acquisition profit bridge', 'Observed Canadian net income is treated as PRE-TAX income. Post-acquisition pre-tax income = observed pre-tax income + eliminated rent - incremental acquisition interest. Existing financing cost is already included in observed income.',
  'Six independent thresholds', 'In order: (1) break even at zero pre-tax profit; (2) restore observed pre-tax profit, floored at zero; (3-6) achieve TOTAL after-tax margins of 15%, 20%, 25% and 30%. All six start from the same acquisition-adjusted position and are independent, not cumulative.',
  'Private profit targets', 'TOTAL after-tax net margins of 15%, 20%, 25% and 30%, informed by Australian airport observations. They are NOT increases above Canadian historical margins. Each target applies to revenue INCLUDING the needed revenue uplift.',
  'Two target formulas', 'Fixed pre-tax thresholds: target profit = 0 (break-even) or MAX(0, historical pre-tax profit) (restore), then extra revenue = positive pre-tax shortfall / conversion c. Private margin thresholds: total after-tax margin m on NEW revenue requires pre-tax margin q=m/(1-t). Example: 20% / 75% retained = 26.67% pre-tax.',
  'Target at original revenue', 'For break-even T=0; for restoration T=MAX(0, historical pre-tax profit). For net-margin goals T=q x original revenue R: the pre-tax profit needed BEFORE any revenue increase. This is an explanatory step, not the final margin target on expanded revenue.',
  'All six: initial shortfall', 'The current pre-tax profit shortfall is G = MAX(0, T - P), with T the applicable starting profit target and P post-acquisition pre-tax profit. No extra revenue is required to meet an already-satisfied goal.',
  'Why margin goals need more revenue', 'For the first two fixed pre-tax goals, each extra dollar adds c dollars of profit and the target does not change. For margin goals, new revenue raises BOTH actual pre-tax profit by c and required target profit by q. The larger denominator matters only for the margin goals.',
  'Revenue solution', 'In every row, required extra revenue = positive initial pre-tax shortfall / shortfall closed per new dollar. For break-even/restore this denominator is c; for net-margin goals it is c-q. With c=1 and q=0.2667, a new dollar closes 73.33 cents of the original margin gap.',
  'All six: final earnings', 'Additional pre-tax profit = c times additional revenue. Final revenue = R + additional revenue; final pre-tax profit = P + added profit; tax = t x MAX(0, final pre-tax profit); after-tax profit = pre-tax profit - tax. The four NET goals must meet the net-margin target; the two FIXED goals must meet pre-tax profit targets.',
  'Validation', 'For fixed pre-tax targets, verify new pre-tax profit reaches 0 or the nonnegative observed pre-tax goal. For net-margin targets, achieved net margin = after-tax profit / new revenue; equals target if additional revenue is needed, may exceed it otherwise. BIG4 sums separately modelled airports.',
  'Tax assumption', 'General statutory rates: YYC 23%, YUL 26.5%, YVR 27%, YYZ 26.5%. Tax applies to positive accounting pre-tax earnings as a proxy, not actual taxable income. No tax-loss carryforwards, interest deductibility limits, taxable-income adjustments or transaction tax structuring.',
  'Baseline and incremental tax', 'Post-acquisition baseline tax applies before extra revenue; total target tax applies after the uplift; incremental tax is the difference. A modelled liability is not equivalent to a forecast of government tax receipts.',
  'Revenue conversion', 'Default c = 100% of extra revenue becomes additional PRE-TAX profit. This is an optimistic lower bound on required revenue, not a prediction of passenger charges, commercial revenues, growth or operating costs.',
  'Where revenue comes from', 'The model solves a requirement only. Higher aeronautical fees, passenger charges, parking/commercial receipts or traffic growth would have different operating costs and demand effects; those mechanisms are not modelled.',
  'Units and CPI', 'COLUMN HEADERS OMIT UNITS for readability. Model annual results are nominal CAD millions; pooled 2023-25 flows are CPI-adjusted 2025 CAD totals. NEW comparison CSVs express BOTH annual and pooled monetary flows in 2025 CAD and calculate per-passenger amounts using observed passengers. Debt and cash are inflation-adjusted average stocks only in pooled model, not passenger expenses.',
  'Capital spending', 'Cash capex is a separately loaded descriptive input but is not an accounting expense in the profit model and is omitted from the slim audits. Taxes do not imply cash available for capital investment or distributions.',
  'Comparison series', 'The cf_ series are illustrative incremental requirements/financial impacts, not observed cost categories. They should be placed alongside, never added automatically to, the existing accounting cost breakdown. Extra revenue assumes conversion c=100%; the optional operating appendix considers c=80% and cost savings.',
  'Limitations', 'No dynamic demand, tariff setting, equity distributions, operating improvements, transaction structuring, tax-loss use, financing restrictions, principal repayments, depreciation changes or changes to capital requirements.'
)

head_style <- openxlsx::createStyle(fontColour='#FFFFFF',fgFill='#193954',
  textDecoration='bold',halign='center',valign='center',wrapText=TRUE)
head_acquisition <- openxlsx::createStyle(fontColour='#FFFFFF',fgFill='#17576A',
  textDecoration='bold',halign='center',valign='center',wrapText=TRUE)
head_financing <- openxlsx::createStyle(fontColour='#FFFFFF',fgFill='#62523A',
  textDecoration='bold',halign='center',valign='center',wrapText=TRUE)
head_earnings <- openxlsx::createStyle(fontColour='#FFFFFF',fgFill='#365A54',
  textDecoration='bold',halign='center',valign='center',wrapText=TRUE)
head_profit <- openxlsx::createStyle(fontColour='#FFFFFF',fgFill='#59486C',
  textDecoration='bold',halign='center',valign='center',wrapText=TRUE)
head_revenue <- openxlsx::createStyle(fontColour='#FFFFFF',fgFill='#4E5869',
  textDecoration='bold',halign='center',valign='center',wrapText=TRUE)
blue_style <- openxlsx::createStyle(fontColour='#164E96')
num_style <- openxlsx::createStyle(numFmt='#,##0.0;[Red](#,##0.0);"-"')
int_style <- openxlsx::createStyle(numFmt='#,##0;[Red](#,##0);"-"')
pct_style <- openxlsx::createStyle(numFmt='0.0%;[Red](0.0%);"-"')
check_style <- openxlsx::createStyle(numFmt='0.000000%')
mult_style <- openxlsx::createStyle(numFmt='0.0"x"')
cpi_style <- openxlsx::createStyle(numFmt='0.000000')
per_pax_style <- openxlsx::createStyle(numFmt='$0.00;[Red]($0.00);"-"')
per_dollar_style <- openxlsx::createStyle(numFmt='0.000')

# Only the acquisition fields USED in the slimmer presentation and audits.
column_headers <- c(
  scenario_id='Scenario',airport='Airport',year_label='Year / period',
  purchase_multiple='Sale multiple',acquisition_debt_share='New debt share',
  acquisition_interest_rate='Interest rate',
  years_in_period='Years of interest',cpi_factor='CPI factor',
  passengers='Passengers',revenue_m='Observed revenue',
  ebitda_m='Reported EBITDA',ground_rent_m='Federal rent eliminated',
  rent_adjusted_ebitda_m='EBITDA after rent saving',
  valuation_ebitda_m='Annualized EBITDA after rent saving',
  estimated_sale_value_m='Estimated sale value',
  gross_debt_m='Existing gross debt',cash_m='Existing cash',
  equity_purchase_consideration_m='Equity purchase consideration',
  new_acquisition_debt_m='New acquisition borrowing',
  buyer_equity_contribution_m='Buyer equity',
  total_gross_debt_m='Combined gross debt',
  actual_total_gross_debt_to_sale_value='Gross debt / sale value',
  net_financing_cost_m='Existing net financing costs',
  additional_acquisition_interest_m='New acquisition interest',
  combined_net_financing_cost_proxy_m='Combined net financing costs',
  purchase_adjusted_ebtda_m='Acquisition-adjusted EBTDA',
  net_income_m='Observed pre-tax profit',
  rent_adjusted_pre_tax_income_m='Pre-tax profit after rent saving',
  purchase_adjusted_pre_tax_income_m='Acquisition-adjusted pre-tax profit',
  purchase_adjusted_pre_tax_margin_pct='Acquisition-adjusted pre-tax margin'
)
stopifnot(all(acquisition_audit_cols %in% names(column_headers)))

# ---- 9. CENTRAL RESULTS: ACQUISITION FINANCING AND PROFITABILITY -----------
# Separate two exercises: (1) the acquisition's financing and PRE-TAX
# earnings consequences, and (2) the additional revenue and tax implications
# of reaching independently defined AFTER-TAX margins. Both show only the
# central case; the three-case audit tables retain every financing scenario.
# central_id was constructed directly from the scenario input table.
stopifnot(nrow(central) == 20L, all(central$scenario_id == central_id))

# Consistent airport ordering and emphasis across the two presentation sheets.
arrange_central <- function(d) {
  d |>
    arrange(factor(airport, levels = c(can_airports, 'BIG4')),
            factor(year_label, levels = c(as.character(years), '2023-25')))
}
style_airport_periods <- function(sheet, d, first_row, ncols) {
  pooled <- which(d[['Year / period']] == '2023-25') + first_row - 1L
  next_airport <- which(d[['Airport']] !=
    dplyr::lag(d[['Airport']], default = d[['Airport']][1])) + first_row - 1L
  big4 <- which(d[['Airport']] == 'BIG4') + first_row - 1L
  if (length(pooled)) {
    openxlsx::addStyle(wb, sheet, openxlsx::createStyle(textDecoration = 'bold'),
      rows = pooled, cols = seq_len(ncols), gridExpand = TRUE, stack = TRUE)
  }
  if (length(big4)) {
    openxlsx::addStyle(wb, sheet,
      openxlsx::createStyle(textDecoration = 'bold', fontColour = '#193954'),
      rows = big4, cols = seq_len(ncols), gridExpand = TRUE, stack = TRUE)
  }
  if (length(next_airport)) {
    openxlsx::addStyle(wb, sheet,
      openxlsx::createStyle(border = 'top', borderColour = '#8695A4'),
      rows = next_airport, cols = seq_len(ncols), gridExpand = TRUE, stack = TRUE)
  }
}

# Sheet 1: keep the existing acquisition valuation / interest / pre-tax bridge.
# Profit-target requirements and illustrative corporate taxes appear on Sheet 2.
results_cols <- c(
  'scenario_id','airport','year_label',
  'revenue_m','ebitda_m','ground_rent_m','rent_adjusted_ebitda_m',
  'valuation_ebitda_m','estimated_sale_value_m',
  'gross_debt_m','cash_m','equity_purchase_consideration_m',
  'new_acquisition_debt_m','buyer_equity_contribution_m',
  'net_financing_cost_m','additional_acquisition_interest_m',
  'combined_net_financing_cost_proxy_m',
  'purchase_adjusted_ebtda_m',
  'net_income_m','rent_adjusted_pre_tax_income_m',
  'purchase_adjusted_pre_tax_income_m'
)
stopifnot(length(results_cols) == 21L, all(results_cols %in% names(model)),
          !'estimated_tax_purchase_adjusted_m' %in% results_cols)
results <- arrange_central(central) |> select(all_of(results_cols))
stopifnot(nrow(results) == 20L,
          !anyDuplicated(results[c('airport','year_label')]))
readr::write_csv(results, file.path(out_dir,'central_acquisition_financing.csv'))
results_headers <- column_headers[results_cols]
results_header_overrides <- c(
  ebitda_m = 'Reported EBITDA',
  ground_rent_m = 'Federal rent eliminated',
  rent_adjusted_ebitda_m = 'EBITDA after rent saving',
  valuation_ebitda_m = 'Annualized EBITDA after rent saving',
  net_financing_cost_m = 'Existing net financing costs',
  additional_acquisition_interest_m = 'New acquisition interest',
  combined_net_financing_cost_proxy_m = 'Total net financing costs',
  purchase_adjusted_ebtda_m = 'Acquisition-adjusted EBTDA',
  net_income_m = 'Observed pre-tax profit',
  rent_adjusted_pre_tax_income_m = 'Pre-tax profit after rent saving',
  purchase_adjusted_pre_tax_income_m = 'Acquisition-adjusted pre-tax profit'
)
results_headers[names(results_header_overrides)] <- results_header_overrides
stopifnot(!anyDuplicated(unname(results_headers)))
names(results) <- unname(results_headers)
results_sheet <- 'Acquisition financing'
openxlsx::addWorksheet(wb, results_sheet, gridLines = FALSE)
res_ncol <- length(results_cols)
res_data_rows <- 5L:(nrow(results) + 4L)
for (r in 1:3) openxlsx::mergeCells(wb, results_sheet, cols = 1:res_ncol, rows = r)
openxlsx::writeData(wb, results_sheet,
  'Airport acquisition counterfactual | acquisition financing (central scenario)', startRow = 1)
openxlsx::writeData(wb, results_sheet,
  '22.5x EBITDA | 60% new borrowing | 5.5% interest | CAD millions | Annual: nominal; pooled 2023-25: 2025 CAD totals',
  startRow = 2)
openxlsx::writeData(wb, results_sheet,
  'Acquisition-adjusted EBTDA = reported EBITDA + eliminated rent - existing net financing - new acquisition interest. Pre-tax results; profit targets and taxes on next sheet.',
  startRow = 3)
openxlsx::writeData(wb, results_sheet, results, startRow = 4, withFilter = TRUE)
openxlsx::setColWidths(wb, results_sheet, cols = 1, widths = 22)
openxlsx::setColWidths(wb, results_sheet, cols = 2:3, widths = 13)
openxlsx::setColWidths(wb, results_sheet, cols = 4:res_ncol, widths = 19)
openxlsx::setColWidths(wb, results_sheet,
  cols = match(c('equity_purchase_consideration_m','valuation_ebitda_m'),results_cols),
  widths = 22)
openxlsx::setRowHeights(wb, results_sheet, rows = 1:3, heights = c(29,24,24))
openxlsx::setRowHeights(wb, results_sheet, rows = 4, heights = 55)
openxlsx::addStyle(wb, results_sheet, head_style,
  rows = 4, cols = 1:res_ncol, gridExpand = TRUE)
results_header_groups <- list(
  acquisition = c('revenue_m','ebitda_m','ground_rent_m',
     'rent_adjusted_ebitda_m','valuation_ebitda_m','estimated_sale_value_m',
     'gross_debt_m','cash_m','equity_purchase_consideration_m',
     'new_acquisition_debt_m','buyer_equity_contribution_m'),
  financing = c('net_financing_cost_m','additional_acquisition_interest_m',
     'combined_net_financing_cost_proxy_m'),
  earnings = c('purchase_adjusted_ebtda_m',
     'net_income_m','rent_adjusted_pre_tax_income_m',
     'purchase_adjusted_pre_tax_income_m')
)
stopifnot(setequal(unlist(results_header_groups,use.names=FALSE),results_cols[-(1:3)]))
for (group in names(results_header_groups)) {
  stage_style <- switch(group, acquisition=head_acquisition,
    financing=head_financing, earnings=head_earnings)
  openxlsx::addStyle(wb, results_sheet, stage_style, rows = 4,
    cols = match(results_header_groups[[group]],results_cols), gridExpand = TRUE)
}
openxlsx::addStyle(wb, results_sheet, num_style,
  rows = res_data_rows, cols = 4:res_ncol, gridExpand = TRUE)
style_airport_periods(results_sheet, results, first_row = 5L, ncols = res_ncol)
openxlsx::addStyle(wb, results_sheet,
  openxlsx::createStyle(fontSize=15,textDecoration='bold',fontColour='#193954'),
  rows=1,cols=1)
openxlsx::addStyle(wb, results_sheet,
  openxlsx::createStyle(fontSize=10,fontColour='#445569'),
  rows=2,cols=1)
openxlsx::addStyle(wb, results_sheet,
  openxlsx::createStyle(fontSize=10,fontColour='#445569',textDecoration='italic'),
  rows=3,cols=1)
openxlsx::freezePane(wb, results_sheet, firstActiveRow = 5, firstActiveCol = 4)

# Sheet 2: ONE ordered six-threshold profitability analysis for each airport
# and year. The first two goals use a FIXED pre-tax target. The last four
# target AFTER-tax margins on counterfactual revenue. They are independent,
# not cumulative. Main table preserves the calculation sequence; a separate
# BIG4 summary sums the already-solved airport-specific requirements.
profitability <- central_scenario(profitability_model) |>
  arrange(factor(airport,levels=c(can_airports,'BIG4')),
          factor(year_label,levels=c(as.character(years),'2023-25')),
          threshold_order) |>
  transmute(
    Airport=airport,
    `Year / period`=year_label,
    `Profitability objective`=profitability_objective,
    `Target method`=target_type,
    `Observed revenue`=revenue_m,
    `Post-acquisition pre-tax profit`=purchase_adjusted_pre_tax_income_m,
    `Historical pre-tax profit`=net_income_m,
    `Target after-tax margin`=target_net_margin,
    `Statutory tax rate`=statutory_tax_rate,
    `Pre-tax margin needed to achieve net target`=required_pre_tax_margin,
    `Pre-tax target on existing revenue`=pre_tax_profit_target_at_existing_revenue_m,
    `Initial pre-tax profit shortfall`=initial_pre_tax_shortfall_m,
    `Share of extra revenue becoming pre-tax profit`=incremental_revenue_to_profit,
    `Shortfall closed per extra dollar`=shortfall_closed_per_new_revenue_dollar,
    `Additional revenue required`=required_revenue_m,
    `Additional revenue per passenger`=required_revenue_per_pax_cad,
    `New total revenue`=counterfactual_revenue_m,
    `New pre-tax profit`=counterfactual_pre_tax_income_m,
    `Estimated corporate tax`=estimated_tax_m,
    `New after-tax profit`=counterfactual_after_tax_income_m,
    `Achieved after-tax margin`=achieved_net_margin
  )
stopifnot(nrow(profitability)==120L,
  !anyDuplicated(profitability[c('Airport','Year / period',
                                'Profitability objective')]),
  all(profitability[['Achieved after-tax margin']][
      profitability[['Target method']]=='After-tax margin'] + 1e-9 >=
      profitability[['Target after-tax margin']][
      profitability[['Target method']]=='After-tax margin']))
readr::write_csv(profitability,
  file.path(out_dir,'central_profitability_requirements.csv'))

profit_individual <- profitability |> filter(Airport!='BIG4')
profit_big4 <- profitability |>
  filter(Airport=='BIG4') |>
  select(
    Airport,`Year / period`,`Profitability objective`,`Target method`,
    `Observed revenue`,`Post-acquisition pre-tax profit`,`Historical pre-tax profit`,
    `Pre-tax target on existing revenue`,`Initial pre-tax profit shortfall`,
    `Additional revenue required`,`Additional revenue per passenger`,
    `New total revenue`,`New pre-tax profit`,`Estimated corporate tax`,
    `New after-tax profit`,`Achieved after-tax margin`
  )
stopifnot(nrow(profit_individual)==96L,nrow(profit_big4)==24L,
  all(is.na(profitability[['Statutory tax rate']][profitability$Airport=='BIG4'])),
  all(is.na(profitability[['Pre-tax margin needed to achieve net target']][
    profitability[['Target method']]=='Fixed pre-tax'])),
  all(is.na(profitability[['Target after-tax margin']][
    profitability[['Target method']]=='Fixed pre-tax'])),
  all(is.na(profitability[['Shortfall closed per extra dollar']][
    profitability$Airport=='BIG4'])))

profit_sheet <- 'Profitability requirements'
openxlsx::addWorksheet(wb,profit_sheet,gridLines=FALSE)
prof_cols <- ncol(profit_individual)
# A reader sees BOTH fixed pre-tax recovery thresholds before the four
# after-tax profit objectives, for the same airport and observation period.
intro <- c(
  'Airport profitability requirements | six independent targets, central acquisition scenario',
  'Annual figures: nominal CAD millions; pooled 2023-25: total 2025 CAD millions. Per-passenger amounts: CAD. Currency units are omitted from headers.',
  '1-2. First recover a nonnegative pre-tax result (break even), then restore the historical pre-tax profit, with a zero floor for an observed loss.',
  '3-6. Then test 15%, 20%, 25% and 30% AFTER-TAX margins. Divide the net target by (1 - tax rate) to obtain the corresponding pre-tax margin.',
  'All goals: compare the pre-tax target at EXISTING revenue with post-acquisition profit. Fixed targets stay fixed; margin targets rise as new revenue expands the denominator.',
  'Extra revenue = initial pre-tax shortfall / shortfall closed per additional dollar. The 100% revenue-to-profit assumption is illustrative; sources of extra revenue are NOT forecast. Goals are not cumulative.'
)
for(r in seq_along(intro)) {
  openxlsx::mergeCells(wb,profit_sheet,cols=1:12,rows=r)
  openxlsx::writeData(wb,profit_sheet,intro[[r]],startRow=r)
}
openxlsx::setRowHeights(wb,profit_sheet,rows=1:6,heights=c(32,30,33,33,36,36))
openxlsx::addStyle(wb,profit_sheet,
  openxlsx::createStyle(fontSize=15,textDecoration='bold',fontColour='#193954'),
  rows=1,cols=1)
openxlsx::addStyle(wb,profit_sheet,
  openxlsx::createStyle(fontSize=10,fontColour='#445569',wrapText=TRUE,valign='center'),
  rows=2:6,cols=1,gridExpand=TRUE)

prof_stage_row <- 7L
prof_header_row <- 8L
prof_first_row <- prof_header_row+1L
prof_last_row <- prof_first_row+nrow(profit_individual)-1L
prof_groups <- list(
  list(first=1L,last=4L,label='AIRPORT AND OBJECTIVE',style=head_style),
  list(first=5L,last=7L,label='STARTING FINANCES',style=head_acquisition),
  list(first=8L,last=12L,label='TARGET PRE-TAX PROFIT AND INITIAL GAP',style=head_earnings),
  list(first=13L,last=16L,label='HOW MUCH NEW REVENUE?',style=head_revenue),
  list(first=17L,last=21L,label='RESULTING EARNINGS AND TAX',style=head_profit)
)
stopifnot(identical(unlist(lapply(prof_groups,
  function(g)seq.int(g$first,g$last))),seq_len(prof_cols)))
for(g in prof_groups) {
  openxlsx::mergeCells(wb,profit_sheet,cols=g$first:g$last,rows=prof_stage_row)
  openxlsx::writeData(wb,profit_sheet,g$label,startRow=prof_stage_row,startCol=g$first)
  openxlsx::addStyle(wb,profit_sheet,g$style,rows=prof_stage_row,
    cols=g$first:g$last,gridExpand=TRUE)
}
openxlsx::setRowHeights(wb,profit_sheet,rows=prof_stage_row,heights=24)
openxlsx::writeData(wb,profit_sheet,profit_individual,
  startRow=prof_header_row,withFilter=TRUE)
openxlsx::addStyle(wb,profit_sheet,head_style,rows=prof_header_row,
  cols=1:prof_cols,gridExpand=TRUE)
for(g in prof_groups) openxlsx::addStyle(wb,profit_sheet,g$style,
  rows=prof_header_row,cols=g$first:g$last,gridExpand=TRUE)
openxlsx::setRowHeights(wb,profit_sheet,rows=prof_header_row,heights=67)
openxlsx::setColWidths(wb,profit_sheet,cols=1:2,widths=c(12,14))
openxlsx::setColWidths(wb,profit_sheet,cols=3:4,widths=c(28,18))
openxlsx::setColWidths(wb,profit_sheet,cols=5:prof_cols,widths=20)
openxlsx::setColWidths(wb,profit_sheet,cols=c(6,7,11,12,13,14),widths=24)
prof_rows <- prof_first_row:prof_last_row
openxlsx::addStyle(wb,profit_sheet,num_style,rows=prof_rows,
  cols=5:prof_cols,gridExpand=TRUE)
openxlsx::addStyle(wb,profit_sheet,pct_style,rows=prof_rows,
  cols=c(8,9,10,13,21),gridExpand=TRUE,stack=TRUE)
openxlsx::addStyle(wb,profit_sheet,per_dollar_style,rows=prof_rows,
  cols=14,gridExpand=TRUE,stack=TRUE)
openxlsx::addStyle(wb,profit_sheet,per_pax_style,rows=prof_rows,
  cols=16,gridExpand=TRUE,stack=TRUE)
style_airport_periods(profit_sheet,profit_individual,
  first_row=prof_first_row,ncols=prof_cols)
# One boundary per COMPLETE six-objective airport-period block. Previously a
# rule before objective 3 split each six-row group visually into 2 + 4 rows.
# Boundaries start at the first objective of the NEXT airport-period, not
# within the six independent objectives of any one airport-period.
profit_group_starts <- which(
  profit_individual[['Profitability objective']] ==
    profit_individual[['Profitability objective']][1L] &
  (profit_individual[['Year / period']] !=
     dplyr::lag(profit_individual[['Year / period']],
                default=profit_individual[['Year / period']][1L]) |
   profit_individual[['Airport']] !=
     dplyr::lag(profit_individual[['Airport']],
                default=profit_individual[['Airport']][1L])))
stopifnot(nrow(profit_individual) %% 6L == 0L,
          identical(as.integer(profit_group_starts),
                    as.integer(seq.int(7L,nrow(profit_individual),by=6L))))
openxlsx::addStyle(wb,profit_sheet,
  openxlsx::createStyle(border='top',borderColour='#8695A4'),
  rows=prof_first_row+profit_group_starts-1L,
  cols=1:prof_cols,gridExpand=TRUE,stack=TRUE)

# BIG4 shows all six objectives too. Target/tax rates are deliberately absent,
# as its tax and revenue requirements are SUMS of independent airport models.
sum_title <- prof_last_row+4L
sum_note <- sum_title+1L
sum_header <- sum_title+2L
sum_first <- sum_header+1L
sum_last <- sum_first+nrow(profit_big4)-1L
for(r in c(sum_title,sum_note))openxlsx::mergeCells(wb,profit_sheet,
  cols=1:12,rows=r)
openxlsx::writeData(wb,profit_sheet,
  'BIG4 COMBINED SUMMARY | all six goals, calculated separately by airport',
  startRow=sum_title)
openxlsx::writeData(wb,profit_sheet,
  'BIG4 targets, positive shortfalls, extra revenue and taxes sum four separate airport results. No single combined statutory tax or net-margin conversion is assumed.',
  startRow=sum_note)
openxlsx::addStyle(wb,profit_sheet,
  openxlsx::createStyle(fontColour='#FFFFFF',fgFill='#193954',
                        textDecoration='bold',fontSize=11),
  rows=sum_title,cols=1)
openxlsx::setRowHeights(wb,profit_sheet,
  rows=c(sum_title,sum_note),heights=c(29,28))
openxlsx::writeData(wb,profit_sheet,profit_big4,
  startRow=sum_header,withFilter=FALSE)
openxlsx::addStyle(wb,profit_sheet,head_profit,rows=sum_header,
  cols=seq_len(ncol(profit_big4)),gridExpand=TRUE)
openxlsx::setRowHeights(wb,profit_sheet,rows=sum_header,heights=65)
openxlsx::addStyle(wb,profit_sheet,num_style,rows=sum_first:sum_last,
  cols=5:ncol(profit_big4),gridExpand=TRUE)
openxlsx::addStyle(wb,profit_sheet,pct_style,rows=sum_first:sum_last,
  cols=16,gridExpand=TRUE,stack=TRUE)
openxlsx::addStyle(wb,profit_sheet,per_pax_style,rows=sum_first:sum_last,
  cols=11,gridExpand=TRUE,stack=TRUE)
style_airport_periods(profit_sheet,profit_big4,
  first_row=sum_first,ncols=ncol(profit_big4))
# Apply the same six-row boundaries to the separate BIG4 summary.
stopifnot(nrow(profit_big4) %% 6L == 0L)
openxlsx::addStyle(wb,profit_sheet,
  openxlsx::createStyle(border='top',borderColour='#8695A4'),
  rows=sum_first+seq.int(6L,nrow(profit_big4)-1L,by=6L),
  cols=seq_len(ncol(profit_big4)),gridExpand=TRUE,stack=TRUE)
openxlsx::freezePane(wb,profit_sheet,firstActiveRow=prof_first_row,
  firstActiveCol=5)

# These are three coherent acquisition packages, not independently ranked
# values per outcome. Values shown here come from the full validated model.
period_levels <- c(as.character(years), '2023-25')
metric_labels <- c(
  additional_acquisition_interest_m = 'Additional acquisition interest',
  purchase_adjusted_pre_tax_income_m = 'Adjusted pre-tax income',
  required_revenue_restore_per_pax_cad = 'Restore revenue per passenger',
  required_revenue_net_15pct_per_pax_cad = '15% net revenue per passenger',
  required_revenue_net_20pct_per_pax_cad = '20% net revenue per passenger',
  required_revenue_net_25pct_per_pax_cad = '25% net revenue per passenger',
  required_revenue_net_30pct_per_pax_cad = '30% net revenue per passenger'
)
arrange_comparison <- function(d) {
  d |>
    arrange(factor(airport, levels = c(can_airports, 'BIG4')),
            factor(period, levels = period_levels),
            factor(metric, levels = unname(metric_labels)))
}
sensitivity_long <- model |>
  select(airport, period = year_label, scenario_id, scenario_name,
         purchase_multiple, acquisition_debt_share, acquisition_interest_rate,
         all_of(names(metric_labels))) |>
  pivot_longer(all_of(names(metric_labels)), names_to = 'metric',
               values_to = 'value') |>
  mutate(metric = unname(metric_labels[metric]))
scenario_ranges <- sensitivity_long |>
  select(airport, period, metric, scenario_name, value) |>
  pivot_wider(names_from = scenario_name, values_from = value) |>
  rename(low_cost = `Low-cost`, central = Central, high_cost = `High-cost`) |>
  arrange_comparison()
stopifnot(nrow(scenario_ranges) == 140L,
          !anyNA(scenario_ranges[c('low_cost','central','high_cost')]))
interest_rows <- scenario_ranges$metric == 'Additional acquisition interest'
pretax_rows <- scenario_ranges$metric == 'Adjusted pre-tax income'
stopifnot(all(scenario_ranges$low_cost[interest_rows] <=
              scenario_ranges$central[interest_rows] + 1e-8),
          all(scenario_ranges$central[interest_rows] <=
              scenario_ranges$high_cost[interest_rows] + 1e-8),
          all(scenario_ranges$low_cost[pretax_rows] + 1e-8 >=
              scenario_ranges$central[pretax_rows]),
          all(scenario_ranges$central[pretax_rows] + 1e-8 >=
              scenario_ranges$high_cost[pretax_rows]))
readr::write_csv(scenario_ranges, file.path(out_dir, 'scenario_comparison.csv'))
readr::write_csv(scenarios, file.path(out_dir, 'scenario_assumptions.csv'))

comparison_sheet <- 'Scenario comparison'
openxlsx::addWorksheet(wb, comparison_sheet, gridLines = FALSE)
openxlsx::mergeCells(wb, comparison_sheet, cols = 1:7, rows = 1)
openxlsx::mergeCells(wb, comparison_sheet, cols = 1:7, rows = 2)
openxlsx::writeData(wb, comparison_sheet,
  'Acquisition financing | three paired scenarios', startRow = 1)
openxlsx::writeData(wb, comparison_sheet,
  '2025 snapshots and pooled 2023–25 results. Monetary totals: CAD millions; per passenger: CAD. Annual: nominal; pooled: 2025 CAD.',
  startRow = 2)
openxlsx::addStyle(wb, comparison_sheet,
  openxlsx::createStyle(fontSize = 15, textDecoration = 'bold',
                        fontColour = '#193954'), rows = 1, cols = 1)
openxlsx::addStyle(wb, comparison_sheet,
  openxlsx::createStyle(fontSize = 10, fontColour = '#445569'),
  rows = 2, cols = 1)
openxlsx::setColWidths(wb, comparison_sheet, cols = 1:7,
  widths = c(13, 17, 36, 21, 21, 21, 19))
openxlsx::setRowHeights(wb, comparison_sheet, rows = 1:2,
  heights = c(30, 24))
scenario_display <- scenarios |>
  select(scenario_name, purchase_multiple, acquisition_debt_share,
         acquisition_interest_rate)
names(scenario_display) <- c('Scenario', 'Sale multiple',
                             'New debt share', 'Borrowing rate')
openxlsx::writeData(wb, comparison_sheet, scenario_display,
                    startRow = 4, withFilter = FALSE)
openxlsx::addStyle(wb, comparison_sheet, head_acquisition,
                    rows = 4, cols = 1:4, gridExpand = TRUE)
openxlsx::addStyle(wb, comparison_sheet, mult_style,
                    rows = 5:7, cols = 2, gridExpand = TRUE)
openxlsx::addStyle(wb, comparison_sheet, pct_style,
                    rows = 5:7, cols = 3:4, gridExpand = TRUE)

# Keep the readable 2025 snapshot first; show the pooled 2023-25 historical
# comparison separately. Earlier annual snapshots remain in both audit tabs
# and the scenario_comparison.csv export.
write_three_case_block <- function(d, title, start_row) {
  openxlsx::mergeCells(wb, comparison_sheet, cols = 1:6, rows = start_row)
  openxlsx::writeData(wb, comparison_sheet, title, startRow = start_row)
  openxlsx::addStyle(wb, comparison_sheet,
    openxlsx::createStyle(fontColour = '#FFFFFF', fgFill = '#193954',
                          textDecoration = 'bold', fontSize = 11),
    rows = start_row, cols = 1)
  disp <- d |> select(airport, period, metric, low_cost, central, high_cost)
  names(disp) <- c('Airport', 'Period', 'Metric',
                   'Low-cost', 'Central', 'High-cost')
  openxlsx::writeData(wb, comparison_sheet, disp,
                      startRow = start_row + 1L, withFilter = FALSE)
  openxlsx::addStyle(wb, comparison_sheet, head_earnings,
                    rows = start_row + 1L, cols = 1:6, gridExpand = TRUE)
  openxlsx::addStyle(wb, comparison_sheet, num_style,
                    rows = (start_row + 2L):(start_row + nrow(disp) + 1L),
                    cols = 4:6, gridExpand = TRUE)
  pp_rows <- which(grepl('per passenger', disp$Metric, fixed = TRUE)) +
             start_row + 1L
  if (length(pp_rows)) openxlsx::addStyle(wb, comparison_sheet,
      per_pax_style, rows = pp_rows, cols = 4:6,
      gridExpand = TRUE, stack = TRUE)
  start_row + nrow(disp) + 4L
}
next_row <- write_three_case_block(
  scenario_ranges |> filter(period == '2025'),
  '2025 purchase counterfactual | current-year comparison', 10L)
next_row <- write_three_case_block(
  scenario_ranges |> filter(period == '2023-25'),
  '2023–25 historical pooled sensitivity | inflation-adjusted totals', next_row)
openxlsx::freezePane(wb, comparison_sheet,
                     firstActiveRow = 12, firstActiveCol = 4)

# ---- 11. TWO FORMULA AUDITS: SEPARATE ANNUAL AND POOLED TABLES ------------
# Audit purpose, not period, determines the worksheet. Within EACH audit
# sheet the Annual and Pooled tables are distinct named Excel tables, with
# independent headers and formula generation. Every derived audit result is
# an Excel formula. Only observed/scenario inputs are hard-coded.
# No explanatory EBTDA margins and no repeated 15/20/25/30% column blocks.

create_audit_sheet <- function(sheet,title,subtitle,ncols) {
  openxlsx::addWorksheet(wb,sheet,gridLines=FALSE)
  for (r in 1:2) openxlsx::mergeCells(wb,sheet,cols=seq_len(ncols),rows=r)
  openxlsx::writeData(wb,sheet,title,startRow=1)
  openxlsx::writeData(wb,sheet,subtitle,startRow=2)
  openxlsx::addStyle(wb,sheet,
    openxlsx::createStyle(fontSize=15,textDecoration='bold',fontColour='#193954'),
    rows=1,cols=1)
  openxlsx::addStyle(wb,sheet,
    openxlsx::createStyle(fontSize=10,fontColour='#445569'),rows=2,cols=1)
  openxlsx::setRowHeights(wb,sheet,rows=1:2,heights=c(29,24))
  openxlsx::freezePane(wb,sheet,firstActiveRow=6,firstActiveCol=4)
}

# A distinct stage band appears above each audit-table header. Stage bands
# are instructions for reading the table, not extra calculated fields.
write_audit_table <- function(sheet,source,columns,headers,title,title_row,
                              table_name,pct_names=character(),
                              pax_names=character(),input_names=character(),
                              stage_groups=list()) {
  unique_keys <- c('scenario_id','airport','year_label')
  if ('threshold_id' %in% columns) unique_keys <- c(unique_keys,'threshold_id')
  stopifnot(all(columns %in% names(source)),
    length(headers)==length(columns),!anyDuplicated(headers),
    !anyDuplicated(source[unique_keys]))
  stage_row <- title_row+1L
  header_row <- stage_row+1L
  first_data_row <- header_row+1L
  d <- source |> select(all_of(columns))
  names(d) <- unname(headers)
  openxlsx::mergeCells(wb,sheet,cols=seq_along(columns),rows=title_row)
  openxlsx::writeData(wb,sheet,title,startRow=title_row)
  openxlsx::addStyle(wb,sheet,
    openxlsx::createStyle(fontColour='#FFFFFF',fgFill='#193954',
                          textDecoration='bold',fontSize=11),
    rows=title_row,cols=1)
  openxlsx::setRowHeights(wb,sheet,rows=title_row,heights=28)
  if(length(stage_groups)) {
    covered <- unlist(lapply(stage_groups,function(g)seq.int(g$first,g$last)))
    stopifnot(identical(covered,seq_along(columns)))
    for(g in stage_groups) {
      openxlsx::mergeCells(wb,sheet,cols=g$first:g$last,rows=stage_row)
      openxlsx::writeData(wb,sheet,g$label,startRow=stage_row,startCol=g$first)
      openxlsx::addStyle(wb,sheet,g$style,rows=stage_row,
        cols=g$first:g$last,gridExpand=TRUE)
    }
    openxlsx::setRowHeights(wb,sheet,rows=stage_row,heights=25)
  }
  # Each period is a separate filterable Excel table with its own formulas.
  openxlsx::writeDataTable(wb,sheet,d,startRow=header_row,
    tableName=table_name,tableStyle='TableStyleMedium2',withFilter=TRUE)
  openxlsx::addStyle(wb,sheet,head_style,rows=header_row,
    cols=seq_along(columns),gridExpand=TRUE)
  openxlsx::setRowHeights(wb,sheet,rows=header_row,heights=62)
  openxlsx::setColWidths(wb,sheet,cols=1,widths=23)
  openxlsx::setColWidths(wb,sheet,cols=2:3,widths=13)
  openxlsx::setColWidths(wb,sheet,cols=4:length(columns),widths=22)
  if ('threshold_id' %in% columns)
    openxlsx::setColWidths(wb,sheet,cols=match('threshold_id',columns),widths=17)
  if ('profitability_objective' %in% columns)
    openxlsx::setColWidths(wb,sheet,cols=match('profitability_objective',columns),widths=31)
  if ('target_type' %in% columns)
    openxlsx::setColWidths(wb,sheet,cols=match('target_type',columns),widths=19)
  openxlsx::addStyle(wb,sheet,num_style,
    rows=first_data_row:(first_data_row+nrow(d)-1L),
    cols=4:length(columns),gridExpand=TRUE)
  if (length(pct_names)) openxlsx::addStyle(wb,sheet,pct_style,
    rows=first_data_row:(first_data_row+nrow(d)-1L),
    cols=match(pct_names,columns),gridExpand=TRUE,stack=TRUE)
  if (length(pax_names)) openxlsx::addStyle(wb,sheet,per_pax_style,
    rows=first_data_row:(first_data_row+nrow(d)-1L),
    cols=match(pax_names,columns),gridExpand=TRUE,stack=TRUE)
  if (length(input_names)) openxlsx::addStyle(wb,sheet,blue_style,
    rows=first_data_row:(first_data_row+nrow(d)-1L),
    cols=match(input_names,columns),gridExpand=TRUE,stack=TRUE)
  # Aggregated BIG4 rows are visibly different; blank explanatory rates in
  # those rows are intentional, since no single tax / gap rate exists.
  big4_rows <- first_data_row + which(source$airport=='BIG4') - 1L
  if(length(big4_rows)) openxlsx::addStyle(wb,sheet,
    openxlsx::createStyle(textDecoration='bold',fontColour='#193954'),
    rows=big4_rows,cols=seq_along(columns),gridExpand=TRUE,stack=TRUE)
  list(data=source,first_row=first_data_row,
       header_row=header_row,last_row=first_data_row+nrow(d)-1L,
       row_ids=first_data_row+seq_len(nrow(d))-1L,
       columns=columns)
}

acq_audit_sheet <- 'Acquisition audit'
create_audit_sheet(acq_audit_sheet,
  'Acquisition audit | valuation, debt and post-acquisition earnings',
  'Two separate, formula-driven tables: annual first, pooled second. Pooled inputs link to CPI-adjusted annual rows. Three financing scenarios.',
  length(acquisition_audit_cols))

acq_audit_headers <- column_headers[acquisition_audit_cols]
acq_pct <- c('acquisition_debt_share','acquisition_interest_rate',
  'actual_total_gross_debt_to_sale_value','purchase_adjusted_pre_tax_margin_pct')
acq_fixed_inputs <- c('purchase_multiple','acquisition_debt_share',
  'acquisition_interest_rate','years_in_period','cpi_factor',
  acquisition_source_cols)

# Preserve five-airport order within each scenario-year: the BIG4 row
# follows YYC, YUL, YVR and YYZ, making sums easy to inspect in Excel.
acq_annual <- model |>
  filter(period=='Annual') |>
  arrange(scenario_id,year_label,factor(airport,levels=c(can_airports,'BIG4')))
acq_pooled <- model |>
  filter(period=='Pooled') |>
  arrange(scenario_id,year_label,factor(airport,levels=c(can_airports,'BIG4')))
acq_stages <- list(
  list(first=1L,last=3L,label='SCENARIO / PERIOD',style=head_style),
  list(first=4L,last=11L,label='ASSUMPTIONS AND OBSERVED FINANCES',style=head_acquisition),
  list(first=12L,last=22L,label='VALUATION AND PURCHASE FUNDING',style=head_financing),
  list(first=23L,last=26L,label='FINANCING COSTS AND EBTDA',style=head_revenue),
  list(first=27L,last=30L,label='PRE-TAX PROFIT BRIDGE',style=head_earnings)
)
acq_a <- write_audit_table(acq_audit_sheet,acq_annual,acquisition_audit_cols,
  acq_audit_headers,'ANNUAL | independent yearly acquisition calculations',
  title_row=4L,table_name='AuditAcqAnnual',pct_names=acq_pct,
  input_names=acq_fixed_inputs,stage_groups=acq_stages)
acq_p <- write_audit_table(acq_audit_sheet,acq_pooled,acquisition_audit_cols,
  acq_audit_headers,'POOLED 2023-25 | CPI-adjusted cash-flow totals, mean debt/cash stocks, three years of interest',
  title_row=acq_a$last_row+7L,table_name='AuditAcqPooled',
  pct_names=acq_pct,input_names=acq_fixed_inputs,stage_groups=acq_stages)

# Lookup the annual acquisition rows for the pooled source formulas.
acq_annual_key <- paste(acq_annual$scenario_id,acq_annual$airport,
                        acq_annual$year_label)
acq_audit_key <- function(which_period) {
  tab <- if(which_period=='Annual') acq_a else acq_p
  d <- tab$data
  setNames(tab$row_ids,paste(d$scenario_id,d$airport,d$year_label))
}
acq_row_lookup <- c(acq_audit_key('Annual'),acq_audit_key('Pooled'))
acq_excel_col <- function(nm) {
  stopifnot(nm %in% acquisition_audit_cols)
  openxlsx::int2col(match(nm,acquisition_audit_cols))
}

# Build formula strings with REFERENCES on the row being audited.
# Formula bridge uses rent-adjusted EBITDA minus combined financing for EBTDA.
acq_row_formulas <- function(r) {
  ref <- function(nm) paste0(acq_excel_col(nm),r)
  c(
    rent_adjusted_ebitda_m=sprintf('%s+%s',ref('ebitda_m'),ref('ground_rent_m')),
    valuation_ebitda_m=sprintf('%s/%s',ref('rent_adjusted_ebitda_m'),ref('years_in_period')),
    estimated_sale_value_m=sprintf('%s*%s',ref('valuation_ebitda_m'),ref('purchase_multiple')),
    equity_purchase_consideration_m=sprintf('%s-%s+%s',
      ref('estimated_sale_value_m'),ref('gross_debt_m'),ref('cash_m')),
    new_acquisition_debt_m=sprintf('IF(%s>0,%s*%s,NA())',
      ref('equity_purchase_consideration_m'),ref('equity_purchase_consideration_m'),
      ref('acquisition_debt_share')),
    buyer_equity_contribution_m=sprintf('%s-%s',
      ref('equity_purchase_consideration_m'),ref('new_acquisition_debt_m')),
    total_gross_debt_m=sprintf('%s+%s',ref('gross_debt_m'),ref('new_acquisition_debt_m')),
    actual_total_gross_debt_to_sale_value=sprintf('%s/%s',
      ref('total_gross_debt_m'),ref('estimated_sale_value_m')),
    additional_acquisition_interest_m=sprintf('%s*%s*%s',
      ref('new_acquisition_debt_m'),ref('acquisition_interest_rate'),
      ref('years_in_period')),
    combined_net_financing_cost_proxy_m=sprintf('%s+%s',
      ref('net_financing_cost_m'),ref('additional_acquisition_interest_m')),
    purchase_adjusted_ebtda_m=sprintf('%s-%s',
      ref('rent_adjusted_ebitda_m'),ref('combined_net_financing_cost_proxy_m')),
    rent_adjusted_pre_tax_income_m=sprintf('%s+%s',
      ref('net_income_m'),ref('ground_rent_m')),
    purchase_adjusted_pre_tax_income_m=sprintf('%s-%s',
      ref('rent_adjusted_pre_tax_income_m'),ref('additional_acquisition_interest_m')),
    purchase_adjusted_pre_tax_margin_pct=sprintf('%s/%s',
      ref('purchase_adjusted_pre_tax_income_m'),ref('revenue_m'))
  )
}
stopifnot(setequal(names(acq_row_formulas(acq_a$first_row)),
                   acquisition_formula_cols))

# Annual BIG4 inputs are SUMs of each of the four annual airport figures.
# Pooled airport source inputs link to three ANNUAL observations and the
# ANNUAL CPI column. Stocks (gross debt, cash) use means rather than sums.
write_acq_source_formulas <- function(tab,which_period) {
  for (i in seq_len(nrow(tab$data))) {
    r <- tab$row_ids[[i]]
    d <- tab$data[i,]
    if(d$airport=='BIG4') {
      stopifnot(i>=5L,identical(as.character(tab$data$airport[(i-4L):(i-1L)]),can_airports))
      for (nm in acquisition_source_cols) {
        col <- acq_excel_col(nm)
        f <- sprintf('SUM(%s%d:%s%d)',col,r-4L,col,r-1L)
        openxlsx::writeFormula(wb,acq_audit_sheet,x=f,
          startCol=match(nm,acquisition_audit_cols),startRow=r)
      }
    } else if (which_period=='Pooled') {
      keys <- paste(d$scenario_id,d$airport,as.character(years))
      rs <- unname(acq_row_lookup[keys])
      stopifnot(length(rs)==length(years),all(!is.na(rs)))
      for (nm in acquisition_source_cols) {
        col <- acq_excel_col(nm)
        addresses <- paste0(col,rs)
        if(nm=='passengers') {
          f <- sprintf('SUM(%s)',paste(addresses,collapse=','))
        } else {
          cpi_addresses <- paste0(acq_excel_col('cpi_factor'),rs)
          terms <- paste0('(',addresses,'*',cpi_addresses,')')
          divisor <- if(nm %in% c('gross_debt_m','cash_m'))
            paste0('/',pooled_years) else ''
          f <- paste0('(',paste(terms,collapse='+'),')',divisor)
        }
        openxlsx::writeFormula(wb,acq_audit_sheet,x=f,
          startCol=match(nm,acquisition_audit_cols),startRow=r)
      }
    }
  }
}
write_acq_source_formulas(acq_a,'Annual')
write_acq_source_formulas(acq_p,'Pooled')

write_acq_derived <- function(tab) {
  by_row <- lapply(tab$row_ids,acq_row_formulas)
  stopifnot(all(vapply(by_row,function(f) setequal(names(f),acquisition_formula_cols),
                       logical(1))))
  for (nm in acquisition_formula_cols) {
    openxlsx::writeFormula(wb,acq_audit_sheet,
      x=vapply(by_row,function(f) unname(f[[nm]]),character(1)),
      startCol=match(nm,acquisition_audit_cols),startRow=tab$first_row)
  }
}
write_acq_derived(acq_a)
write_acq_derived(acq_p)
for(tab in list(acq_a,acq_p)) {
  openxlsx::addStyle(wb,acq_audit_sheet,mult_style,rows=tab$row_ids,
    cols=match('purchase_multiple',acquisition_audit_cols),
    gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,acq_audit_sheet,cpi_style,rows=tab$row_ids,
    cols=match('cpi_factor',acquisition_audit_cols),
    gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,acq_audit_sheet,int_style,rows=tab$row_ids,
    cols=match('passengers',acquisition_audit_cols),
    gridExpand=TRUE,stack=TRUE)
}

# Profit audit: the targets live in ROWS. The columns form a complete
# explanatory chain from the original revenue and required pre-tax profit,
# through the initial shortfall, revenue solution, and verified final result.
# Two audit tables, ANNUAL and POOLED, remain separate on this sheet.
# Within each period, all SIX independent targets appear in order for each
# airport/scenario; no third recovery table and no repeated wide target blocks.
profit_audit_cols <- c(
  'scenario_id','airport','year_label',
  'threshold_id','profitability_objective','target_type',
  'revenue_m','purchase_adjusted_pre_tax_income_m','net_income_m',
  'target_net_margin','statutory_tax_rate','required_pre_tax_margin',
  'pre_tax_profit_target_at_existing_revenue_m','initial_pre_tax_shortfall_m',
  'incremental_revenue_to_profit','shortfall_closed_per_new_revenue_dollar',
  'required_revenue_m','required_profit_m',
  'counterfactual_revenue_m','counterfactual_pre_tax_income_m',
  'estimated_tax_purchase_adjusted_m','estimated_tax_m','incremental_tax_m',
  'counterfactual_after_tax_income_m','achieved_net_margin',
  'required_uplift_pct','passengers','required_revenue_per_pax_cad',
  'unmet_target_pct'
)
profit_audit_headers <- c(
  scenario_id='Scenario',airport='Airport',year_label='Year / period',
  threshold_id='Threshold key',
  profitability_objective='Profitability objective',
  target_type='Target method',
  revenue_m='Observed revenue',
  purchase_adjusted_pre_tax_income_m='Post-acquisition pre-tax profit',
  net_income_m='Historical pre-tax profit',
  target_net_margin='Target after-tax margin',
  statutory_tax_rate='Statutory tax rate',
  required_pre_tax_margin='Required pre-tax margin',
  pre_tax_profit_target_at_existing_revenue_m='Pre-tax target on existing revenue',
  initial_pre_tax_shortfall_m='Initial pre-tax profit shortfall',
  incremental_revenue_to_profit='Share of extra revenue becoming pre-tax profit',
  shortfall_closed_per_new_revenue_dollar='Shortfall closed per extra dollar',
  required_revenue_m='Additional revenue required',
  required_profit_m='Additional pre-tax profit',
  counterfactual_revenue_m='New total revenue',
  counterfactual_pre_tax_income_m='New pre-tax profit',
  estimated_tax_purchase_adjusted_m='Tax before additional revenue',
  estimated_tax_m='Tax after additional revenue',
  incremental_tax_m='Increase in estimated tax',
  counterfactual_after_tax_income_m='New after-tax profit',
  achieved_net_margin='Achieved after-tax margin',
  required_uplift_pct='Increase in revenue',
  passengers='Passengers',
  required_revenue_per_pax_cad='Additional revenue per passenger',
  unmet_target_pct='Unmet net-margin target check'
)
stopifnot(identical(names(profit_audit_headers),profit_audit_cols),
  length(profit_audit_cols)==29L)
profit_audit_sheet <- 'Profitability audit'
create_audit_sheet(profit_audit_sheet,
  'Profitability audit | break even, restore, then four net-margin targets',
  'Two independent formula tables (annual and pooled). The first two rows per airport use fixed pre-tax targets; the last four use after-tax margins on enlarged revenue.',
  length(profit_audit_cols))
profit_audit_pct <- c('target_net_margin','statutory_tax_rate',
  'incremental_revenue_to_profit','required_pre_tax_margin',
  'achieved_net_margin','required_uplift_pct','unmet_target_pct')
profit_stages <- list(
  list(first=1L,last=6L,label='SCENARIO AND SIX GOALS',style=head_style),
  list(first=7L,last=9L,label='STARTING FINANCES',style=head_acquisition),
  list(first=10L,last=14L,label='PRE-TAX TARGET AND OPENING SHORTFALL',style=head_earnings),
  list(first=15L,last=18L,label='REVENUE NEEDED TO CLOSE GAP',style=head_revenue),
  list(first=19L,last=25L,label='COUNTERFACTUAL PROFIT AND TAX',style=head_financing),
  list(first=26L,last=29L,label='CONTEXT AND CHECK',style=head_style)
)
prof_annual <- profitability_model |>
  filter(period=='Annual') |>
  arrange(scenario_id,year_label,
          factor(airport,levels=c(can_airports,'BIG4')),threshold_order)
prof_pooled <- profitability_model |>
  filter(period=='Pooled') |>
  arrange(scenario_id,year_label,
          factor(airport,levels=c(can_airports,'BIG4')),threshold_order)
prof_a <- write_audit_table(profit_audit_sheet,prof_annual,profit_audit_cols,
  profit_audit_headers,'ANNUAL | six independently solved profitability objectives per airport-year',
  title_row=4L,table_name='AuditProfitAnnual',
  pct_names=profit_audit_pct,pax_names='required_revenue_per_pax_cad',
  input_names=c('target_net_margin','statutory_tax_rate',
                'incremental_revenue_to_profit'),stage_groups=profit_stages)
prof_p <- write_audit_table(profit_audit_sheet,prof_pooled,profit_audit_cols,
  profit_audit_headers,
  'POOLED 2023-25 | six goals on CPI-adjusted three-year totals; taxes calculated by airport',
  title_row=prof_a$last_row+7L,table_name='AuditProfitPooled',
  pct_names=profit_audit_pct,pax_names='required_revenue_per_pax_cad',
  input_names=c('target_net_margin','statutory_tax_rate',
                'incremental_revenue_to_profit'),stage_groups=profit_stages)
profit_excel_col <- function(nm) {
  stopifnot(nm %in% profit_audit_cols)
  openxlsx::int2col(match(nm,profit_audit_cols))
}
profit_key <- function(d) {
  paste(d$scenario_id,d$airport,d$year_label,d$threshold_id)
}
profit_rows <- c(setNames(prof_a$row_ids,profit_key(prof_a$data)),
                 setNames(prof_p$row_ids,profit_key(prof_p$data)))
profit_formula_cols <- setdiff(profit_audit_cols,c(
  'scenario_id','airport','year_label','threshold_id',
  'profitability_objective','target_type','target_net_margin',
  'statutory_tax_rate','incremental_revenue_to_profit'))

# Excel reconstructs every value from visible cells, with BOTH types of
# goal in the SAME logical sequence. Fixed pre-tax recovery targets:
#   break-even target = 0; restore target = MAX(0,observed pre-tax profit)
#   gap = MAX(0,target - post-acquisition profit)
#   gap closed per new dollar = c
# Net-margin targets on increased revenue:
#   required pre-tax margin q = after-tax goal m / (1 - statutory tax t)
#   target at original revenue = q * original revenue
#   gap = MAX(0,target at original revenue - post-acquisition profit)
#   gap closed per new dollar = c - q (new revenue also raises target profit)
# For BOTH: extra revenue = gap / gap closed per new dollar; then calculate
# final revenue, pre-tax profit, tax, after-tax profit and achieved margin.
# BIG4 rows sum airport solutions, never treat BIG4 as one tax filer.
profit_row_formulas <- function(d,r) {
  pref <- function(nm) paste0(profit_excel_col(nm),r)
  acq_key <- paste(d$scenario_id,d$airport,d$year_label)
  ar <- unname(acq_row_lookup[acq_key])
  stopifnot(length(ar)==1L,!is.na(ar))
  acqref <- function(nm) paste0("'",acq_audit_sheet,"'!",acq_excel_col(nm),ar)
  is_big4 <- d$airport=='BIG4'
  is_fixed <- d$target_type=='Fixed pre-tax'
  peers <- NULL
  if(is_big4) {
    peer_keys <- paste(d$scenario_id,can_airports,d$year_label,d$threshold_id)
    peers <- unname(profit_rows[peer_keys])
    stopifnot(length(peers)==length(can_airports),all(!is.na(peers)),all(peers<r))
  }
  peer_sum <- function(nm) sprintf('SUM(%s)',
    paste(paste0(profit_excel_col(nm),peers),collapse=','))
  baseline_tax <- if(is_big4) peer_sum('estimated_tax_purchase_adjusted_m') else
    sprintf('MAX(0,%s)*%s',pref('purchase_adjusted_pre_tax_income_m'),
                               pref('statutory_tax_rate'))
  target_on_existing <- if(is_big4) {
    peer_sum('pre_tax_profit_target_at_existing_revenue_m')
  } else if(d$threshold_id=='break_even') {
    '0'
  } else if(d$threshold_id=='restore') {
    sprintf('MAX(0,%s)',pref('net_income_m'))
  } else {
    sprintf('%s*%s',pref('required_pre_tax_margin'),pref('revenue_m'))
  }
  initial <- if(is_big4) peer_sum('initial_pre_tax_shortfall_m') else
    sprintf('MAX(0,%s-%s)',pref('pre_tax_profit_target_at_existing_revenue_m'),
      pref('purchase_adjusted_pre_tax_income_m'))
  closed <- if(is_big4) '""' else if(is_fixed)
    pref('incremental_revenue_to_profit') else
    sprintf('%s-%s',pref('incremental_revenue_to_profit'),
      pref('required_pre_tax_margin'))
  needed <- if(is_big4) peer_sum('required_revenue_m') else
    sprintf('IF(%s>0,MAX(0,%s/%s),NA())',
      pref('shortfall_closed_per_new_revenue_dollar'),
      pref('initial_pre_tax_shortfall_m'),
      pref('shortfall_closed_per_new_revenue_dollar'))
  cf_tax <- if(is_big4) peer_sum('estimated_tax_m') else
    sprintf('MAX(0,%s)*%s',
      pref('counterfactual_pre_tax_income_m'),pref('statutory_tax_rate'))
  cf_profit <- if(is_big4) peer_sum('required_profit_m') else
    sprintf('%s*%s',pref('required_revenue_m'),
      pref('incremental_revenue_to_profit'))
  formulas <- c(
    revenue_m=acqref('revenue_m'),
    purchase_adjusted_pre_tax_income_m=acqref('purchase_adjusted_pre_tax_income_m'),
    net_income_m=acqref('net_income_m'),
    required_pre_tax_margin=if(is_big4 || is_fixed) '""' else
      sprintf('%s/(1-%s)',pref('target_net_margin'),pref('statutory_tax_rate')),
    pre_tax_profit_target_at_existing_revenue_m=target_on_existing,
    initial_pre_tax_shortfall_m=initial,
    shortfall_closed_per_new_revenue_dollar=closed,
    required_revenue_m=needed,
    required_profit_m=cf_profit,
    counterfactual_revenue_m=sprintf('%s+%s',
      pref('revenue_m'),pref('required_revenue_m')),
    counterfactual_pre_tax_income_m=sprintf('%s+%s',
      pref('purchase_adjusted_pre_tax_income_m'),pref('required_profit_m')),
    estimated_tax_purchase_adjusted_m=baseline_tax,
    estimated_tax_m=cf_tax,
    incremental_tax_m=sprintf('%s-%s',
      pref('estimated_tax_m'),pref('estimated_tax_purchase_adjusted_m')),
    counterfactual_after_tax_income_m=sprintf('%s-%s',
      pref('counterfactual_pre_tax_income_m'),pref('estimated_tax_m')),
    achieved_net_margin=sprintf('%s/%s',
      pref('counterfactual_after_tax_income_m'),pref('counterfactual_revenue_m')),
    required_uplift_pct=sprintf('%s/%s',pref('required_revenue_m'),pref('revenue_m')),
    passengers=acqref('passengers'),
    required_revenue_per_pax_cad=sprintf('%s*1000000/%s',
      pref('required_revenue_m'),pref('passengers')),
    unmet_target_pct=if(is_fixed) '""' else
      sprintf('MAX(0,%s-%s)',pref('target_net_margin'),pref('achieved_net_margin'))
  )
  stopifnot(setequal(names(formulas),profit_formula_cols))
  formulas
}

write_profit_derived <- function(tab) {
  by_row <- lapply(seq_len(nrow(tab$data)),function(i)
    profit_row_formulas(tab$data[i,],tab$row_ids[[i]]))
  stopifnot(all(vapply(by_row,function(f)
    setequal(names(f),profit_formula_cols),logical(1))))
  for(nm in profit_formula_cols) {
    v <- vapply(by_row,function(f) unname(f[[nm]]),character(1))
    stopifnot(!anyNA(v))
    openxlsx::writeFormula(wb,profit_audit_sheet,x=v,
      startCol=match(nm,profit_audit_cols),startRow=tab$first_row)
  }
}
write_profit_derived(prof_a)
write_profit_derived(prof_p)
for(tab in list(prof_a,prof_p)) {
  openxlsx::addStyle(wb,profit_audit_sheet,per_dollar_style,rows=tab$row_ids,
    cols=match('shortfall_closed_per_new_revenue_dollar',profit_audit_cols),
    gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,profit_audit_sheet,int_style,rows=tab$row_ids,
    cols=match('passengers',profit_audit_cols),gridExpand=TRUE,stack=TRUE)
  openxlsx::addStyle(wb,profit_audit_sheet,check_style,rows=tab$row_ids,
    cols=match('unmet_target_pct',profit_audit_cols),gridExpand=TRUE,stack=TRUE)
  # A visible boundary separates the fixed recovery goals from the four
  # net-margin goals in every scenario / airport / period group.
  margin_start_rows <- tab$row_ids[tab$data$threshold_order==3L]
  if(length(margin_start_rows))openxlsx::addStyle(wb,profit_audit_sheet,
    openxlsx::createStyle(border='top',borderColour='#B7A77A'),
    rows=margin_start_rows,cols=1:length(profit_audit_cols),
    gridExpand=TRUE,stack=TRUE)
}
stopifnot(length(profit_audit_cols)==29L,
  nrow(prof_a$data)==6L*nrow(acq_a$data),
  nrow(prof_p$data)==6L*nrow(acq_p$data))

# ---- 12. METHODOLOGY AND SMALL FIELD DICTIONARY ---------------------------
# Definitions cover the two audit worksheets, including all six profit goals.
# Avoid the previous 100-row inventory of mostly repeated target variables.
acq_field_definitions <- c(
  scenario_id='Scenario key: sale multiple, new purchase-debt share, new borrowing rate.',
  airport='Individual airport (YYC, YUL, YVR, YYZ) or the BIG4 aggregate.',
  year_label='2023/2024/2025 independent annual snapshots, or pooled 2023-25.',
  purchase_multiple='Assumed value / annualized rent-adjusted EBITDA multiple.',
  acquisition_debt_share='New debt divided by equity purchase consideration, NOT total post-sale debt.',
  acquisition_interest_rate='Annual assumed interest rate on NEW acquisition borrowing.',
  years_in_period='1 for annual scenarios; 3 for pooled interest costs.',
  cpi_factor='Multiplier to convert that annual source observation to 2025 CAD; 1 for pooled.',
  passengers='Observed passenger count; pooled passenger counts are summed.',
  revenue_m='Observed total revenue in CAD millions. Annual nominal, pooled 2025 CAD.',
  ebitda_m='Observed reported EBITDA, including ground rent expense.',
  ground_rent_m='Federal ground rent eliminated in the hypothetical sale.',
  rent_adjusted_ebitda_m='Reported EBITDA + eliminated federal rent.',
  valuation_ebitda_m='Rent-adjusted EBITDA / number of years in period.',
  estimated_sale_value_m='Annualized rent-adjusted EBITDA x sale multiple.',
  gross_debt_m='Existing gross debt. Pooled uses CPI-adjusted annual average.',
  cash_m='Existing unrestricted cash. Pooled uses CPI-adjusted annual average.',
  equity_purchase_consideration_m='Estimated sale value - existing gross debt + cash.',
  new_acquisition_debt_m='Equity purchase consideration x assumed new debt share.',
  buyer_equity_contribution_m='Consideration - new acquisition borrowing.',
  total_gross_debt_m='Existing gross debt + new acquisition borrowing.',
  actual_total_gross_debt_to_sale_value='Combined gross debt / estimated sale value.',
  net_financing_cost_m='Observed financing costs net of interest income; already reflected in net income.',
  additional_acquisition_interest_m='New acquisition debt x annual borrowing rate x years in period.',
  combined_net_financing_cost_proxy_m='Observed net financing expenses + added borrowing interest.',
  purchase_adjusted_ebtda_m='Rent-adjusted EBITDA - combined net financing costs; BEFORE depreciation/amortization.',
  net_income_m='Canadian airport authority net income, interpreted as pre-tax profit.',
  rent_adjusted_pre_tax_income_m='Observed pre-tax profit + eliminated federal rent.',
  purchase_adjusted_pre_tax_income_m='Rent-adjusted pre-tax profit - additional acquisition interest.',
  purchase_adjusted_pre_tax_margin_pct='Post-acquisition pre-tax profit / observed revenue.'
)
profit_field_definitions <- c(
  scenario_id='Financing scenario linking this profitability calculation to the acquisition audit.',
  airport='Individual airport, or BIG4 sums of independently calculated airport results.',
  year_label='Annual observation or three-year CPI-adjusted pooled period.',
  threshold_id='Machine-readable identifier for one of six independent profitability targets.',
  profitability_objective='Ordered from 1. break even, 2. restore historical pre-tax profit, then 3-6. the four after-tax margin benchmarks. Goals are independent, not cumulative.',
  target_type='Fixed pre-tax targets use dollar profit amounts; after-tax margin targets use 15/20/25/30% of NEW total revenue. Two formula methods in the same sequence.',
  revenue_m='Current observed revenue from acquisition audit, BEFORE any required new receipts.',
  purchase_adjusted_pre_tax_income_m='Starting pre-tax profit after rent is eliminated and extra acquisition interest is deducted, BEFORE additional revenue.',
  net_income_m='Observed Canadian airport authority profit (pre-tax); used to determine the restoration goal, with a floor of zero.',
  target_net_margin='AFTER-TAX margin of 15/20/25/30% on the NEW revenue base; BLANK for the two fixed pre-tax goals.',
  statutory_tax_rate='Illustrative corporate tax rate for each airport. For fixed targets, it affects only the displayed after-tax OUTCOME, not required revenue. Blank for BIG4.',
  required_pre_tax_margin='For net-margin goals only, target after-tax margin / (1 - statutory tax rate). This algebraic equivalent is not an observed margin. Blank for fixed goals and BIG4.',
  pre_tax_profit_target_at_existing_revenue_m='For break-even: zero. For restoration: MAX(0, observed historical pre-tax profit). For net margins: required pre-tax margin x CURRENT revenue. This is the initial target BEFORE revenue increases.',
  initial_pre_tax_shortfall_m='MAX(0, initial pre-tax profit target - post-acquisition pre-tax profit). BIG4 sums independently nonnegative airport-level gaps.',
  incremental_revenue_to_profit='Assumed share c of every extra revenue dollar appearing as ADDITIONAL pre-tax earnings; default 1.0 = 100%.',
  shortfall_closed_per_new_revenue_dollar='For FIXED pre-tax targets: c (target stays fixed). For NET MARGIN targets: c - required pre-tax margin (new revenue also increases the required target). Blank for BIG4.',
  required_revenue_m='INITIAL pre-tax shortfall divided by the amount of shortfall closed by each added revenue dollar. BIG4 adds separately solved requirements.',
  required_profit_m='Additional pre-tax profit from the new revenue = additional revenue x c. For BIG4, sum the separate airports.',
  counterfactual_revenue_m='Starting revenue + additional revenue. This enlarged amount is the final denominator for after-tax margin goals.',
  counterfactual_pre_tax_income_m='Post-acquisition starting pre-tax profit + additional pre-tax profit.',
  estimated_tax_purchase_adjusted_m='MAX(0, pre-tax profit BEFORE added revenue) x airport statutory tax rate; BIG4 sums airport amounts.',
  estimated_tax_m='MAX(0, final pre-tax profit) x airport statutory rate; BIG4 sums four airport estimates. Applies to the OUTCOME for all six goals.',
  incremental_tax_m='Estimated tax on final pre-tax profit minus the post-acquisition baseline estimated tax.',
  counterfactual_after_tax_income_m='Final pre-tax profit - estimated corporate tax.',
  achieved_net_margin='Final after-tax income / final total revenue. On the first two pre-tax recovery goals this is an OUTCOME, not a targeted margin.',
  required_uplift_pct='Additional revenue requirement divided by original observed revenue.',
  passengers='Observed annual or pooled passenger count linked from acquisition audit.',
  required_revenue_per_pax_cad='Additional revenue in CAD millions x 1,000,000 / passenger count.',
  unmet_target_pct='Only for the four net-margin goals: MAX(0, target after-tax margin - achieved margin). Blank for fixed pre-tax goals.'
)

stopifnot(setequal(names(acq_field_definitions),acquisition_audit_cols),
          setequal(names(profit_field_definitions),profit_audit_cols))

unit_for <- function(nm) {
  if(nm %in% c('scenario_id','airport','year_label','threshold_id',
    'profitability_objective','target_type')) return('Text')
  if(nm=='passengers') return('Passengers')
  if(nm=='years_in_period') return('Years')
  if(nm=='cpi_factor') return('Multiplier')
  if(nm=='purchase_multiple') return('Multiple')
  if(nm=='required_revenue_per_pax_cad') return('CAD / passenger')
  if(nm=='shortfall_closed_per_new_revenue_dollar') return('CAD of initial shortfall per CAD of new revenue')
  if(nm %in% c('acquisition_debt_share','acquisition_interest_rate',
    'actual_total_gross_debt_to_sale_value','purchase_adjusted_pre_tax_margin_pct',
    'target_net_margin','statutory_tax_rate','incremental_revenue_to_profit',
    'required_pre_tax_margin',
    'achieved_net_margin','required_uplift_pct','unmet_target_pct'))
    return('Fraction (%)')
  'CAD millions'
}
make_dict <- function(fields,headers,defs,sheet,source_fields) {
  tibble::tibble(
    Order=seq_along(fields),
    Stage=sheet,
    Column=fields,
    `Display header`=unname(headers[fields]),
    `Field type`=if_else(fields %in% source_fields,'Input / linked source','Formula'),
    Units=vapply(fields,unit_for,character(1)),
    `Definition / calculation`=unname(defs[fields])
  )
}
column_dictionary <- bind_rows(
  make_dict(acquisition_audit_cols,acq_audit_headers,acq_field_definitions,
    'Acquisition audit',c('scenario_id','airport','year_label',
      'purchase_multiple','acquisition_debt_share','acquisition_interest_rate',
      'years_in_period','cpi_factor',acquisition_source_cols)),
  make_dict(profit_audit_cols,profit_audit_headers,profit_field_definitions,
    'Profitability audit',c('scenario_id','airport','year_label',
      'threshold_id','profitability_objective','target_type',
      'target_net_margin','statutory_tax_rate','incremental_revenue_to_profit',
      'passengers','revenue_m','purchase_adjusted_pre_tax_income_m','net_income_m'))
)
stopifnot(!anyNA(column_dictionary))

notes_sheet <- 'Notes and Methodology'
openxlsx::addWorksheet(wb,notes_sheet,gridLines=FALSE)
openxlsx::mergeCells(wb,notes_sheet,cols=1:7,rows=1)
openxlsx::writeData(wb,notes_sheet,
  'Notes and Methodology | acquisition counterfactual (v22)',startRow=1)
openxlsx::addStyle(wb,notes_sheet,
  openxlsx::createStyle(fontSize=15,textDecoration='bold',fontColour='#193954'),
  rows=1,cols=1)
openxlsx::writeData(wb,notes_sheet,notes,startRow=3,withFilter=FALSE)
for(r in 3:(nrow(notes)+3L)) {
  openxlsx::mergeCells(wb,notes_sheet,cols=2:7,rows=r)
}
openxlsx::addStyle(wb,notes_sheet,head_style,rows=3,cols=1:7,
                   gridExpand=TRUE)
openxlsx::setRowHeights(wb,notes_sheet,rows=3,heights=29)
openxlsx::setRowHeights(wb,notes_sheet,
  rows=4:(nrow(notes)+3L),heights=43)
openxlsx::addStyle(wb,notes_sheet,
  openxlsx::createStyle(wrapText=TRUE,valign='center'),
  rows=4:(nrow(notes)+3L),cols=1:2,gridExpand=TRUE)

# Fictional worked example: present the ECONOMIC REASON for the intermediate
# variables. The amount of extra revenue is not given; it is solved for while
# preserving the net margin on the larger revenue denominator.
example_m <- 0.20
example_t <- 0.25
example_c <- 1.00
example_R <- 1000  # CAD millions
example_P <- 0     # CAD millions
example_q <- example_m/(1-example_t)
example_pre_tax_target <- example_q*example_R
example_gap <- max(0,example_pre_tax_target-example_P)
example_closed <- example_c-example_q
example_extra <- example_gap/example_closed
example_total_R <- example_R+example_extra
example_pre_tax <- example_P+example_c*example_extra
example_tax <- max(0,example_pre_tax)*example_t
example_after_tax <- example_pre_tax-example_tax
stopifnot(abs(example_after_tax/example_total_R-example_m)<1e-12)
worked_example <- tibble::tibble(
  Step=c('1. Current revenue',
    '2. Post-acquisition pre-tax profit',
    '3. Desired after-tax margin',
    '4. Corporate tax rate',
    '5. Required pre-tax margin',
    '6. Required pre-tax profit on CURRENT revenue',
    '7. Initial pre-tax profit shortfall',
    '8. New revenue becoming pre-tax profit',
    '9. Initial shortfall closed by each added dollar',
    '10. Additional revenue REQUIRED',
    '11. New total revenue',
    '12. New pre-tax profit',
    '13. Estimated corporate tax',
    '14. New after-tax profit',
    '15. Achieved net margin'),
  Value=c(sprintf('$%.1fM',example_R),
    sprintf('$%.1fM',example_P),
    sprintf('%.2f%%',100*example_m),
    sprintf('%.2f%%',100*example_t),
    sprintf('%.2f%%',100*example_q),
    sprintf('$%.1fM',example_pre_tax_target),
    sprintf('$%.1fM',example_gap),
    sprintf('$%.2f per $1',example_c),
    sprintf('$%.3f per $1',example_closed),
    sprintf('$%.1fM',example_extra),
    sprintf('$%.1fM',example_total_R),
    sprintf('$%.1fM',example_pre_tax),
    sprintf('$%.1fM',example_tax),
    sprintf('$%.1fM',example_after_tax),
    sprintf('%.2f%%',100*example_after_tax/example_total_R)),
  Explanation=c(
    'Starting observed revenue; the net target must ultimately apply to a larger denominator.',
    'Financing costs have left this fictional airport exactly at break-even.',
    'The private owner wants to KEEP 20 cents after tax for every dollar of FINAL revenue.',
    'At 25% tax, only 75 cents of each positive pre-tax profit dollar remains.',
    '20% / (1 - 25%) = 26.67% pre-tax. The airport must earn 26.67 cents before tax per revenue dollar.',
    '26.67% x $1,000M = $266.7M, BEFORE considering the revenue increase.',
    '$266.7M target - $0M existing profit = $266.7M initial shortfall.',
    'c = 100% means each additional $1 generates $1 of pre-tax earnings; a strong simplifying assumption.',
    'Each new $1 ALSO increases the target pre-tax profit by $0.267. Only $1.00 - $0.267 = $0.733 closes the ORIGINAL shortfall.',
    '$266.7M / $0.733 = $363.6M additional revenue is needed, not merely $266.7M.',
    '$1,000M + $363.6M = $1,363.6M, the denominator of the FINAL margin.',
    '$0M starting profit + $363.6M new pre-tax profit.',
    '25% of $363.6M positive pre-tax profit = $90.9M estimated corporation tax.',
    '$363.6M - $90.9M = $272.7M retained after tax.',
    '$272.7M / $1,363.6M = 20%, verifying that the enlarged-revenue target is met.'
  )
)
example_title_row <- nrow(notes)+7L
example_header_row <- example_title_row+2L
example_data_start <- example_header_row+1L
openxlsx::mergeCells(wb,notes_sheet,cols=1:7,rows=example_title_row)
openxlsx::writeData(wb,notes_sheet,
  'Worked example | why $266.7M of missing pre-tax profit requires $363.6M of new revenue',
  startRow=example_title_row)
openxlsx::addStyle(wb,notes_sheet,
  openxlsx::createStyle(fontSize=12,textDecoration='bold',fontColour='#193954'),
  rows=example_title_row,cols=1)
# The example Step uses the combined width of A:B; a short Value lives in
# C and a full-language explanation spans D:G. This keeps the worked example
# readable without altering the audit sheets or dictionary layout.
worked_example_display <- tibble::tibble(
  Step=worked_example$Step,Spacer=rep('',nrow(worked_example)),
  Value=worked_example$Value,Explanation=worked_example$Explanation)
openxlsx::writeData(wb,notes_sheet,worked_example_display,
  startRow=example_header_row,withFilter=FALSE)
openxlsx::addStyle(wb,notes_sheet,head_revenue,rows=example_header_row,
  cols=1:7,gridExpand=TRUE)
for (r in example_header_row:(example_data_start+nrow(worked_example)-1L)) {
  openxlsx::mergeCells(wb,notes_sheet,cols=1:2,rows=r)
  openxlsx::mergeCells(wb,notes_sheet,cols=4:7,rows=r)
}
openxlsx::setRowHeights(wb,notes_sheet,rows=example_header_row,heights=29)
openxlsx::setRowHeights(wb,notes_sheet,
  rows=example_data_start:(example_data_start+nrow(worked_example)-1L),
  heights=38)
openxlsx::addStyle(wb,notes_sheet,
  openxlsx::createStyle(wrapText=TRUE,valign='center'),
  rows=example_data_start:(example_data_start+nrow(worked_example)-1L),
  cols=1:4,gridExpand=TRUE)

# Small six-threshold map: the first two goals have FIXED pre-tax targets,
# whereas the four net-margin goals automatically increase their target
# pre-tax profit as revenue grows. This guide is intentionally not another
# calculation sheet; it explains which formula to read across each audit row.
threshold_roadmap <- tibble::tibble(
  `Goal`=c('1. Break even','2. Restore historical pre-tax profit',
    '3. 15% net','4. 20% net','5. 25% net','6. 30% net'),
  `Target method`=c(rep('Fixed pre-tax',2),rep('After-tax margin',4)),
  `Target profit at current revenue`=c(
    'Zero','MAX(0, historical pre-tax profit)',
    rep('Net target / (1 - tax rate) x current revenue',4)),
  `How another $1 closes the gap`=c(
    rep('c dollars: fixed pre-tax target does not rise',2),
    rep('c - required pre-tax margin: target also rises with revenue',4))
)
roadmap_title_row <- example_data_start+nrow(worked_example)+4L
roadmap_header_row <- roadmap_title_row+1L
roadmap_data_first <- roadmap_header_row+1L
openxlsx::mergeCells(wb,notes_sheet,cols=1:7,rows=roadmap_title_row)
openxlsx::writeData(wb,notes_sheet,
  'Six objectives | same shortfall-and-revenue logic, two different target formulas',
  startRow=roadmap_title_row)
openxlsx::addStyle(wb,notes_sheet,
  openxlsx::createStyle(fontSize=12,textDecoration='bold',fontColour='#193954'),
  rows=roadmap_title_row,cols=1)
roadmap_display <- tibble::tibble(
  Goal=threshold_roadmap[['Goal']],
  Method=threshold_roadmap[['Target method']],
  Spacer=rep('',6),
  `Target on current revenue`=threshold_roadmap[['Target profit at current revenue']],
  Spacer2=rep('',6),
  `Gap closed per $1`=threshold_roadmap[['How another $1 closes the gap']],
  Spacer3=rep('',6))
openxlsx::writeData(wb,notes_sheet,roadmap_display,
  startRow=roadmap_header_row,withFilter=FALSE)
openxlsx::addStyle(wb,notes_sheet,head_earnings,
  rows=roadmap_header_row,cols=1:7,gridExpand=TRUE)
for (r in roadmap_header_row:(roadmap_data_first+5L)) {
  openxlsx::mergeCells(wb,notes_sheet,cols=4:5,rows=r)
  openxlsx::mergeCells(wb,notes_sheet,cols=6:7,rows=r)
}
openxlsx::setRowHeights(wb,notes_sheet,rows=roadmap_header_row,heights=32)
openxlsx::setRowHeights(wb,notes_sheet,
  rows=roadmap_data_first:(roadmap_data_first+5L),heights=32)
openxlsx::addStyle(wb,notes_sheet,
  openxlsx::createStyle(wrapText=TRUE,valign='center'),
  rows=roadmap_data_first:(roadmap_data_first+5L),
  cols=1:7,gridExpand=TRUE)

dictionary_start <- roadmap_data_first+6L+4L
openxlsx::mergeCells(wb,notes_sheet,cols=1:7,rows=dictionary_start)
openxlsx::writeData(wb,notes_sheet,
  'Field dictionary | acquisition and profitability audits',
  startRow=dictionary_start)
openxlsx::addStyle(wb,notes_sheet,
  openxlsx::createStyle(fontSize=12,textDecoration='bold',fontColour='#193954'),
  rows=dictionary_start,cols=1)
openxlsx::writeData(wb,notes_sheet,column_dictionary,
                    startRow=dictionary_start+1L,withFilter=FALSE)
openxlsx::addStyle(wb,notes_sheet,head_style,rows=dictionary_start+1L,
  cols=1:7,gridExpand=TRUE)
openxlsx::setRowHeights(wb,notes_sheet,rows=dictionary_start+1L,heights=34)
openxlsx::setRowHeights(wb,notes_sheet,
  rows=(dictionary_start+2L):(dictionary_start+nrow(column_dictionary)+1L),
  heights=45)
openxlsx::addStyle(wb,notes_sheet,
  openxlsx::createStyle(wrapText=TRUE,valign='center'),
  rows=(dictionary_start+2L):(dictionary_start+nrow(column_dictionary)+1L),
  cols=1:7,gridExpand=TRUE)
openxlsx::setColWidths(wb,notes_sheet,cols=1:7,
                       widths=c(30,28,24,44,26,28,105))
openxlsx::freezePane(wb,notes_sheet,firstActiveRow=4)

# ---- 13. PER-PASSENGER COMPARISON EXPORTS (2025 CAD) -------------------
# Schema closely mirrors GitHub r/per_unit_comparison.R (V10)'s cost_groups_annual and
# cost_groups_pooled: country, airport, year/period, activity, variable,
# value_m, passengers, amount_cad_2025_m, per_passenger_cad_2025.
# Scenario is an EXTRA key. Keep it: no silent averaging across cases.
# No BIG4 rows in the integration feeds; the comparator is by airport.
# All figures are FLOW amounts. Do not add transaction value or outstanding
# debt stocks to the per-passenger operating-cost comparison.
comparison_fields <- c(
  'cf_observed_total_revenue' = 'revenue_m',
  'cf_ground_rent_eliminated' = 'ground_rent_m',
  'cf_additional_acquisition_interest' = 'additional_acquisition_interest_m',
  'cf_post_acquisition_net_financing_cost' = 'combined_net_financing_cost_proxy_m',
  'cf_post_acquisition_pre_tax_income' = 'purchase_adjusted_pre_tax_income_m',
  'cf_additional_revenue_break_even' = 'required_revenue_break_even_m',
  'cf_additional_revenue_restore' = 'required_revenue_restore_m',
  'cf_additional_revenue_net_15pct' = 'required_revenue_net_15pct_m',
  'cf_additional_revenue_net_20pct' = 'required_revenue_net_20pct_m',
  'cf_additional_revenue_net_25pct' = 'required_revenue_net_25pct_m',
  'cf_additional_revenue_net_30pct' = 'required_revenue_net_30pct_m',
  'cf_net_additional_financing_pressure' = 'net_added_financing_pressure_m',
  'cf_total_revenue_after_restore' = 'total_revenue_after_restore_m'
)
comparison_base <- model |>
  filter(airport %in% can_airports) |>
  mutate(net_added_financing_pressure_m =
           additional_acquisition_interest_m - ground_rent_m,
         total_revenue_after_restore_m = revenue_m + required_revenue_restore_m)
comparison_export <- comparison_base |>
  select(airport, year, period, year_label,
         scenario_id, scenario_name, purchase_multiple,
         acquisition_debt_share, acquisition_interest_rate,
         cpi_factor, passengers, all_of(unname(comparison_fields))) |>
  pivot_longer(cols = all_of(unname(comparison_fields)),
               names_to = 'source_field', values_to = 'value_m') |>
  mutate(
    country = 'Canada',
    activity = 'total',
    variable = names(comparison_fields)[match(source_field,
                                              unname(comparison_fields))],
    # Annual model outputs are nominal; pooled flows already use 2025 CAD.
    # Apply the exact source CPI factor used by the acquisition model.
    adjustment_to_2025 = if_else(period == 'Annual', cpi_factor, 1.0),
    amount_cad_2025_m = value_m * adjustment_to_2025,
    per_passenger_cad_2025 = amount_cad_2025_m * 1e6 / passengers,
    period = if_else(period == 'Annual', as.character(year), '2023-25'),
    series_type = case_when(
      variable %in% c('cf_observed_total_revenue',
                      'cf_post_acquisition_net_financing_cost',
                      'cf_post_acquisition_pre_tax_income',
                      'cf_total_revenue_after_restore') ~ 'Level / outcome',
      TRUE ~ 'Incremental change / requirement'
    ),
    source_context = 'Illustrative acquisition counterfactual; not an observed account'
  ) |>
  select(country, airport, year, period, activity, variable, value_m,
         passengers, amount_cad_2025_m, per_passenger_cad_2025,
         scenario = scenario_name, scenario_id,
         purchase_multiple, acquisition_debt_share, acquisition_interest_rate,
         adjustment_to_2025, series_type, source_context) |>
  arrange(scenario, variable, airport, year)
stopifnot(!anyNA(comparison_export[c('airport','variable', 'passengers',
       'amount_cad_2025_m','per_passenger_cad_2025','scenario')]),
       nrow(comparison_export) == 4L * (length(years) + 1L) *
                                  nrow(scenarios) * length(comparison_fields),
       !anyDuplicated(comparison_export[c('scenario','airport','period','variable')]))

comparison_annual <- comparison_export |> filter(!is.na(year))
comparison_pooled <- comparison_export |> filter(is.na(year))
stopifnot(nrow(comparison_annual) ==
            4L * length(years) * 3L * length(comparison_fields),
          nrow(comparison_pooled) == 4L * 3L * length(comparison_fields))
readr::write_csv(comparison_annual,
                 file.path(out_dir, 'acquisition_per_passenger_annual.csv'))
readr::write_csv(comparison_pooled,
                 file.path(out_dir, 'acquisition_per_passenger_pooled.csv'))

# The pooled per-passenger yield equals the passenger-weighted annual series
# for additive flow variables. Reconstructed annual revenue requirements do
# not, in general, equal separately solved pooled thresholds; those are
# distinct historical stress tests and are intentionally NOT forced equal.
additive_checks <- c('cf_observed_total_revenue',
                     'cf_ground_rent_eliminated',
                     'cf_additional_acquisition_interest',
                     'cf_post_acquisition_net_financing_cost',
                     'cf_post_acquisition_pre_tax_income',
                     'cf_net_additional_financing_pressure')
annual_flow_check <- comparison_annual |>
  filter(variable %in% additive_checks) |>
  group_by(scenario, airport, variable) |>
  summarise(reconstructed_amount_m = sum(amount_cad_2025_m),
            reconstructed_pax = sum(passengers), .groups = 'drop')
pooled_flow_check <- comparison_pooled |>
  filter(variable %in% additive_checks) |>
  left_join(annual_flow_check, by = c('scenario', 'airport', 'variable'))
stopifnot(nrow(pooled_flow_check) == 3L * 4L * length(additive_checks),
  all(abs(pooled_flow_check$amount_cad_2025_m -
          pooled_flow_check$reconstructed_amount_m) < 1e-5),
  all(abs(pooled_flow_check$passengers -
          pooled_flow_check$reconstructed_pax) < 1e-5))

# ---- 14. FINISHED ACQUISITION PRESENTATION EXPORTS ----------------------
# This is the only implementation of the acquisition presentation bridge and
# its 15% operating sensitivities. The workbook builder formats only.
annual <- comparison_annual
pooled <- comparison_pooled
assumptions <- scenarios
financing <- acquisition_export
profitability <- profitability_model
observed_annual <- readr::read_csv(
  here('output', 'airport_comparison', 'comparison_annual_pp_2023_2025.csv'),
  show_col_types = FALSE)
observed_pooled <- readr::read_csv(
  here('output', 'airport_comparison', 'comparison_pooled_pp_2023_2025.csv'),
  show_col_types = FALSE)

assert_cols <- function(d, cols, label) {
  absent <- setdiff(cols, names(d))
  if (length(absent)) {
    stop(label, ' lacks: ', paste(absent, collapse = ', '), call. = FALSE)
  }
}
required_colnames <- c('country', 'airport', 'year', 'period', 'scenario',
  'variable', 'passengers', 'per_passenger_cad_2025', 'amount_cad_2025_m')
assert_cols(annual, required_colnames, 'Acquisition annual CSV')
assert_cols(pooled, required_colnames, 'Acquisition pooled CSV')
assert_cols(assumptions, c('scenario_name', 'purchase_multiple',
  'acquisition_debt_share', 'acquisition_interest_rate'),
  'Acquisition assumptions')
assert_cols(observed_annual, c('country', 'airport', 'year', 'total_revenue'),
            'Observed annual comparison')
assert_cols(observed_pooled, c('country', 'airport', 'total_revenue'),
            'Observed pooled comparison')

scenario_names <- c('Low-cost', 'Central', 'High-cost')
expected_airports <- c('YYC', 'YUL', 'YVR', 'YYZ')
if (!identical(assumptions$scenario_name, scenario_names) ||
    nrow(assumptions) != 3L ||
    !setequal(unique(annual$airport), expected_airports) ||
    !setequal(unique(pooled$airport), expected_airports) ||
    !setequal(unique(annual$scenario), scenario_names) ||
    !setequal(unique(pooled$scenario), scenario_names)) {
  stop('Acquisition scenario/airport coverage does not match the three-case model.',
       call. = FALSE)
}

# Reconcile the model's observed revenue yield with the authoritative
# comparison before adding anything. Both are already in 2025 CAD.
check_observed <- function(acq, official, keys, label) {
  a <- acq |>
    filter(scenario == 'Central', variable == 'cf_observed_total_revenue') |>
    select(all_of(keys), model_pp = per_passenger_cad_2025)
  b <- official |>
    filter(country == 'Canada') |>
    select(all_of(keys), published_pp = total_revenue)
  if (anyDuplicated(a[keys]) || anyDuplicated(b[keys])) {
    stop('Duplicate revenue-validation keys in ', label, call. = FALSE)
  }
  comparison <- inner_join(a, b, by = keys)
  if (nrow(comparison) != nrow(a) || nrow(comparison) != nrow(b) ||
      anyNA(comparison) ||
      any(abs(comparison$model_pp - comparison$published_pp) > 1e-4)) {
    stop('Acquisition revenue per passenger does not reconcile with ', label,
         '. Rebuild from the same current source inputs before combining.',
         call. = FALSE)
  }
}
check_observed(annual, observed_annual, c('airport','year'), 'annual comparison')
check_observed(pooled, observed_pooled, 'airport', 'pooled comparison')

# The presentation keeps the model's flow measures alongside a SMALL
# transaction bridge. Valuation/debt/cash/equity remain absolute 2025 CAD
# millions on BOTH panels: dividing a transaction stock by annual passengers
# would imply an inappropriate annual-flow interpretation.
metric_names <- c(
  cf_observed_total_revenue = 'Observed revenue',
  cf_ground_rent_eliminated = 'Federal ground rent eliminated',
  cf_additional_acquisition_interest = 'Additional acquisition interest',
  cf_post_acquisition_net_financing_cost = 'Post-acquisition net financing costs',
  cf_net_additional_financing_pressure = 'Net added financing pressure',
  cf_post_acquisition_pre_tax_income = 'Post-acquisition pretax income',
  cf_additional_revenue_break_even = 'Revenue needed: break even',
  cf_additional_revenue_restore = 'Revenue needed: restore profit',
  cf_total_revenue_after_restore = 'Total revenue after restoring profit',
  cf_additional_revenue_net_15pct = 'Revenue needed: 15% net margin',
  cf_additional_revenue_net_20pct = 'Revenue needed: 20% net margin',
  cf_additional_revenue_net_25pct = 'Revenue needed: 25% net margin',
  cf_additional_revenue_net_30pct = 'Revenue needed: 30% net margin'
)

# All keys use the literal period label; year remains numeric in the source.
period_name <- function(year) if_else(is.na(year),
  'Pooled 2023–25', as.character(year))
panel_keys <- c('airport', 'period_label', 'scenario')

long_panel <- bind_rows(annual, pooled) |>
  filter(variable %in% names(metric_names)) |>
  mutate(period_label = period_name(year),
         scenario = factor(scenario, levels = scenario_names)) |>
  select(airport, period_label, scenario, variable, passengers,
         per_passenger_cad_2025, amount_cad_2025_m)
if (nrow(long_panel) != 4L * 4L * 3L * length(metric_names) ||
    anyNA(long_panel) ||
    anyDuplicated(long_panel[c(panel_keys, 'variable')]) ||
    !setequal(unique(long_panel$variable), names(metric_names)) ||
    any(long_panel$passengers <= 0)) {
  stop('Incomplete or duplicated annual/pooled acquisition comparison data.',
       call. = FALSE)
}
conversion_error <- abs(long_panel$per_passenger_cad_2025 -
  long_panel$amount_cad_2025_m * 1e6 / long_panel$passengers)
if (any(!is.finite(conversion_error)) || any(conversion_error > 1e-7)) {
  stop('Acquisition absolute and per-passenger figures do not reconcile.',
       call. = FALSE)
}

# One source row per airport x period x financing case (BIG4 is excluded).
# The audit model exports stock figures in nominal CAD millions for each
# annual snapshot and in 2025 CAD millions for its pooled balance-sheet
# assumptions (which are AVERAGES, not three-year stock sums).
bridge_fields <- c(
  'valuation_ebitda_m', 'estimated_sale_value_m', 'gross_debt_m',
  'cash_m', 'equity_purchase_consideration_m',
  'new_acquisition_debt_m', 'buyer_equity_contribution_m',
  'total_gross_debt_m'
)
bridge_headers <- c(
  purchase_multiple = 'Sale multiple (x)',
  acquisition_debt_share = 'New debt share (%)',
  acquisition_interest_rate = 'Acquisition rate (%)',
  valuation_ebitda_m = 'Annualized EBITDA after rent saving (2025 CAD m)',
  estimated_sale_value_m = 'Estimated enterprise value (2025 CAD m)',
  gross_debt_m = 'Existing gross debt (2025 CAD m)',
  cash_m = 'Existing cash (2025 CAD m)',
  equity_purchase_consideration_m = 'Equity purchase consideration (2025 CAD m)',
  new_acquisition_debt_m = 'New acquisition borrowing (2025 CAD m)',
  buyer_equity_contribution_m = 'Buyer equity contribution (2025 CAD m)',
  total_gross_debt_m = 'Total gross debt after purchase (2025 CAD m)',
  actual_total_gross_debt_to_sale_value = 'Post-purchase gross debt / EV (%)'
)
assert_cols(financing, c('scenario_id','airport','year','period',
                        'cpi_factor','purchase_multiple',
                        'acquisition_debt_share','acquisition_interest_rate',
                        'revenue_m','ebitda_m','ground_rent_m',
                        'purchase_adjusted_pre_tax_income_m',
                        bridge_fields, 'actual_total_gross_debt_to_sale_value'),
            'Acquisition financing CSV')
assert_cols(profitability, c('scenario_id','airport','year',
  'threshold_id','statutory_tax_rate','required_pre_tax_margin',
  'required_revenue_m'), 'Acquisition profitability CSV')

finance_rows <- financing |>
  filter(airport %in% expected_airports) |>
  left_join(assumptions |>
    select(scenario_name, purchase_multiple, acquisition_debt_share,
           acquisition_interest_rate),
    by = c('purchase_multiple','acquisition_debt_share',
           'acquisition_interest_rate')) |>
  mutate(
    scenario = scenario_name,
    period_label = period_name(year),
    # Annual source stocks were reported at that year's prices.
    # Pooled stocks already reflect CPI-adjusted means in 2025 CAD.
    factor_2025 = if_else(is.na(year), 1.0, cpi_factor),
    across(all_of(bridge_fields), ~ .x * factor_2025)
  )
if (nrow(finance_rows) != 48L ||
    anyNA(finance_rows[c(panel_keys, bridge_fields)]) ||
    anyDuplicated(finance_rows[panel_keys]) ||
    any(finance_rows$estimated_sale_value_m <= 0)) {
  stop('Invalid, unmatched or duplicated acquisition financing bridge.',
       call. = FALSE)
}

# Independent financing/purchase reconciliation before display.
is_close <- function(a, b, tol = 1e-5) {
  all(is.finite(a) & is.finite(b) &
      abs(a - b) <= tol * pmax(1, abs(a), abs(b)))
}
if (!is_close(finance_rows$estimated_sale_value_m,
              finance_rows$valuation_ebitda_m * finance_rows$purchase_multiple) ||
    !is_close(finance_rows$equity_purchase_consideration_m,
              finance_rows$estimated_sale_value_m -
              finance_rows$gross_debt_m + finance_rows$cash_m) ||
    !is_close(finance_rows$new_acquisition_debt_m,
              finance_rows$equity_purchase_consideration_m *
              finance_rows$acquisition_debt_share) ||
    !is_close(finance_rows$buyer_equity_contribution_m,
              finance_rows$equity_purchase_consideration_m -
              finance_rows$new_acquisition_debt_m) ||
    !is_close(finance_rows$total_gross_debt_m,
              finance_rows$gross_debt_m + finance_rows$new_acquisition_debt_m) ||
    !is_close(finance_rows$actual_total_gross_debt_to_sale_value,
              finance_rows$total_gross_debt_m /
              finance_rows$estimated_sale_value_m)) {
  stop('Acquisition financing bridge fails a transaction/debt check.',
       call. = FALSE)
}
bridge <- finance_rows |>
  select(all_of(panel_keys), all_of(names(bridge_headers)))
names(bridge)[-(1:3)] <- unname(bridge_headers)

# Sensitivity calculations extend the optional 2025 CENTRAL appendix to all
# financing cases and years, applying the EXACT SAME model formula but ONLY
# to the 15% after-tax margin. No change to the acquisition model itself.
# (An optional audit appendix need not be enabled for this integration.)
profit15 <- profitability |>
  filter(airport %in% expected_airports, threshold_id == 'net_15pct') |>
  select(scenario_id, airport, year, statutory_tax_rate,
         required_pre_tax_margin, required_revenue_m)
sensitivity_base <- finance_rows |>
  left_join(profit15, by = c('scenario_id','airport','year')) |>
  mutate(
    operating_cost_base_m = revenue_m - ebitda_m - ground_rent_m,
    target_pre_tax_m = required_pre_tax_margin * revenue_m,
    baseline_15_m = pmax(0, target_pre_tax_m -
      purchase_adjusted_pre_tax_income_m) /
      (1 - required_pre_tax_margin),
    conversion_80_m = pmax(0, target_pre_tax_m -
      purchase_adjusted_pre_tax_income_m) /
      (0.80 - required_pre_tax_margin),
    savings_10_m = 0.10 * operating_cost_base_m,
    saving_10_m = pmax(0, target_pre_tax_m -
      purchase_adjusted_pre_tax_income_m - savings_10_m) /
      (1 - required_pre_tax_margin),
    combined_m = pmax(0, target_pre_tax_m -
      purchase_adjusted_pre_tax_income_m - savings_10_m) /
      (0.80 - required_pre_tax_margin)
  )
if (nrow(sensitivity_base) != 48L ||
    anyNA(sensitivity_base[c(panel_keys, 'required_pre_tax_margin',
      'operating_cost_base_m','baseline_15_m','conversion_80_m',
      'saving_10_m','combined_m')]) ||
    any(sensitivity_base$operating_cost_base_m < -1e-7) ||
    any(sensitivity_base$required_pre_tax_margin >= 0.80) ||
    !is_close(sensitivity_base$baseline_15_m,
              sensitivity_base$required_revenue_m, tol = 1e-6)) {
  stop('15% net-margin sensitivity fails coverage, feasibility or baseline check.',
       call. = FALSE)
}

sensitivity_headers <- c(
  conversion_80_m = '15% margin: 80% conversion',
  saving_10_m = '15% margin: 10% cost saving',
  combined_m = '15% margin: combined'
)
sensitivity_values <- sensitivity_base |>
  mutate(across(all_of(names(sensitivity_headers)),
                ~ .x * factor_2025)) |>
  select(all_of(panel_keys), all_of(names(sensitivity_headers)),
         passengers)
sensitivity_amounts <- sensitivity_values |>
  select(-passengers)
sensitivity_per_pax <- sensitivity_values |>
  mutate(across(all_of(names(sensitivity_headers)),
                ~ .x * 1e6 / passengers)) |>
  select(-passengers)
names(sensitivity_amounts)[-(1:3)] <- unname(sensitivity_headers)
names(sensitivity_per_pax)[-(1:3)] <- unname(sensitivity_headers)

# Audit the baseline 15%-margin requirement against the exact published
# inflation-adjusted acquisition comparison export.
published_15 <- long_panel |>
  filter(variable == 'cf_additional_revenue_net_15pct') |>
  select(all_of(panel_keys),
         baseline_amount_m = amount_cad_2025_m)
baseline_check <- sensitivity_base |>
  left_join(published_15, by = panel_keys)
if (nrow(baseline_check) != 48L || anyNA(baseline_check$baseline_amount_m) ||
    !is_close(baseline_check$baseline_15_m *
                baseline_check$factor_2025,
              baseline_check$baseline_amount_m)) {
  stop('Sensitivity baseline does not reconcile to 15% margin export.',
       call. = FALSE)
}

make_panel <- function(value_col, sensitivity) {
  flows <- long_panel |>
    select(all_of(panel_keys), variable, all_of(value_col)) |>
    pivot_wider(names_from = variable, values_from = all_of(value_col))
  panel <- bridge |>
    left_join(flows, by = panel_keys) |>
    left_join(sensitivity, by = panel_keys) |>
    arrange(airport, factor(period_label,
      levels = c('2023','2024','2025','Pooled 2023–25')),
      factor(scenario, levels = scenario_names)) |>
    select(all_of(panel_keys), all_of(unname(bridge_headers)),
           all_of(names(metric_names)), all_of(unname(sensitivity_headers)))
  if (nrow(panel) != 48L || anyNA(panel) ||
      anyDuplicated(panel[panel_keys])) {
    stop('Incomplete joined panel: ', value_col, call. = FALSE)
  }
  names(panel)[match(names(metric_names), names(panel))] <-
    unname(metric_names)
  names(panel)[1:3] <- c('Airport','Period','Scenario')
  panel
}
panel_amounts <- make_panel('amount_cad_2025_m', sensitivity_amounts)
panel_per_pax <- make_panel('per_passenger_cad_2025', sensitivity_per_pax)
stopifnot(identical(panel_amounts[1:3], panel_per_pax[1:3]))

readr::write_csv(panel_amounts,
  file.path(out_dir, 'acquisition_presentation_amounts.csv'))
readr::write_csv(panel_per_pax,
  file.path(out_dir, 'acquisition_presentation_per_passenger.csv'))

# ---- 15. TWO SIMPLE PLOTS | 2025 PURCHASE, NO DEMAND MODEL ------------
plot_data <- comparison_annual |>
  filter(year == 2025L, airport %in% can_airports) |>
  mutate(scenario = factor(scenario,
                           levels = c('Low-cost', 'Central', 'High-cost')))
plot_colors <- c('Low-cost' = '#6895B6',
                 'Central' = '#E79F45',
                 'High-cost' = '#C75B62')

# This is *additional acquisition interest minus eliminated federal rent*.
# It is NOT a total financing cost or a projection of passenger charges.
financing_2025 <- plot_data |>
  filter(variable == 'cf_net_additional_financing_pressure')
p_financing <- ggplot(financing_2025,
  aes(x = airport, y = per_passenger_cad_2025, fill = scenario)) +
  geom_hline(yintercept = 0, colour = '#7C8793', linewidth = 0.35) +
  geom_col(position = position_dodge(width = 0.77), width = 0.66) +
  scale_fill_manual(values = plot_colors, drop = FALSE) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1)) +
  labs(title = 'Net acquisition financing pressure per passenger',
       subtitle = '2025 hypothetical purchase | new interest less eliminated federal ground rent',
       x = NULL, y = 'CAD per passenger (2025)', fill = 'Financing case') +
  theme_minimal(base_size = 12) +
  theme(legend.position = 'bottom', panel.grid.major.x = element_blank(),
        plot.title.position = 'plot')
ggsave(file.path(out_dir, 'chart_net_financing_pressure_2025.png'),
       p_financing, width = 9.2, height = 5.4, dpi = 220, bg = 'white')

restore_2025 <- plot_data |>
  filter(variable == 'cf_additional_revenue_restore')
p_restore <- ggplot(restore_2025,
  aes(x = airport, y = per_passenger_cad_2025, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.77), width = 0.66) +
  scale_fill_manual(values = plot_colors, drop = FALSE) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1)) +
  labs(title = 'Additional revenue needed to restore historical pre-tax profit',
       subtitle = '2025 hypothetical purchase | 100% incremental revenue-to-profit conversion',
       x = NULL, y = 'Additional CAD per passenger (2025)',
       fill = 'Financing case') +
  theme_minimal(base_size = 12) +
  theme(legend.position = 'bottom', panel.grid.major.x = element_blank(),
        plot.title.position = 'plot')
ggsave(file.path(out_dir, 'chart_revenue_restore_2025.png'),
       p_restore, width = 9.2, height = 5.4, dpi = 220, bg = 'white')

if (include_operating_sensitivities) {
  # ---- 15. SMALL 2025 OPERATING SENSITIVITY (CENTRAL FINANCING ONLY) ---------
  # Optional standalone analyst worksheet. Does not change the three financing
  # packages, their calculations or their audit trails.
  # Existing profitability_model assumes c=1 and zero operating savings.
  # Saving on existing operations and the costs of earning NEW revenue are
  # different assumptions, so test each separately and together.
  operating_cases <- tibble::tribble(
    ~operating_case,              ~conversion, ~cost_saving_share,
    'Baseline',                         1.00,               0.00,
    '80% conversion',                   0.80,               0.00,
    '10% cost saving',                  1.00,               0.10,
    'Combined',                         0.80,               0.10
  )

  # This is an EBITDA-level approximation, NOT an audited functional-cost
  # allocation. Ground rent is removed from the savings base because acquisition
  # already eliminates it; allowing 10% savings on rent would double count.
  # Financial units are CAD millions (and observed 2025 passenger counts).
  operating_base <- central |>
    filter(airport %in% can_airports, year_label == '2025') |>
    transmute(airport, operating_cost_base_m = revenue_m - ebitda_m - ground_rent_m)
  if (nrow(operating_base) != 4L || anyNA(operating_base$operating_cost_base_m) ||
      any(operating_base$operating_cost_base_m < 0)) {
    stop('Invalid 2025 operating cost proxy (revenue minus EBITDA minus federal rent).')
  }

  operating_start <- profitability_model |>
    filter(scenario_id == central_id, year_label == '2025',
           airport %in% can_airports) |>
    left_join(operating_base, by = 'airport')
  stopifnot(nrow(operating_start) == 24L)

  operating_sensitivity_individual <- tidyr::crossing(operating_start, operating_cases) |>
    mutate(
      operating_savings_m = operating_cost_base_m * cost_saving_share,
      starting_profit_after_savings_m =
        purchase_adjusted_pre_tax_income_m + operating_savings_m,
      # Recovery goals have fixed targets; net-margin goals have a target
      # that grows along with revenue.  Each target is INDEPENDENT.
      gap_to_target_m = pmax(0, pre_tax_profit_target_at_existing_revenue_m -
        starting_profit_after_savings_m),
      shortfall_closed_per_dollar = if_else(target_type == 'Fixed pre-tax',
        conversion, conversion - required_pre_tax_margin),
      required_revenue_sensitivity_m = gap_to_target_m / shortfall_closed_per_dollar,
      required_revenue_per_pax_sensitivity =
        required_revenue_sensitivity_m * 1e6 / passengers,
      change_from_baseline_m = required_revenue_sensitivity_m - required_revenue_m,
      uplift_share = required_revenue_sensitivity_m / revenue_m
    )
  stopifnot(nrow(operating_sensitivity_individual) == 96L,
    all(operating_sensitivity_individual$shortfall_closed_per_dollar > 0),
    all(is.finite(operating_sensitivity_individual$required_revenue_sensitivity_m)),
    all(operating_sensitivity_individual$required_revenue_sensitivity_m >= 0),
    all(abs(operating_sensitivity_individual$required_revenue_sensitivity_m[
      operating_sensitivity_individual$operating_case == 'Baseline'] -
      operating_sensitivity_individual$required_revenue_m[
      operating_sensitivity_individual$operating_case == 'Baseline']) < 1e-6))

  # BIG4 has no single statutory tax rate or margin denominator. Sum the four
  # individually solved requirements, savings and underlying cost bases instead.
  operating_sensitivity_big4 <- operating_sensitivity_individual |>
    group_by(operating_case, conversion, cost_saving_share,
             threshold_id, threshold_order, profitability_objective) |>
    summarise(
      operating_cost_base_m = sum(operating_cost_base_m),
      operating_savings_m = sum(operating_savings_m),
      starting_profit_after_savings_m = sum(starting_profit_after_savings_m),
      required_revenue_sensitivity_m = sum(required_revenue_sensitivity_m),
      required_revenue_per_pax_sensitivity =
        sum(required_revenue_sensitivity_m) * 1e6 / sum(passengers),
      change_from_baseline_m = sum(change_from_baseline_m),
      uplift_share = sum(required_revenue_sensitivity_m) / sum(revenue_m),
      .groups = 'drop') |>
    mutate(airport = 'BIG4')

  operating_sensitivity <- bind_rows(
    operating_sensitivity_individual, operating_sensitivity_big4) |>
    arrange(factor(airport, levels = c(can_airports, 'BIG4')),
            factor(operating_case, levels = operating_cases$operating_case),
            threshold_order) |>
    transmute(
      Airport = airport,
      `Operating scenario` = operating_case,
      `Profitability objective` = profitability_objective,
      `Revenue conversion` = conversion,
      `Existing-cost reduction` = cost_saving_share,
      `Existing expense base` = operating_cost_base_m,
      `Operating savings` = operating_savings_m,
      `Pre-tax profit after savings` = starting_profit_after_savings_m,
      `Additional revenue required` = required_revenue_sensitivity_m,
      `Additional revenue per passenger` = required_revenue_per_pax_sensitivity,
      `Change vs baseline` = change_from_baseline_m,
      `Revenue uplift / existing revenue` = uplift_share
    )
  stopifnot(nrow(operating_sensitivity) == 120L,
    !anyNA(operating_sensitivity),
    all(is.finite(operating_sensitivity[['Additional revenue required']])))

  operating_sheet <- 'Operating sensitivities'
  openxlsx::addWorksheet(wb, operating_sheet, gridLines = FALSE)
  operating_titles <- c(
    'Operating sensitivities | hypothetical purchase at end of 2025',
    'Central financing scenario only (22.5x multiple, 60% acquisition debt, 5.5% interest). All amounts in CAD millions except per passenger in CAD.',
    'Illustrative assumptions, not forecasts: 100% or 80% of NEW revenue becomes pre-tax profit; 0% or 10% reduction in EXISTING operating expenses.',
    'Expense base proxy = 2025 revenue - reported EBITDA - federal ground rent. Rent is already removed in acquisition calculations; it cannot be saved twice.',
    'Each of the six goals starts from acquisition-adjusted profit PLUS assumed savings. BIG4 sums separately computed airports. Existing six worksheets are unchanged.'
  )
  for (r in seq_along(operating_titles)) {
    openxlsx::mergeCells(wb, operating_sheet, cols = 1:12, rows = r)
    openxlsx::writeData(wb, operating_sheet, operating_titles[[r]], startRow = r)
  }
  openxlsx::addStyle(wb, operating_sheet,
    openxlsx::createStyle(fontSize=15,textDecoration='bold',fontColour='#193954'),
    rows=1,cols=1)
  openxlsx::addStyle(wb, operating_sheet,
    openxlsx::createStyle(fontSize=10,fontColour='#445569',wrapText=TRUE,valign='center'),
    rows=2:5,cols=1,gridExpand=TRUE)
  openxlsx::setRowHeights(wb, operating_sheet, rows=1:5,
    heights=c(31,29,34,34,32))
  openxlsx::writeData(wb, operating_sheet, operating_sensitivity,
    startRow=7,withFilter=TRUE)
  openxlsx::addStyle(wb, operating_sheet, head_style,
    rows=7,cols=1:12,gridExpand=TRUE)
  openxlsx::setRowHeights(wb, operating_sheet, rows=7,heights=56)
  openxlsx::setColWidths(wb, operating_sheet, cols=1, widths=11)
  openxlsx::setColWidths(wb, operating_sheet, cols=2:3, widths=c(22,29))
  openxlsx::setColWidths(wb, operating_sheet, cols=4:5,widths=17)
  openxlsx::setColWidths(wb, operating_sheet, cols=6:12,widths=22)
  operating_rows <- 8L:(nrow(operating_sensitivity) + 7L)
  openxlsx::addStyle(wb, operating_sheet,num_style,
    rows=operating_rows,cols=6:11,gridExpand=TRUE)
  openxlsx::addStyle(wb, operating_sheet,pct_style,
    rows=operating_rows,cols=c(4,5,12),gridExpand=TRUE)
  openxlsx::addStyle(wb, operating_sheet,per_pax_style,
    rows=operating_rows,cols=10,gridExpand=TRUE,stack=TRUE)
  # Six contiguous objectives per airport and scenario; no split after row 2.
  block_starts <- 8L + seq.int(0L,nrow(operating_sensitivity)-1L,by=6L)
  openxlsx::addStyle(wb, operating_sheet,
    openxlsx::createStyle(border='top',borderColour='#8695A4'),
    rows=block_starts[-1L],cols=1:12,gridExpand=TRUE,stack=TRUE)
  big4_rows <- which(operating_sensitivity$Airport=='BIG4') + 7L
  openxlsx::addStyle(wb, operating_sheet,
    openxlsx::createStyle(textDecoration='bold',fontColour='#193954'),
    rows=big4_rows,cols=1:12,gridExpand=TRUE,stack=TRUE)
  openxlsx::freezePane(wb, operating_sheet,firstActiveRow=8,firstActiveCol=4)

}

output_xlsx <- file.path(out_dir,'airport_acquisition_counterfactual_audit.xlsx')
openxlsx::saveWorkbook(wb,output_xlsx,overwrite=TRUE)
message('Saved v22: ',nrow(model),' acquisition rows and ',
        nrow(profitability_model),' profitability rows, ',
        if (include_operating_sensitivities) 'seven' else 'six',
        ' Excel sheets, CSV exports and two figures. Output directory: ', out_dir)
