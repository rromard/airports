# Datawrapper presentation exports

Run the production airport pipeline first, then:

```r
source(here::here("r", "prepare_datawrapper_exports.R"))
```

The script writes flat UTF-8 CSVs to `output/presentation_data/`. It **does not** recalculate the acquisition model, alter formulas, or rebuild an Excel workbook. `00_data_catalog.csv` is the authoritative output/source register: rows with `status = ready` were written and rows with `status = external source needed` identify the missing or optional part of the evidence.

## Country and institutional comparability

The added `data/comparability/` files feed four prefixed Datawrapper outputs:
- `00a_country_context_2025.csv`: population, land area, density, nominal GDP and GDP per capita from World Bank national comparators. The 2025 GDP totals are deliberately rounded to the nearest US$0.1 trillion.
- `00b_domestic_airline_capacity_2022.csv`: exact same-week, same-unit domestic capacity comparison from **ACCC, Airline competition in Australia (June 2022), Figure 11**.
- `00c_top_two_airline_share_2022.csv`: harmonized duopoly shares from those observations: Canada = **76.5%** (Air Canada group 43.7%, WestJet group 32.8%), Australia = **92.8%** (Qantas group 62.4%, Virgin group 30.4%).
- `00d_policy_context.csv`: sourced, qualitative comparisons of Westminster-style federal parliamentary structures; dispersed national population; public ground ownership/long-term leases; Canadian not-for-profit airport authorities versus Australian for-profit airport concessionaires; Australian initial CPI-X caps and transition to price monitoring in 2002.

The ACCC's 2022 report explicitly compares Canada and Australia as having similar geographic and economic characteristics (pp. 18–21). That report is a stronger methodological justification than treating national size alone as proof of equivalence:
https://www.accc.gov.au/system/files/Airline%20competition%20in%20Australia%20report%20%28June%202022%29.pdf

**Important differences:** Canada has a much larger international/transborder share of airline traffic, while Australia's passenger market is more domestic; Australia's two dominant airline groups have a larger share than Canada's. Neither national population density nor national airline concentration automatically identifies airport-level competitive conditions. Keep this separate from already prepared airport comparisons. For a political/institutional argument, the important contrast is not a matching policy regime but the choice of divergent ownership and profit incentives in otherwise reasonably comparable settings.

The airline duopoly comparison intentionally uses **2022**, when ACCC Figure 11 provides a harmonized method for both countries. Transport Canada 2024 domestic seat-kilometre and ACCC October 2025 domestic passenger figures are newer, but different indicators and should not be put into an apples-to-apples percentage chart.

Sources:
- [World Bank Australia–Canada indicators](https://data.worldbank.org/?locations=AU-CA), [World Bank land area](https://data.worldbank.org/indicator/AG.LND.TOTL.K2)
- [ACCC June 2022 direct cross-country comparison](https://www.accc.gov.au/system/files/Airline%20competition%20in%20Australia%20report%20%28June%202022%29.pdf)
- [Transport Canada: Canadian NAS airport ownership](https://www.canada.ca/en/transport-canada/news/2025/03/policy-statement-on-investment-at-national-airports-system-airports-operated-by-airport-authorities.html)
- [ACCC: end of initial airport price caps](https://www.accc.gov.au/media-release/final-price-cap-reports-for-phase-i-privatised-airports)
- [Australian Productivity Commission: privatization and regulation](https://www.pc.gov.au/media-speeches/speeches/airport-regulation/)

## Storyboard and outputs

| Section | Datawrapper-ready export | Suggested display |
|---|---|---|
| Canada–Australia comparison | `01_airport_scale_2025.csv`, `01b_big4_aggregate_2025.csv` | Eight individual airports and the two four-airport country aggregates; passenger volumes, revenue mix and EBITDA margins |
| Australian real revenue | `02_actual_aeronautical_yield_2015_2025.csv` | 2015=100 indexed lines for real aeronautical revenue per passenger |
| Current finances | `03_current_financials_2025.csv` | Small multiples of revenue, spending, EBITDA, EBIT, financing costs, after-tax profit, shareholder payouts and cash capital investment |
| Tax | `04_australia_tax_cash_accounting_2016_2025.csv`, `04b_australia_ato_entities_2013_2025.csv`, `04c_australia_tax_periods.csv` (from existing tax script) | Source-specific tax measures, diagnostic ratios and cumulative period totals |
| Three acquisition cases | `05_acquisition_assumptions.csv` and `06_acquisition_scenarios_2025.csv` | Assumption comparison and acquisition cost sensitivities |
| Profit targets | `07_profitability_targets_2025.csv` | Small multiples: extra revenue total / passenger by airport and objective |
| Revenue composition | `08_revenue_impacts_2025.csv` | Observed vs counterfactual category revenue |
| Observed cap/monitoring history | `09_australia_observed_real_yield_1998_2007.csv` (from existing growth script) | CPI-deflated ACCC/FAC yield; airport and unweighted BIG4 indices (1998=100) |\n| Cap-then-shock sensitivity | `09_legacy_cap_then_shock_paths.csv` (optional) | Illustrative Canadian counterfactual trajectories; **not observed Australian history** |
| Quality | `10_australia_quality_series.csv` (optional) | Time series of one carefully chosen ACCC quality measure |

The historical exports reuse the existing `airport_revenue_counterfactual.R` and its source `output/accc_legacy_vector_selected.csv`; the real-yield table is now written from that script's *already calculated* history, rather than independently deflating the original data. The tax exports reuse `airport_tax_with_cash.R`. The tax script and presentation exporter now run by default in `run_pipeline.R`, controlled independently by `refresh_australian_tax` and `run_presentation_exports`. The legacy growth script remains optional; its outputs must exist before its presentation section is available. Disable presentation exports for partial runs without the required financial/acquisition inputs. Quality requires the parsed ACCC RDS.

**Historical scope:** The counterfactual script already reconstructs the Australian FY1998–2007 price-cap/monitoring period from ACCC Figure 2.2, includes the 1998 Sydney FAC observation, applies Australian FY CPI, and builds three modelled Canadian growth trajectories (gradual, cap-then-shock, immediate-shock) with extrapolated points flagged. The new output `australia_observed_real_yield_1998_2007.csv` simply exports its existing `history` calculation, with per-airport and pooled indexes, so there is **no second deflation method**. The Sydney 1998 figure has different scope from ACCC Ansett-adjusted observations. Historical real-yield levels are CPI-deflated source AUD, *not* directly 2025-CAD currency amounts. Do not confuse observed historical data with modelled Canadian scenarios.

**Quality:** ACCC observations identify source (airline/passenger/objective), terminal, category, measure and fiscal year. Never average noncomparable measures or combine airline with passenger responses mechanically. See [ACCC 2024–25 monitoring](https://www.accc.gov.au/about-us/publications/serial-publications/airport-monitoring-reports/airport-monitoring-report-2024-25).

**Tax:** `airport_tax_with_cash.R` downloads the ATO corporate tax-transparency workbooks (2013–14 to 2024–25), matches four airport groups by ABN, and processes ACCC regulatory accounts (FY2015–16 to FY2024–25). Run it before regenerating presentation exports; older tax CSVs must be regenerated because the combined schema has changed. All amounts are nominal source AUD, not 2025 CAD.

- `ato_*` identifies tax-return amounts; `reg_*` identifies ACCC regulatory-account measures. ATO total income is not necessarily ACCC revenue, and taxable income is not accounting EBT. Sydney changes ATO reporting entity in 2022–23. Other group boundaries also need care (e.g. Melbourne/Launceston).
- `ato_payable_rate` = ATO tax payable / ATO taxable income; `accounting_etr` = ACCC tax expense / ACCC EBT; `cash_accounting_etr` = ACCC cash paid / ACCC EBT. `payable_accounting_etr` and `cash_taxable_income_rate` mix sources and are diagnostic, not necessarily same-entity rates. The latter is not a conventional cash ETR. `taxable_accounting_income_ratio` helps diagnose the denominator difference.
- [ATO reporting rules](https://www.ato.gov.au/businesses-and-organisations/corporate-tax-measures-and-assurance/large-business/corporate-tax-transparency/report-of-entity-tax-information) suppress amounts of zero or less. Matched ATO records retain non-positive-field flags and a zero proxy for taxable income and tax payable; zero taxable income does not reveal the amount of any tax loss. Unmatched ATO records and undisclosed ACCC cash tax remain missing, not zero. Annual ratios with non-positive denominators are undefined. Negative cash tax indicates a refund.
- `tax_gap` follows the broad [Corporate Knights / Toronto Star 2017 approach](https://corporateknights.com/responsible-investing/the-high-cost-of-low-corporate-taxes/), subsequently used by [Canadians for Tax Fairness (2022)](https://www.taxfairness.ca/en/resources/reports/unaccountable-how-did-canada-lose-30-billion-corporations): statutory tax (30% of reported accounting EBT) less cash income taxes paid. The benchmark is stored as `expected_tax_on_accounting_profit`. It captures tax preferences, reductions and deferral, not just noncompliance, and is not an assessed liability. For cumulative periods, signed accounting EBT is summed, so loss years offset profitable years; this is an explicit implementation choice because the supplied methodology does not specify annual loss treatment. `statutory_ebt_less_cash` retains the positive-EBT floor as a sensitivity. Prefer multi-year gaps to annual gaps. `ato_payable_less_cash` is a separate cross-source liability/payment diagnostic, affected by timing and entity scope; `accounting_expense_less_cash` and `statutory_ebit_less_cash` also remain separate. No Sydney-only EBIT substitution is used for the primary gap. Sydney's historical ACCC EBT is unusual and must be verified before publishing its gap; a consistent definition does not itself resolve source-scope issues.
- `04c_australia_tax_periods.csv` contains ratios of sums, with coverage counts. Missing observations propagate to totals. The historical period 2015–2020 includes FY2019–20 (not strictly pre-pandemic); ATO income totals are sums of disclosed positive taxable income, not net taxable income over time.
- **Overlap reconciliation:** `output/taxation/accc_tax_extraction_reconciliation.csv` compares the tax extractor with the total rows of the existing production `accc_airport_ebitda_by_activity_2022_2025.csv` for all four airports in FY2022–23 through FY2024–25 (48 comparisons: revenue, reported EBIT including abnormal items, EBT and accounting income-tax expense). Production nominal AUD millions are converted back to nominal AUD; differences exceeding AUD 1,000 (one reporting unit in  ATO taxable income AUD 198,150,762 and payable AUD 58,978,216 imply 29.76%, not 58.3% (the latter uses ACCC EBT). Cash paid of AUD 43,037,000 gives 21.72% of ATO taxable income. The ACCC extraction still uses the existing first-numeric-cell and unit heuristic; source file, sheet and multiplier are exported for manual verification. Check the cash rows and units, especially Melbourne and Sydney, before publishing. ATO and ACCC reporting scope is not automatically reconciled.

**Units and comparison cautions:** The financial comparison is in **2025 Canadian dollars**; monetary amounts are millions unless labelled per passenger. For Australia, the reported period is the fiscal year ending June 2025; Canadian observations are calendar 2025. Australian aeronautical revenue is compared with Canadian aeronautical revenue **including AIF**. No causal effect of privatization is identified, and increases in airport revenue per passenger should not be presented as increases in posted passenger charges. BIG4 aggregates in the profitability bridge are deliberately summed from the four airports, not a fifth airport.

## Remaining data acquisition

The historical cap-period revenue, tax series, and country-level comparability figures are **already assembled** in the repository or this PR. Remaining work is principally (1) selecting a defensible common ACCC quality measure for comparison; (2) optional traffic composition detail beyond the ACCC's published Canada–Australia comparisons; and (3) editorial review of ATO legal-entity coverage and the varying definitions of tax benchmarks. Avoid silently substituting mixed accounting measures.

The script requires R packages `dplyr`, `tidyr`, `readr` and `here`. It validates keys and expected counts on critical inputs. It deliberately does not mix financial-year and calendar-year values into a single pseudo-calendar time series.


## Observed shareholder payouts, cash capital investment and Sydney financing

The final annual and pooled comparison tables retain their existing columns and add
`shareholder_payouts` and `cash_capital_investment`. The annual absolute values
are millions of 2025 CAD (the same price/currency basis as the revenue
counterfactual); annual per-passenger values and three-year pooled measures use
the existing conversion and pooling rules. The 2025 observations are also
available as metrics in `03_current_financials_2025.csv` (`dw$financials_2025`).
They are observed amounts, not forecasts, and are not deducted again from
operating profit or inserted into the counterfactual model.

- Canadian shareholder payouts are structural zeros. BNE/MEL/PER use the ACCC
  reported dividends-paid line, with unreported dividends left missing. This
  is not an exhaustive measure of all possible related-party payments.
- Sydney payouts include ordinary cash dividends plus cash interest paid on
  redeemable preference shares (RPS), excluding cash capital returns and
  non-cash distributions/redemption. The source and reconciliation retain these
  separately for audit. Payouts may relate to earnings in other periods.
- Sydney's comparative financing costs and profit exclude RPS interest expense,
  debt-cost amortisation and the 2025 non-cash redemption loss. Finance income,
  swap movements and reported accounting tax remain unchanged. This is a
  classification of earnings before shareholder remuneration, not a
  hypothetical estimate of borrowing costs or tax under alternative ownership.
  Other airports retain their prior income statement definitions; the ACCC
  interest line does not separately identify interest income.
- Cash capital investment is outlay on capital assets and investment property,
  excluding separately identifiable intangible expenditure, borrowing costs,
  grants and disposal proceeds (gross of grants and disposals). Canadian reports
  sometimes bundle intangible and physical assets; Calgary's reported investment
  is reduced by the positive change in related payables to estimate the cash
  outlay. The preserved input CSV records report filename, PDF page and line
  definition. This independent cash-flow measure uses annual-report consolidation
  boundaries, not necessarily ACCC regulatory boundaries. Sydney uses SCACH
  calendar years, with the existing Australian FY CPI and passenger convention.
- `preserved_data/scach_income_verified_inputs_template.csv` is the versioned
  SCACH source; the former local regulatory-accounts path remains a fallback.
  `preserved_data/airport_cash_capital_investment_2023_2025.csv` records the
  capital outlay inputs for all 24 airport-years.
- `output/airport_comparison/sydney_financing_reconciliation_2023_2025.csv`
  and `cash_capital_investment_source_2023_2025.csv` in the same directory
  retain the calculation details. The workbook input tabs also record them.

For a chart comparing the scale of the new revenue requirements against observed
profit, shareholder payouts and investment, filter the current-financials table
to the relevant measures and the revenue-impacts table to the total category,
one financing scenario and one profitability objective. Do not add scenarios,
or the revenue total to its component rows. Australian payout benchmarks are
observed amounts, not estimates of future Canadian shareholder payouts.
000 workbooks) are flagged. The check compares unadjusted ACCC data, not Sydney's SCACH-based production adjustments, and retains both source filenames and sheet names, including possible Perth FY2023–24 revision differences. Missing/invalid reference data, missing values and discrepancies warn and are recorded in the audit rather than silently accepted. Neither extraction is overwritten; the accounting extraction remains duplicated technical debt pending a later refactor. Review every row not marked `within_tolerance` before publication. The check does not validate the non-overlapping historical years, cash tax or ATO values.
- Perth FY2018–19 is a regression diagnostic: ATO taxable income AUD 198,150,762 and payable AUD 58,978,216 imply 29.76%, not 58.3% (the latter uses ACCC EBT). Cash paid of AUD 43,037,000 gives 21.72% of ATO taxable income. The ACCC extraction still uses the existing first-numeric-cell and unit heuristic; source file, sheet and multiplier are exported for manual verification. Check the cash rows and units, especially Melbourne and Sydney, before publishing. ATO and ACCC reporting scope is not automatically reconciled.

**Units and comparison cautions:** The financial comparison is in **2025 Canadian dollars**; monetary amounts are millions unless labelled per passenger. For Australia, the reported period is the fiscal year ending June 2025; Canadian observations are calendar 2025. Australian aeronautical revenue is compared with Canadian aeronautical revenue **including AIF**. No causal effect of privatization is identified, and increases in airport revenue per passenger should not be presented as increases in posted passenger charges. BIG4 aggregates in the profitability bridge are deliberately summed from the four airports, not a fifth airport.

## Remaining data acquisition

The historical cap-period revenue, tax series, and country-level comparability figures are **already assembled** in the repository or this PR. Remaining work is principally (1) selecting a defensible common ACCC quality measure for comparison; (2) optional traffic composition detail beyond the ACCC's published Canada–Australia comparisons; and (3) editorial review of ATO legal-entity coverage and the varying definitions of tax benchmarks. Avoid silently substituting mixed accounting measures.

The script requires R packages `dplyr`, `tidyr`, `readr` and `here`. It validates keys and expected counts on critical inputs. It deliberately does not mix financial-year and calendar-year values into a single pseudo-calendar time series.


## Observed shareholder payouts, cash capital investment and Sydney financing

The final annual and pooled comparison tables retain their existing columns and add
`shareholder_payouts` and `cash_capital_investment`. The annual absolute values
are millions of 2025 CAD (the same price/currency basis as the revenue
counterfactual); annual per-passenger values and three-year pooled measures use
the existing conversion and pooling rules. The 2025 observations are also
available as metrics in `03_current_financials_2025.csv` (`dw$financials_2025`).
They are observed amounts, not forecasts, and are not deducted again from
operating profit or inserted into the counterfactual model.

- Canadian shareholder payouts are structural zeros. BNE/MEL/PER use the ACCC
  reported dividends-paid line, with unreported dividends left missing. This
  is not an exhaustive measure of all possible related-party payments.
- Sydney payouts include ordinary cash dividends plus cash interest paid on
  redeemable preference shares (RPS), excluding cash capital returns and
  non-cash distributions/redemption. The source and reconciliation retain these
  separately for audit. Payouts may relate to earnings in other periods.
- Sydney's comparative financing costs and profit exclude RPS interest expense,
  debt-cost amortisation and the 2025 non-cash redemption loss. Finance income,
  swap movements and reported accounting tax remain unchanged. This is a
  classification of earnings before shareholder remuneration, not a
  hypothetical estimate of borrowing costs or tax under alternative ownership.
  Other airports retain their prior income statement definitions; the ACCC
  interest line does not separately identify interest income.
- Cash capital investment is outlay on capital assets and investment property,
  excluding separately identifiable intangible expenditure, borrowing costs,
  grants and disposal proceeds (gross of grants and disposals). Canadian reports
  sometimes bundle intangible and physical assets; Calgary's reported investment
  is reduced by the positive change in related payables to estimate the cash
  outlay. The preserved input CSV records report filename, PDF page and line
  definition. This independent cash-flow measure uses annual-report consolidation
  boundaries, not necessarily ACCC regulatory boundaries. Sydney uses SCACH
  calendar years, with the existing Australian FY CPI and passenger convention.
- `preserved_data/scach_income_verified_inputs_template.csv` is the versioned
  SCACH source; the former local regulatory-accounts path remains a fallback.
  `preserved_data/airport_cash_capital_investment_2023_2025.csv` records the
  capital outlay inputs for all 24 airport-years.
- `output/airport_comparison/sydney_financing_reconciliation_2023_2025.csv`
  and `cash_capital_investment_source_2023_2025.csv` in the same directory
  retain the calculation details. The workbook input tabs also record them.

For a chart comparing the scale of the new revenue requirements against observed
profit, shareholder payouts and investment, filter the current-financials table
to the relevant measures and the revenue-impacts table to the total category,
one financing scenario and one profitability objective. Do not add scenarios,
or the revenue total to its component rows. Australian payout benchmarks are
observed amounts, not estimates of future Canadian shareholder payouts.
