# Datawrapper presentation exports

Run the production airport pipeline first, then:

```r
source(here::here("r", "prepare_datawrapper_exports.R"))
```

The script writes flat UTF-8 CSVs to `output/presentation_data/`. It **does not** recalculate the acquisition model, alter formulas, or rebuild an Excel workbook. `00_data_catalog.csv` is the authoritative output/source register: rows with `status = ready` were written and rows with `status = external source needed` identify the missing or optional part of the evidence.

## Storyboard and outputs

| Section | Datawrapper-ready export | Suggested display |
|---|---|---|
| Canada–Australia comparison | `01_airport_scale_2025.csv`, `01b_big4_aggregate_2025.csv` | Eight individual airports and the two four-airport country aggregates; passenger volumes, revenue mix and EBITDA margins |
| Australian real revenue | `02_actual_aeronautical_yield_2015_2025.csv` | 2015=100 indexed lines for real aeronautical revenue per passenger |
| Current finances | `03_current_financials_2025.csv` | Small multiples of revenue, spending, EBITDA, EBIT, after-tax profit |
| Tax | `04_australia_tax_cash_accounting_2016_2025.csv`, `04b_australia_ato_entities_2013_2025.csv` (from existing tax script) | ACCC cash tax, accounting tax expense, ATO tax payable and taxable income |
| Three acquisition cases | `05_acquisition_assumptions.csv` and `06_acquisition_scenarios_2025.csv` | Assumption comparison and acquisition cost sensitivities |
| Profit targets | `07_profitability_targets_2025.csv` | Small multiples: extra revenue total / passenger by airport and objective |
| Revenue composition | `08_revenue_impacts_2025.csv` | Observed vs counterfactual category revenue |
| Observed cap/monitoring history | `09_australia_observed_real_yield_1998_2007.csv` (from existing growth script) | CPI-deflated ACCC/FAC yield; airport and unweighted BIG4 indices (1998=100) |\n| Cap-then-shock sensitivity | `09_legacy_cap_then_shock_paths.csv` (optional) | Illustrative Canadian counterfactual trajectories; **not observed Australian history** |
| Quality | `10_australia_quality_series.csv` (optional) | Time series of one carefully chosen ACCC quality measure |

The historical exports reuse the existing `airport_revenue_counterfactual.R` and its source `output/accc_legacy_vector_selected.csv`; the real-yield table is now written from that script's *already calculated* history, rather than independently deflating the original data. The tax exports reuse `airport_tax_with_cash.R`. These two scripts are outside the default production path and their outputs must exist before their presentation sections are available. Quality requires the parsed ACCC RDS.

**Historical scope:** The counterfactual script already reconstructs the Australian FY1998–2007 price-cap/monitoring period from ACCC Figure 2.2, includes the 1998 Sydney FAC observation, applies Australian FY CPI, and builds three modelled Canadian growth trajectories (gradual, cap-then-shock, immediate-shock) with extrapolated points flagged. The new output `australia_observed_real_yield_1998_2007.csv` simply exports its existing `history` calculation, with per-airport and pooled indexes, so there is **no second deflation method**. The Sydney 1998 figure has different scope from ACCC Ansett-adjusted observations. Historical real-yield levels are CPI-deflated source AUD, *not* directly 2025-CAD currency amounts. Do not confuse observed historical data with modelled Canadian scenarios.

**Quality:** ACCC observations identify source (airline/passenger/objective), terminal, category, measure and fiscal year. Never average noncomparable measures or combine airline with passenger responses mechanically. See [ACCC 2024–25 monitoring](https://www.accc.gov.au/about-us/publications/serial-publications/airport-monitoring-reports/airport-monitoring-report-2024-25).

**Tax:** `airport_tax_with_cash.R` already downloads the ATO corporate tax-transparency workbooks (2013–14 to 2024–25), maps airport reporting entities by name and ABN, processes ACCC airport regulatory-account profits and income tax cash-flow outlays (FY2015–16 through FY2024–25), and exports `output/taxation/aus_airport_tax_rates.csv` and `ato_airport_tax_2013_2025.csv`. The presentation script reuses those validated source files **without refetching anything**. Reported tax expense, cash tax paid and ATO tax payable are distinct measures, and Sydney's 2022–23 reporting entity changes. The source `tax_gap` uses a **30% operating-profit benchmark for Sydney but accounting tax expense for other airports**, so do not plot it as though it represented a uniform statutory benchmark. The tax script also replaces missing cash-tax observations with zero; check original source disclosures before interpreting zeros. Tax exports are in source AUD, not 2025 CAD.

**Units and comparison cautions:** The financial comparison is in **2025 Canadian dollars**; monetary amounts are millions unless labelled per passenger. For Australia, the reported period is the fiscal year ending June 2025; Canadian observations are calendar 2025. Australian aeronautical revenue is compared with Canadian aeronautical revenue **including AIF**. No causal effect of privatization is identified, and increases in airport revenue per passenger should not be presented as increases in posted passenger charges. BIG4 aggregates in the profitability bridge are deliberately summed from the four airports, not a fifth airport.

## Remaining data acquisition

The historical cap-period revenue and ATO cash-tax series are **already assembled** in the repository. Outstanding work is primarily (1) harmonized traffic composition, international/domestic mix and catchment comparability; (2) selecting a defensible common ACCC quality measure for comparison; and (3) editorial review of ATO legal-entity coverage and the varying definitions of tax benchmarks. Avoid silently substituting mixed accounting measures.

The script requires R packages `dplyr`, `tidyr`, `readr` and `here`. It validates keys and expected counts on critical inputs. It deliberately does not mix financial-year and calendar-year values into a single pseudo-calendar time series.
