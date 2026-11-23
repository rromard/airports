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

The historical cap-period revenue, tax series, and country-level comparability figures are **already assembled** in the repository or this PR. Remaining work is principally (1) selecting a defensible common ACCC quality measure for comparison; (2) optional traffic composition detail beyond the ACCC's published Canada–Australia comparisons; and (3) editorial review of ATO legal-entity coverage and the varying definitions of tax benchmarks. Avoid silently substituting mixed accounting measures.

The script requires R packages `dplyr`, `tidyr`, `readr` and `here`. It validates keys and expected counts on critical inputs. It deliberately does not mix financial-year and calendar-year values into a single pseudo-calendar time series.
