# Datawrapper presentation exports

Run the production airport pipeline first, then:

```r
source(here::here("r", "prepare_datawrapper_exports.R"))
```

The script writes flat UTF-8 CSVs to `output/presentation_data/`. It **does not** recalculate the acquisition model, alter formulas, or rebuild an Excel workbook. `00_data_catalog.csv` is the authoritative output/source register: rows with `status = ready` were written and rows with `status = external source needed` identify the missing or optional part of the evidence.

## Storyboard and outputs

| Section | Datawrapper-ready export | Suggested display |
|---|---|---|
| Canada–Australia comparison | `01_airport_scale_2025.csv` | Eight airport passenger volumes, airport revenue mix, or EBITDA margin |
| Australian real revenue | `02_actual_aeronautical_yield_2015_2025.csv` | 2015=100 indexed lines for real aeronautical revenue per passenger |
| Current finances | `03_current_financials_2025.csv` | Small multiples of revenue, spending, EBITDA, EBIT, after-tax profit |
| Tax | `04_australia_accounting_tax_2023_2025.csv` | Accounting tax expense vs pre-tax profit, with major caveats |
| Three acquisition cases | `05_acquisition_assumptions.csv` and `06_acquisition_scenarios_2025.csv` | Assumption comparison and acquisition cost sensitivities |
| Profit targets | `07_profitability_targets_2025.csv` | Small multiples: extra revenue total / passenger by airport and objective |
| Revenue composition | `08_revenue_impacts_2025.csv` | Observed vs counterfactual category revenue |
| Observed cap/monitoring history | `09_observed_nominal_australia_1998_2007.csv` (optional) | ACCC historical nominal aeronautical yield; **not inflation-adjusted** |\n| Cap-then-shock sensitivity | `09_legacy_cap_then_shock_paths.csv` (optional) | Illustrative Canadian counterfactual trajectories; **not observed Australian history** |
| Quality | `10_australia_quality_series.csv` (optional) | Time series of one carefully chosen ACCC quality measure |

The observed historical export requires the separately prepared `output/accc_legacy_vector_selected.csv`; the historical trajectory requires the separately disabled legacy revenue-growth model; quality requires the parsed ACCC RDS. The current production pipeline is sufficient for the first eight exports.

**Historical scope:** The actual 2015–2025 series belongs to the modern monitoring-period comparison. The 1998–2007 cap-and-shock sequence is drawn from original ACCC/FAC digitization when available. That file is in nominal AUD and **must be deflated before being plotted as real growth**, with explicit notes about price-cap expiry, the Sydney 1998 source discontinuity, and the difference between observed and extrapolated years. The optional legacy trajectory is a **Canadian counterfactual**, not Australian observed history.

**Quality:** ACCC observations identify source (airline/passenger/objective), terminal, category, measure and fiscal year. Never average noncomparable measures or combine airline with passenger responses mechanically. See [ACCC 2024–25 monitoring](https://www.accc.gov.au/about-us/publications/serial-publications/airport-monitoring-reports/airport-monitoring-report-2024-25).

**Tax:** `accounting_income_tax_expense` is not the same as tax paid to the Australian Taxation Office. The independent ATO [corporate tax transparency publications](https://www.ato.gov.au/businesses-and-organisations/corporate-tax-measures-and-assurance/large-business/corporate-tax-transparency/our-corporate-tax-transparency-reports) report entity-level taxable income and tax payable, not necessarily consolidated airport-group results. Before publishing comparisons, construct an entity crosswalk and address deferred tax, debt/interest, accumulated losses, and reporting scope. Neither zero tax paid nor low accounting tax expense alone proves illegal avoidance.

**Units and comparison cautions:** The financial comparison is in **2025 Canadian dollars**; monetary amounts are millions unless labelled per passenger. For Australia, the reported period is the fiscal year ending June 2025; Canadian observations are calendar 2025. Australian aeronautical revenue is compared with Canadian aeronautical revenue **including AIF**. No causal effect of privatization is identified, and increases in airport revenue per passenger should not be presented as increases in posted passenger charges. BIG4 aggregates in the profitability bridge are deliberately summed from the four airports, not a fifth airport.

## Remaining data acquisition

The data catalog marks separate source work needed for: (1) traffic composition, international/domestic mix and airport/catchment comparability; (2) 1998–2007 directly observed ACCC revenue/charge history; (3) ATO entity-level cash tax and group-ownership/entity concordance. Keep those additions in independently sourced tables with definitions and URLs before integrating them into charts.

The script requires R packages `dplyr`, `tidyr`, `readr` and `here`. It validates keys and expected counts on critical inputs. It deliberately does not mix financial-year and calendar-year values into a single pseudo-calendar time series.
