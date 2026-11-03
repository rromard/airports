# Airport privatization: Canada–Australia financial comparisons

Research code and data sources for comparing financial outcomes at Canada's four largest airports with those at four major privately operated Australian airports, and for exploring illustrative Canadian airport-revenue scenarios under Australian historical revenue-growth patterns.

The analysis is a **descriptive comparison and scenario exercise**, not a causal estimate of the effect of privatization. The repository documents the assumptions and source transformations used in the calculations.

## Coverage

| Canada (calendar year) | Australia (financial year ending 30 June) |
| --- | --- |
| Calgary (YYC) | Brisbane (BNE) |
| Montréal–Trudeau (YUL) | Melbourne (MEL) |
| Toronto Pearson (YYZ) | Perth (PER) |
| Vancouver (YVR) | Sydney (SYD) |

Revenue and passenger comparisons cover **2015–2025** where sources are available; detailed Canadian–Australian cost and EBITDA comparisons focus on **2023–2025**. The Australian year labelled 2025 is FY2024–25, whereas Canada's 2025 is the calendar year. The COVID-19 disruption therefore falls into nonidentical reporting periods.

## Data sources

### Canadian airports

1. **Airport authority annual reports, audited financial statements, and financial reports.** These provide Canadian revenue and expenditure line items, including aeronautical revenues, airport improvement fees (AIF), commercial revenues, staffing and other operating expenses, rent, and depreciation/amortization where reported. The four authorities are the Calgary Airport Authority, Aéroports de Montréal, Vancouver Airport Authority, and Greater Toronto Airports Authority. Source documents are generally excluded from Git; their underlying values are manually compiled in project workbooks.
2. **Hand-compiled Canadian historical series, 2015–2022:** `data/canada/operator_reports/canadian_big4_aeronautical_revenue_2015_2022.xlsx`, worksheet `Main 2015-2022`. Includes reported aeronautical and AIF revenues, separate gross-up adjustments, and comparison totals; original reported figures are retained. This is a curated input worth tracking in Git.
3. **Hand-compiled recent Canadian financial series, 2023–2025:** currently read from `output/Airport privatization CCPA sheet.xlsx`, worksheet `Big 4 long`. The source sheet is worth extracting into a dedicated tracked input outside `output/`; the scripts' paths must be updated together when it is moved.
4. **Statistics Canada, table 23-10-0253-01**, airport passenger traffic (`Total, passenger sector`) for the four Canadian airports, accessed via the `cansim` R package.
5. **Statistics Canada, table 18-10-0005-01**, annual all-items CPI for Canada, accessed via `cansim`.

### Australian airports

1. **Australian Competition and Consumer Commission (ACCC), Airport Monitoring Report 2024–25 supplementary database.** Long-run airport financial, passenger, parking, and other reported series. The project's derived financial workbook is currently read from `output/accc_airport_financial_analysis_2007_2025.xlsx`, particularly the `financial` and `passenger_calcs` sheets. The original ACCC workbook is a public source; the derived workbook is a generated input to later analysis.
2. **ACCC annual regulatory accounts for 2022–23, 2023–24, and 2024–25**, including the revised 2023–24 Perth disclosure. These contain audited totals and aeronautical/non-aeronautical activity allocations and are parsed by `accc_regulatory_ebitda_v4_fixed.R`.
3. **Australian Bureau of Statistics, Consumer Price Index, Australia**, quarterly all-groups CPI (weighted average of eight capital cities), series `A2325846C`. The project constructs financial-year average CPI from the four relevant quarters via `readabs`. [ABS CPI](https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia).
4. **Bank of Canada annual-average exchange rates.** The scripts use **0.9009 CAD per AUD** for 2025 to present Australian amounts in common 2025 Canadian-dollar units. [Bank of Canada rates](https://www.bankofcanada.ca/rates/exchange/annual-average-exchange-rates/).
5. **ACCC, *Airport price monitoring and financial reporting, 2002–03*.** Used for the separate historical price-shock sensitivity exercise, including reported changes in average aeronautical charges per passenger in the early 2000s. [Historical ACCC report](https://www.accc.gov.au/system/files/Airport%20price%20monitoring%20%26%20financial%20report%202002-03.pdf).

Public downloads and generated files are not necessarily tracked by Git. The distinction is intentional: preserve curated, difficult-to-recreate research inputs, while documenting how to obtain public sources and rebuild derived data.

## Main methods

### Harmonized revenue and passenger yields

Financial values are expressed in **constant 2025 CAD** and, where appropriate, divided by annual passenger traffic to obtain dollars per passenger. Canadian nominal dollar amounts are adjusted with annual Canadian CPI, rebased to 2025. Australian ACCC *supplementary* financial figures are already in **constant 2024–25 AUD**: **do not deflate these a second time**. Australian *regulatory-account* amounts are nominal AUD and are separately adjusted using Australian financial-year CPI before conversion to CAD.

The main longitudinal aeronautical comparison uses:

- **Canada:** reported aeronautical revenue **plus airport improvement fees** (AIF), as an explicitly derived measure;
- **Australia:** ACCC-reported aeronautical revenue.

AIF is included for a more informative comparison of the revenues collected from airport users, but these measures are **not necessarily identical in regulatory or accounting scope**. The AIF-inclusive aggregate must not be added to total revenue again. The scripts retain more detailed components so that definitions and sources can be inspected.

The recent comparison also distinguishes aeronautical, commercial, parking/landside, and expenditure categories where available. The Australian commercial aggregate is calculated as total revenue less aeronautical revenue; narrower `other` categories should not be assumed to include parking or landside services unless documented by the parser.

### Cost and earnings comparisons

Detailed Australian regulatory expenditure is grouped into labour, purchased services/other operating costs, and depreciation/amortisation. EBITDA is calculated in the regulatory parser as **revenue less total expenditure plus depreciation/amortisation**. The source accounts distinguish aeronautical, non-aeronautical, and total operations.

The corresponding Canadian financial categories are mapped from airport authority statements. The accounts do not always use equivalent classifications, and Canadian airports have distinctive line items, including federal ground rent and payments in lieu of municipal taxes. EBITDA, operating earnings, and ACCC supplementary EBITA or operating-margin concepts should not be treated as interchangeable. Reported margins require attention to their numerators, asset valuations, and accounting boundaries.

For 2023–2025 comparisons, pooled per-passenger measures use **the sum of real amounts divided by the sum of passengers**, not the unweighted average of annual ratios. The scripts also provide annual-level results and coverage diagnostics.

### Counterfactual revenue scenarios

`airport_revenue_counterfactual_v7.R` applies the **observed Australian mean growth in real aeronautical revenue per passenger** to each Canadian airport's own **2015 real aeronautical-plus-AIF revenue per passenger**. Subsequent implied revenue uses **actual Canadian airport-specific passenger traffic**, and the difference is measured against actual Canadian aeronautical-plus-AIF revenue in constant 2025 CAD. The central scenario is the annual unweighted mean of the four Australian airport growth indices. Low/high illustrative trajectories phase to ±10 percentage points around the central 2025 growth-index endpoint; these are **assumed sensitivity bands, not confidence intervals**. Differences may be negative, and the cumulative measure covers 2016–2025.

`airport_revenue_counterfactual_price_shock.R` extends the exercise with alternative trajectories using observed Australian aeronautical revenue-per-passenger increases around 2000–2003, alongside explicit assumptions about when such a shock would occur in Canada, its strength, and subsequent real growth. Historic Australian changes are used as **scenario shapes**, not forecasts. The analysis does **not** establish that privatization caused the price changes, does not model airline pass-through to airfares, and is not a net-profit or welfare estimate.

## Scripts and execution order

The script filenames below reflect the current working versions; some locally required upstream scripts or intermediate files may not yet be included in this repository.

1. **ACCC supplementary database parser** (`parse_accc_airports.R`, referenced by the scripts; ensure it is present in the repository). Creates the long-run Australian financial analysis workbook and passenger export from the ACCC supplementary database.
2. **`accc_regulatory_ebitda_v4_fixed.R`** parses the three years of regulatory accounts and the revised Perth source. Writes activity-specific EBITDA and Australian expenditure CSVs under `output/`.
3. **`per_unit_comparison_v8.R`** builds the 2015–2025 Canada–Australia revenue comparison and, when `run_cost_comparison <- TRUE`, the recent cost comparison. Set this option to `FALSE` for a revenue-only run without regulatory-account cost exports. It reads Canadian curated workbooks, Australian supplementary outputs, traffic and CPI data, and emits CSVs to `output/airport_comparison/`.
4. **`airport_revenue_counterfactual_v7.R`** consumes `output/airport_comparison/aero_revenue_growth_2015_2025.csv` and produces annual, cumulative, chart, and workbook comparisons under `output/airport_counterfactual_three_scenarios/`.
5. **`airport_revenue_counterfactual_price_shock.R`** consumes the same comparison input and writes additional sensitivity results under `output/airport_counterfactual_price_shock/`.

`per_unit_comparison_v7.R` is an earlier three-year comparison retained for reference, not the preferred entry point for the extended 2015–2025 analysis.

R dependencies used across the scripts include `tidyverse`, `here`, `readxl`, `readr`, `janitor`, `cansim`, `readabs`, and `openxlsx` (for counterfactual Excel exports). Additional functions use `lubridate` and plotting packages available through the project setup. Run scripts from the repository root so that `here::here()` resolves the intended paths.

### Key working input paths

```text
data/canada/operator_reports/canadian_big4_aeronautical_revenue_2015_2022.xlsx
data/australia/regulatory_accounts/{ACCC year-specific workbooks}
data/australia/accc_airport_passengers_2007_2025.csv    # optional if workbook fallback is available
data/australia/abs_cpi_fy_2015_2025.csv             # optional cache
output/Airport privatization CCPA sheet.xlsx         # CURRENT recent Canadian input: relocate
output/accc_airport_financial_analysis_2007_2025.xlsx # generated upstream ACCC input
```

The `output/` folder is a working/generated area even though the current analysis also reads some files from it. **Moving a curated input out of `output/` requires changing the corresponding path in the script.** This README describes the working code as inspected; it does not certify a clean-clone build yet.

## Known qualifications and reproducibility checks

- The historical Canadian source's **YYZ 2015 AIF gross-up** uses an **assumed 4% airline collection fee** when the corresponding documented adjustment is missing. It is explicitly flagged and should not be mistaken for an audited reported figure.
- The working V8 script notes that **YYC 2015** was blank in the last supplied historical workbook. Verify that this has been corrected before treating the 2015 Canadian Big 4 aggregate or 2015-based counterfactuals as complete.
- Australian fiscal years and Canadian calendar years are not identical. No year alignment can remove that mismatch entirely.
- Historic and recent airport accounts use different reporting rules and financial definitions; categories are harmonized only to the extent supported by the reported data.
- The scripts contain uniqueness, panel-completeness, reconciliation, and source-coverage checks. Failed checks should be investigated, not suppressed to force complete output.
- Historical Australian price changes occurred in a distinctive context of changing regulation, aviation-industry disruption, and security costs. Scenario calculations should not be presented as causal estimates of privatization.

## Repository policy

**Commit:** R scripts, this README, and manually compiled/annotated input tables whose recreation would require significant additional research. **Generally exclude:** downloaded annual-report PDFs, large archives, public-source caches, and generated spreadsheets, CSVs, and plots. Original ACCC workbooks are optional source snapshots if offline reproducibility is a priority. Keep the original source values and document any restatements or estimates rather than overwriting them silently.

## Project status

Working research code, prepared for an initial Git commit. Before labelling the repository fully reproducible, confirm the outstanding historical-source completeness checks, include the supplementary ACCC parser, relocate the curated recent Canadian input if desired, and test an end-to-end run with the paths and dependencies supplied in the repository.
