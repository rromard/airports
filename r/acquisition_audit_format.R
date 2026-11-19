# Shared colours, financial number formats and acquisition column headings.
# Workbook-specific model/audit column definitions must already exist.

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

