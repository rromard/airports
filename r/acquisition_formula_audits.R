# Shared acquisition/profitability formula audits, sourced by both workbooks.
# audit_2025_cad: TRUE when the input annual amounts are already 2025 CAD.
# Annual and pooled audit tables remain independently formula driven.

# ---- 11. TWO FORMULA AUDITS: SEPARATE ANNUAL AND POOLED TABLES ------------
# Audit purpose, not period, determines the worksheet. Within EACH audit
# sheet the Annual and Pooled tables are distinct named Excel tables, with
# independent headers and formula generation. Every derived audit result is
# an Excel formula. Only observed/scenario inputs are hard-coded.
# No explanatory EBTDA margins and no repeated 15/20/25/30% column blocks.

create_audit_sheet <- function(sheet,title,subtitle,ncols) {
  add_acquisition_worksheet(wb,sheet,gridLines=FALSE)
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

acq_audit_sheet <- 'Acquisition calculations'
create_audit_sheet(acq_audit_sheet,
  'Acquisition calculations | valuation, debt and post-acquisition earnings',
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
          terms <- if (isTRUE(audit_2025_cad)) addresses else
            paste0('(',addresses,'*',cpi_addresses,')')
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
profit_audit_sheet <- 'Profitability calculations'
create_audit_sheet(profit_audit_sheet,
  'Profitability calculations | break even, restore, then four net-margin targets',
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

