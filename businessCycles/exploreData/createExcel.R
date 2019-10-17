library(openxlsx)

# Per Firm Data
selectedFirmsToExcel = fScQ %>%
  filter( OLQ != 2, 
          BornTick < recessionStart,
          Death == 60)

#varsToExcel = setdiff(names(fScQ), c(idxVars, relevantParams, quantileVars, "toSTring"))
varsToExcel = c(
  "AcumProfit",
  "AcumQuantity",
  "Capital",
  "Invest",
  "LearningRate",
  "MarketShare",               
  "MaxFunding",
  "MedCost",
  "OperatingLeverage",
  "Performance",
  "Profit",
  "Quantity",
  "Return",
  "TotalProdCost",
  "UnitProdCost",
  "FlexibilityCostComponent",
  "LearningComponent",
  "OperatingLevarageComponent",
  "OptimalFullCapacityMarkUp",
  "LongTermMarginalCost",
  "ShortTermMarginalCost",     
  "CurrentMarkUpForPlanning",
  "CapitalShareAfterExit",
  "QuantityPerPeriod",
  "UsedCapacity",
  "IndustryPrice",
  "LocalPrice",
  "Sales",
  "Margin"
)

varsToExcel = c(
  "Capital",
  "Invest",
  "LearningRate",
  "MarketShare",               
  "MaxFunding",
  "MedCost",
  "Performance",
  "Profit",
  "Quantity",
  "Return",
  "UnitProdCost",
  "FlexibilityCostComponent",
  "LearningComponent",
  "OperatingLevarageComponent",
  "OptimalFullCapacityMarkUp",
  "LongTermMarginalCost",
  "ShortTermMarginalCost",     
  "CurrentMarkUpForPlanning",
  "CapitalShareAfterExit",
  "UsedCapacity",
  "IndustryPrice",
  "LocalPrice",
  "Sales",
  "Margin"
)

varsToExcel.d = sapply(varsToExcel, function(x) paste0(x, ".d"))

#Plain Data per Cohort
dataToExcel = selectedFirmsToExcel %>%
  select_at(vars(idxVars, relevantParams, "OLQ", varsToExcel)) %>%
  meanByQuantiles("OLQ", varsToExcel, relevantParams) 


# Differences per Firm against Base
perFirmDiffToExcel = selectedFirmsToExcel %>%
  select_at(vars(idxVars, relevantParams, "OLQ", varsToExcel)) %>%
  addDiff(varsToExcel, c("run", "OLQ"), "recessionMagnitude", 0, percent = FALSE) %>%
  meanByQuantiles("OLQ", varsToExcel.d, relevantParams)

###
### Agreggated Data fro Selected Firms
###

calcVarsByQuantileToExcel = c("N", "NDeath", "NBorn","TotalSales")
calcVarsByQuantileToExcel.d = sapply(calcVarsByQuantileToExcel, function(x) paste0(x, ".d"))

# Plain Data
dataByQuantiletoExcel = selectedFirmsToExcel %>%
  addDataByQuantiles("OLQ", relevantParams) %>%
  meanByQuantiles("OLQ", calcVarsByQuantileToExcel, relevantParams)

# Differences agains base scenario
diffDataByQuantiletoExcel = dataByQuantiletoExcel %>% 
  addDiff(calcVarsByQuantileToExcel, c("run"), "recessionMagnitude", 0, percent = FALSE) %>%
  meanByQuantiles("OLQ", calcVarsByQuantileToExcel.d, relevantParams)


###
### Agreggated Data for ALL Firms
###

calcVarsByQuantileToExcel = c("N", "NDeath", "NBorn","TotalSales")
calcVarsByQuantileToExcel.d = sapply(calcVarsByQuantileToExcel, function(x) paste0(x, ".d"))

# Plain Data
dataByQuantiletoExcelAllFirms = fScQ %>%
  addDataByQuantiles("OLQ", relevantParams) %>%
  meanByQuantiles("OLQ", calcVarsByQuantileToExcel, relevantParams)

# Differences agains base scenario
diffDataByQuantiletoExcelAllFirms = dataByQuantiletoExcelAllFirms %>% 
  addDiff(calcVarsByQuantileToExcel, c("run"), "recessionMagnitude", 0, percent = FALSE) %>%
  meanByQuantiles("OLQ", calcVarsByQuantileToExcel.d, relevantParams)



# Write file
wb = createWorkbook()

addWorksheet(wb, "data")
addWorksheet(wb, "diff")
addWorksheet(wb, "aggDataSelFirms")
addWorksheet(wb, "aggDiffSelFirms")
addWorksheet(wb, "aggDataAllFirms")
addWorksheet(wb, "aggDiffAllFirms")
addWorksheet(wb, "params")


writeData(wb, "data", dataToExcel)
writeData(wb, "diff", perFirmDiffToExcel)
writeData(wb, "aggDataSelFirms",dataByQuantiletoExcel)
writeData(wb, "aggDiffSelFirms",diffDataByQuantiletoExcel)
writeData(wb, "aggDataAllFirms",dataByQuantiletoExcelAllFirms)
writeData(wb, "aggDiffAllFirms",diffDataByQuantiletoExcelAllFirms)
writeData(wb, "params", t(getFixedParams(p)), colNames = FALSE, rowNames = TRUE)


saveWorkbook(wb, paste0(fileID, ".xlsx"), overwrite = TRUE)
