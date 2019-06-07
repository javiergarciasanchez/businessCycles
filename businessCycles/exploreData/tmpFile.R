varsToDraw = c("Quantity",
               "Profit",
               "FirmMargin",
               "Price",
               "Sales",
               "FlexibilityCost",
               "OperatingLeverage",
               "AcumQuantity",
               "FirstUnitCost",
               "MedCost",
               "VarCost",
               "TotalVarCost",
               "TotalFixedCost",
               "VarCostWoAdj",
               "OperatingLeverageAdjustment",
               "LearningRate",
               "Expon",
               "Capital"
               )

varsToDraw.d = sapply(varsToDraw, function(x) paste0(x, ".d"))

tmp =  fScQ %>%
  
  filter(OLQ != 2,
         recessionMagnitude == 0.3 | recessionMagnitude == 0.0,
         exitOnRecession == FALSE,
         recessionStart == 5,
         recessionDuration == 4) %>%
         
  # Keep variables to be drawn
  select_at(vars(idxVars, relevantParams, "OLQ", varsToDraw)) %>%
  
  # Add percentage variation per firm respect to base scenario
  addDiff(varsToDraw, c("run", "OLQ"), "recessionMagnitude", 0, percent = TRUE) #%>%
  
  # Add Stats by Quantile
  meanByQuantiles("OLQ", varsToDraw.d, relevantParams)



tmp2 = tmp %>%
  filter(tick == 5,
         OLQ == 1,
         random_seed == 1,
         FirmNumID == 3)


library(xlsx)
write.xlsx(tmp2, "./tmp2.xlsx")
