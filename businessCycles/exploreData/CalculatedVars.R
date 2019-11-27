
###
###
### Calculated Vars By Quantile - All Firms
###
###

#calcVarsByQuantileToDraw = c("N", "MaxN", "NToMaxN", "NDeath", "NBorn")
calcVarsByQuantileToDraw = c("N", "NDeath", "NBorn","TotalQuantity")

dataByQuantileAllFirms = selectedScenarios %>%
  
  addDataByQuantiles("OLQ", relevantParams) %>%
  
  meanByQuantiles("OLQ", calcVarsByQuantileToDraw, relevantParams)
  
dataByQuantileAllFirms %>% 
  drawVars(relevantParams, OLQ, tit = "Variables at Different Scenarios") %>%
  htmlwidgets::saveWidget("DataByQ-All.html")

# Diference variables
calcVarsByQuantileToDraw.d = sapply(calcVarsByQuantileToDraw, function(x) paste0(x, ".d"))

# Table of differences respect to base scenario, grouped by OLQ
diffDataByQuantileAllFirms = dataByQuantileAllFirms %>%
  
  # Add variation respect to base scenario
  addDiff(calcVarsByQuantileToDraw, c("run"), "recessionMagnitude", 0, percent = FALSE) %>%

  select(relevantParams, tick, OLQ, calcVarsByQuantileToDraw.d, )

  
diffDataByQuantileAllFirms %>%
  
  drawVars(relevantParams, OLQ, tit = "Difference vs Base Scenario (current - Base)") %>%
  htmlwidgets::saveWidget("DataByQ_D-All.html")

# Difference of Difference
# calcVarsByQuantileToDraw.dd = sapply(calcVarsByQuantileToDraw.d, function(x) paste0(x, ".d"))
# 
# diffDataByQuantileAllFirms %>%
#   
#   addDiff(calcVarsByQuantileToDraw.d, "", "OLQ", 3, percent = FALSE) %>%
#  
#   select_at(vars(relevantParams, "tick", calcVarsByQuantileToDraw.dd)) %>%
#   
#   drawVars(relevantParams, tit = "Difference of Differences: OLQ 1 - OLQ 3") %>%
#   htmlwidgets::saveWidget("DataByQ_DD-All.html")

###
###
### Calculated Vars By Quantile - Selected Firms
###
###


dataByQuantileSelectedFirms = selectedFirms %>%
  
  addDataByQuantiles("OLQ", relevantParams) %>%
  
  meanByQuantiles("OLQ", calcVarsByQuantileToDraw, relevantParams)

dataByQuantileSelectedFirms %>% 
  drawVars(relevantParams, OLQ, tit = "Variables at Different Scenarios") %>%
  htmlwidgets::saveWidget("DataByQ-SelF.html")

# Diference variables

# Table of differences respect to base scenario, grouped by OLQ
diffDataByQuantileSelectedFirms = dataByQuantileSelectedFirms %>%
  
  # Add variation respect to base scenario
  addDiff(calcVarsByQuantileToDraw, c("run"), "recessionMagnitude", 0, percent = FALSE) %>%
  
  select(relevantParams, tick, OLQ, calcVarsByQuantileToDraw.d, )


diffDataByQuantileSelectedFirms %>%
  
  drawVars(relevantParams, OLQ, tit = "Difference vs Base Scenario (current - Base)") %>%
  htmlwidgets::saveWidget("DataByQ_D-SelF.html")

# Difference of Difference
# diffDataByQuantileSelectedFirms %>%
#   
#   addDiff(calcVarsByQuantileToDraw.d, "", "OLQ", 3, percent = FALSE) %>%
#   
#   select_at(vars(relevantParams, "tick", calcVarsByQuantileToDraw.dd)) %>%
#   
#   drawVars(relevantParams, tit = "Difference of Differences: OLQ 1 - OLQ 3") %>%
#   htmlwidgets::saveWidget("DataByQ_DD-SelF.html")