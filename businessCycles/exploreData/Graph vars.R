selectedScenarios = fScQ %>%
  
  filter(recessionMagnitude == 0.3 | recessionMagnitude == 0.0,
#         recessionDuration == 1 | recessionDuration == 3,
         recessionStart == 15 | recessionStart == 25,
         entryOnlyAtStart == FALSE
  )

selectedFirms = selectedScenarios %>%
  filter( OLQ != 2, 
          BornTick < recessionStart,
          Death == 40)


createGraph = function(title, htmlName) {
  
selectedFirms %>%
  
  select_at(vars(idxVars, relevantParams, "OLQ", varsToDraw)) %>%
  
  meanByQuantiles("OLQ", varsToDraw, relevantParams) %>%
  
  drawVars(relevantParams, OLQ, tit = title) %>%
  htmlwidgets::saveWidget(paste0(htmlName, ".html"))


# Diference variables
varsToDraw.d = sapply(varsToDraw, function(x) paste0(x, ".d"))

# Table of differences respect to base scenario, grouped by OLQ
diffByFirm = selectedFirms %>% 
  
  # Keep variables to be drawn
  select_at(vars(idxVars, relevantParams, "OLQ", varsToDraw)) %>%
  
  # Add percentage variation per firm respect to base scenario
  addDiff(varsToDraw, c("run", "OLQ"), "recessionMagnitude", 0, percent = FALSE) %>%
  
  # Add Stats by Quantile
  meanByQuantiles("OLQ", varsToDraw.d, relevantParams)


diffByFirm %>%
  drawVars(relevantParams, OLQ, tit = paste0(title, " - Diff:current - Base")) %>%
  htmlwidgets::saveWidget(paste0(htmlName, "_D.html"))

}


###
###
### Call function with different sets
###
###

#Conjunto 1
varsToDraw = c("MarketShare", "Margin", "DesiredAnnualNetInvestment", "Return")
title = "Variables at Different Scenarios"
htmlName = "Vars-1"
createGraph(title, htmlName)

#Conjunto 2
varsToDraw = c("LocalPrice", "IndustryPrice", "MedCost", "Quantity")
title = "Variables at Different Scenarios"
htmlName = "Vars-2"
createGraph(title, htmlName)

#Conjunto 3
varsToDraw = c("MaxFunding", "Capital", "Invest", "DesiredAnnualNetInvestment")
title = "Variables at Different Scenarios"
htmlName = "Vars-3"
createGraph(title, htmlName)


#Conjunto 4
varsToDraw = c("LearningComponent", "FlexibilityCostComponent", "OperatingLevarageComponent", "UsedCapacity")
title = "Variables at Different Scenarios"
htmlName = "Vars-4"
createGraph(title, htmlName)


###
###
### Difference of Difference
###
###
# varsToDraw.dd = sapply(varsToDraw.d, function(x) paste0(x, ".d"))
# 
# diffByFirm %>%
#   
#   addDiff(varsToDraw.d, "", "OLQ", 3, percent = FALSE) %>%
#   
#   select_at(vars(relevantParams, "tick", varsToDraw.dd)) %>%
#   
#   drawVars(relevantParams, tit = "Difference of Differences: OLQ 1 - OLQ 3") %>%
#   htmlwidgets::saveWidget("Vars_DD.html")