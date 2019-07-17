#Load functions
source('C:/Users/javie/git/businessCycles/businessCycles/exploreData/process.R')

#readFiles
path = "C:/Users/javie/git/businessCycles/businessCycles/output"
setwd(path)

fileID = "2019.jul..16.20_49_06"

f = read_csv(paste0("Firms.", fileID, ".csv"))
p = read_csv(paste0("Firms.", fileID, ".batch_param_map.csv"))


relevantParams = getRelevantParams(p)

#Add calculated variables
fPlus = f %>% 
  
  # Add year of Death
  group_by(run, FirmNumID) %>%
  summarise(Death = max(tick)) %>%
  ungroup() %>%
  full_join(f, by = c("run", "FirmNumID")) %>%
  
  # Add Margin and optimal Mark Up divided by Mark up
  mutate(Sales = Quantity * Price,
         UsedCap = Quantity / Capital,
         PrMinShMC = Price - ShortTermMarginalCost)

idxVars = c("run", "random_seed", "tick", "FirmNumID")

relevantVars = fPlus %>%
  names() %>% 
  setdiff(idxVars)

# Add Quantiles and Scenarios
fScQ = fPlus %>%
  addQuantiles(3) %>%
  addScenarios(p, relevantParams)

quantileVars = c("OLQ", "QQ", "OLQQ")

filteredFScQ = fScQ %>%
  
  filter(OLQ != 2,
         recessionMagnitude == 0.6 | recessionMagnitude == 0.0,
         recessionDuration == 3
  ) 


# Drawing
#varsToDraw = c("Quantity", "UsedCap", "Capital", "MedCost", "Price","Sales")
varsToDraw = c("UsedCap", "ShortTermMarginalCost", "Price", "MedCost", "PrMinShMC")
#varsToDraw = c("UsedCap", "MedCost", "PrMinShMC")

# VarsToDraw
filteredFScQ %>%
  
  select_at(vars(idxVars, relevantParams, "OLQ", varsToDraw)) %>%
  
  meanByQuantiles("OLQ", varsToDraw, relevantParams) %>%

  drawVars(relevantParams, OLQ, tit = "Variables at Different Scenarios") %>%
  htmlwidgets::saveWidget("Vars.html")


# Diference variables
varsToDraw.d = sapply(varsToDraw, function(x) paste0(x, ".d"))

# Table of differences respect to base scenario, grouped by OLQ
filteredD = filteredFScQ %>% 

  # Keep variables to be drawn
  select_at(vars(idxVars, relevantParams, "OLQ", varsToDraw)) %>%
  
  # Add percentage variation per firm respect to base scenario
  addDiff(varsToDraw, c("run", "OLQ"), "recessionMagnitude", 0, percent = TRUE) %>%
  
  # Add Stats by Quantile
  meanByQuantiles("OLQ", varsToDraw.d, relevantParams)


filteredD %>%
    drawVars(relevantParams, OLQ, tit = "Difference vs Base Scenario (current - Base)") %>%
    htmlwidgets::saveWidget("Vars_D.html")


# Difference of Difference
varsToDraw.dd = sapply(varsToDraw.d, function(x) paste0(x, ".d"))

filteredD %>%
  
  addDiff(varsToDraw.d, "", "OLQ", 3, percent = FALSE) %>%

  select_at(vars(relevantParams, "tick", varsToDraw.dd)) %>%
  
  drawVars(relevantParams, tit = "Difference of Differences: OLQ 1 - OLQ 3") %>%
  htmlwidgets::saveWidget("Vars_DD.html")


### Calculated Vars By Quantile

#calcVarsByQuantileToDraw = c("N", "MaxN", "NToMaxN", "NDeath", "NBorn")
calcVarsByQuantileToDraw = c("N", "NDeath", "NBorn")

dataByQuantile = filteredFScQ %>%
  
  addDataByQuantiles("OLQ", relevantParams) %>%
  
  meanByQuantiles("OLQ", calcVarsByQuantileToDraw, relevantParams)
  
dataByQuantile %>%
  drawVars(relevantParams, OLQ, tit = "Variables at Different Scenarios") %>%
  htmlwidgets::saveWidget("DataByQ.html")

# Diference variables
calcVarsByQuantileToDraw.d = sapply(calcVarsByQuantileToDraw, function(x) paste0(x, ".d"))

# Table of differences respect to base scenario, grouped by OLQ
dataByQuantileD = dataByQuantile %>% 

  # Add variation respect to base scenario
  addDiff(calcVarsByQuantileToDraw, c("run"), "recessionMagnitude", 0, percent = FALSE) %>%

  # Add Stats by Quantile
  meanByQuantiles("OLQ", calcVarsByQuantileToDraw.d, relevantParams)

  
dataByQuantileD %>%
  
  drawVars(relevantParams, OLQ, tit = "Difference vs Base Scenario (current - Base)") %>%
  htmlwidgets::saveWidget("DataByQ_D.html")

# Difference of Difference
calcVarsByQuantileToDraw.dd = sapply(calcVarsByQuantileToDraw.d, function(x) paste0(x, ".d"))

dataByQuantileD %>%
  
  addDiff(calcVarsByQuantileToDraw.d, "", "OLQ", 3, percent = FALSE) %>%
 
  select_at(vars(relevantParams, "tick", calcVarsByQuantileToDraw.dd)) %>%
  
  drawVars(relevantParams, tit = "Difference of Differences: OLQ 1 - OLQ 3") %>%
  htmlwidgets::saveWidget("DataByQ_DD.html")