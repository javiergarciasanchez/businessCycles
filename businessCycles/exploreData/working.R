#Load functions
source('C:/Users/javie/git/businessCycles/businessCycles/exploreData/process.R')

#readFiles
path = "C:/Users/javie/git/businessCycles/businessCycles/output"
setwd(path)

#fileID = "Base"
#fileID = "2019.may..23.15_49_51"
fileID = "2019.may..23.18_04_43"

f = read_csv(paste0("Firms.", fileID, ".csv"))
p = read_csv(paste0("Firms.", fileID, ".batch_param_map.csv"))


relevantParams = getRelevantParams(p)

#Add calculated variables
f = f %>% 
  
  # Add year of Death
  group_by(run, FirmNumID) %>%
  summarise(Death = max(tick)) %>%
  full_join(f, by = c("run", "FirmNumID")) %>%
  
  # Add Margin and optimal Mark Up divided by Mark up
  mutate(OptMUDivMU = OptimalMarkUp / MarkUp,
         FirmMargin = MarkUp - 1)

idxVars = c("run", "random_seed", "tick", "FirmNumID")

relevantVars = f %>%
  names() %>% 
  setdiff(idxVars)

# Add Quantiles and Scenarios
fScQ = f %>%
  addQuantiles(3) %>%
  addScenarios(p, relevantParams)

quantileVars = c("OLQ", "QQ", "OLQQ")

filteredFScQ = fScQ %>%
  
  filter(OLQ != 2,
         recessionMagnitude == 0.05 | recessionMagnitude == 0.0,
         exitOnRecession == TRUE
  ) 


# Drawing
varsToDraw = c("Quantity", "MarketShare", "MedCost", "FirmMargin")
#varsToDraw = c("OpLevMean","OpLevStdDev", "OperatingLeverage", "MaxFunding", "Invest", "FlexibilityCost")
#varsToDraw = c("OperatingLeverage", "MaxFunding", "Invest")
#varsToDraw = c("Invest", "OptimalMarkUp", "MarkUp", "optMUDivMU")


# VarsToDraw
filteredFScQ %>%
  
  select_at(vars(idxVars, relevantParams, "OLQ", varsToDraw)) %>%
  
  meanByQuantiles("OLQ", varsToDraw, relevantParams) %>%

  drawVars(relevantParams, OLQ) %>%
  htmlwidgets::saveWidget("Vars.html")


# Diference variables
varsToDraw.d = sapply(varsToDraw, function(x) paste0(x, ".d"))

# Table of differences respect to base scenario, grouped by OLQ
filteredD = filteredFScQ %>% 

  # Keep variables to be drawn
  select_at(vars(idxVars, relevantParams, "OLQ", varsToDraw)) %>%
  
  # Add percentage variation per firm respect to base scenario
  addDiff(varsToDraw, c("run", "OLQ"), "recessionMagnitude", 0, percent = TRUE) %>%
  set_names(~sub("\\.x","",.)) %>%
  
  # Add Stats by Quantile
  meanByQuantiles("OLQ", varsToDraw.d, relevantParams) %>%
  
  # Eliminate base scenario no difference against itself
  filter(recessionMagnitude != 0)

  filteredD %>%
    drawVars(relevantParams, OLQ) %>%
    htmlwidgets::saveWidget("Vars_D.html")


# Difference of Difference
varsToDraw.dd = sapply(varsToDraw.d, function(x) paste0(x, ".d"))

filteredD %>%
  
  addDiff(varsToDraw.d, "", "OLQ", 3, percent = FALSE) %>%
  
  #Eliminate difference with itself
  filter(OLQ.x != OLQ.y) %>%
  
  select_at(vars(relevantParams, "tick", varsToDraw.dd)) %>%
  
  drawVars(relevantParams) %>%
  htmlwidgets::saveWidget("Vars_DD.html")


### Calculated Vars By Quantile

calcVarsByQuantileToDraw = c("N", "MaxN", "NToMaxN", "NDeath", "NBorn")
#calcVarsByQuantileToDraw = c("N", "NDeath", "NBorn")

dataByQuantile = filteredFScQ %>%
  
  addDataByQuantiles("OLQ", relevantParams) %>%
  
  meanByQuantiles("OLQ", calcVarsByQuantileToDraw, relevantParams)
  

dataByQuantile %>%
  drawVars(relevantParams, OLQ) %>%
  htmlwidgets::saveWidget("DataByQ.html")

# Diference variables
calcVarsByQuantileToDraw.d = sapply(calcVarsByQuantileToDraw, function(x) paste0(x, ".d"))

# Table of differences respect to base scenario, grouped by OLQ
dataByQuantileD = dataByQuantile %>% 

  # Add percentage variation respect to base scenario
  addDiff(calcVarsByQuantileToDraw, c("run", "OLQ"), "recessionMagnitude", 0, percent = TRUE) %>%
  set_names(~sub("\\.x","",.)) %>%
  
  # Add Stats by Quantile
  meanByQuantiles("OLQ", calcVarsByQuantileToDraw.d, relevantParams) %>%
  
  # Eliminate base scenario no difference against itself
  filter(recessionMagnitude != 0)
  
dataByQuantileD %>%
  
  drawVars(relevantParams, OLQ) %>%
  htmlwidgets::saveWidget("DataByQ_D.html")

# Difference of Difference
calcVarsByQuantileToDraw.dd = sapply(calcVarsByQuantileToDraw.d, function(x) paste0(x, ".d"))

dataByQuantileD %>%
  
  addDiff(calcVarsByQuantileToDraw.d, "", "OLQ", 3, percent = FALSE) %>%
  
  #Eliminate Difference with itself
  filter(OLQ.x != OLQ.y) %>%
  
  select_at(vars(relevantParams, "tick", calcVarsByQuantileToDraw.dd)) %>%
  
  drawVars(relevantParams) %>%
  htmlwidgets::saveWidget("DataByQ_DD.html")


### Aggregated market data
s = read_csv(paste0("Supply.", fileID, ".csv"))
ssc = s %>% 
  addScenarios(p, relevantParams)

aggVarsToDraw = c("Dead", "BornFirms")
aggVarsToDraw.d = sapply(aggVarsToDraw, function(x) paste0(x, ".d"))

sscG = ssc %>% 
  select_at(vars(relevantParams, aggVarsToDraw, c("run","tick","random_seed"))) %>%
  
  # Add percentage variation per firm respect to base scenario
  addDiff(aggVarsToDraw, c("run"), "recessionMagnitude", 0, percent = FALSE) %>%
  set_names(~sub("\\.x","",.)) %>%
  
  
  group_by_at(vars(relevantParams, "tick")) %>%
  summarise_at( vars(aggVarsToDraw, aggVarsToDraw.d), mean, na.rm = TRUE) %>%
  ungroup()  

