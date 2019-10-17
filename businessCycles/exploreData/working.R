#Load functions
#setwd('C:/Users/javie/git/businessCycles/businessCycles/exploreData/')
source('C:/Users/javie/git/businessCycles/businessCycles/exploreData/process.R')

#readFiles
path = "C:/Users/javie/git/businessCycles/businessCycles/output/output/"
setwd(path)

fileID = "2019.ago..27.10_22_54"

f = read_csv(paste0("Firms.", fileID, ".csv"))
p = read_csv(paste0("Firms.", fileID, ".batch_param_map.csv"))

#rm(list=setdiff(ls(), c("f", "p")))

relevantParams = getRelevantParams(p)

#Add calculated variables
fPlus = f %>% 
  
  # Add year of Death
  group_by(run, FirmNumID) %>%
  summarise(Death = max(tick)) %>%
  ungroup() %>%
  inner_join(f, by = c("run", "FirmNumID")) %>%
  
  # Add Margin and optimal Mark Up divided by Mark up
  mutate(Sales = Quantity * LocalPrice,
         Margin = (LocalPrice - MedCost)/LocalPrice,
         excFund = MaxFunding - Invest,
         relatMarkUp = OptimalFullCapacityMarkUp / CurrentMarkUpForPlanning)

idxVars = c("run", "random_seed", "tick", "FirmNumID")

relevantVars = fPlus %>%
  names() %>% 
  setdiff(idxVars)

# Add Quantiles and Scenarios
fScQ = fPlus %>%
  addQuantiles(3) %>%
  addScenarios(p, relevantParams)

quantileVars = c("OLQ", "QQ", "OLQQ")

selectedScenarios = fScQ %>%
  
  filter(recessionMagnitude == 0.05 | recessionMagnitude == 0.0,
         recessionDuration == 1 | recessionDuration == 3,
         entryOnlyAtStart == TRUE
  ) 

selectedFirms = selectedScenarios %>%
  filter( OLQ != 2, 
          BornTick < recessionStart,
          Death == 60)



# Drawing
varsToDraw = c("Margin", "MarketShare", "Return", "MedCost", "LearningComponent")
#varsToDraw = c("Quantity", "UsedCap", "Capital", "MedCost", "Sales")
#varsToDraw = c("MaxFunding", "Capital", "Invest", "excFund")
#varsToDraw = c("OptimalFullCapacityMarkUp", "CurrentMarkUpForPlanning", "relatMarkUp")

# VarsToDraw
selectedFirms %>%
  
  select_at(vars(idxVars, relevantParams, "OLQ", varsToDraw)) %>%
  
  meanByQuantiles("OLQ", varsToDraw, relevantParams) %>%

  drawVars(relevantParams, OLQ, tit = "Variables at Different Scenarios") %>%
  htmlwidgets::saveWidget("Vars.html")


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
    drawVars(relevantParams, OLQ, tit = "Difference vs Base Scenario (current - Base)") %>%
    htmlwidgets::saveWidget("Vars_D.html")


# Difference of Difference
varsToDraw.dd = sapply(varsToDraw.d, function(x) paste0(x, ".d"))

diffByFirm %>%
  
  addDiff(varsToDraw.d, "", "OLQ", 3, percent = FALSE) %>%

  select_at(vars(relevantParams, "tick", varsToDraw.dd)) %>%
  
  drawVars(relevantParams, tit = "Difference of Differences: OLQ 1 - OLQ 3") %>%
  htmlwidgets::saveWidget("Vars_DD.html")

###
###
### Industry variables
###
###
industryVarsToDraw = c("I_Sales", "I_N", "I_Quantity")

aggIdxVars = c("run", "random_seed", "tick")

industryData = selectedScenarios %>%
  group_by(run, tick) %>%
  mutate(I_N = n(),
         I_Sales = sum(Sales),
         I_Quantity = sum(Quantity)) %>%
  ungroup() %>%
  
  select_at(vars(aggIdxVars, relevantParams, industryVarsToDraw)) %>%
  
  group_by_at(vars(relevantParams, "tick")) %>%
  summarise_at( vars(industryVarsToDraw), mean, na.rm = TRUE) %>%
  ungroup()

industryData %>%
  
  drawVars(relevantParams, tit = "Industry Variables at Different Scenarios") %>%
  htmlwidgets::saveWidget("Industry.html")

# Diference variables
industryVarsToDraw.d = sapply(industryVarsToDraw, function(x) paste0(x, ".d"))

# Table of differences respect to base scenario
diffIndustryData = industryData %>%
  
  # Add variation respect to base scenario
  addDiff(industryVarsToDraw, c("run"), "recessionMagnitude", 0, percent = FALSE) %>%

  select_at(vars(relevantParams, tick, industryVarsToDraw.d)) %>%

  drawVars(relevantParams, tit = "Difference vs Base Scenario (current - Base)") %>%
  htmlwidgets::saveWidget("Industry_D.html")


###
###
### Calculated Vars By Quantile - All Firms
###
###

#calcVarsByQuantileToDraw = c("N", "MaxN", "NToMaxN", "NDeath", "NBorn")
calcVarsByQuantileToDraw = c("N", "NDeath", "NBorn","TotalSales")

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

  # Add Stats by Quantile
  meanByQuantiles("OLQ", calcVarsByQuantileToDraw.d, relevantParams)

  
diffDataByQuantileAllFirms %>%
  
  drawVars(relevantParams, OLQ, tit = "Difference vs Base Scenario (current - Base)") %>%
  htmlwidgets::saveWidget("DataByQ_D-All.html")

# Difference of Difference
calcVarsByQuantileToDraw.dd = sapply(calcVarsByQuantileToDraw.d, function(x) paste0(x, ".d"))

diffDataByQuantileAllFirms %>%
  
  addDiff(calcVarsByQuantileToDraw.d, "", "OLQ", 3, percent = FALSE) %>%
 
  select_at(vars(relevantParams, "tick", calcVarsByQuantileToDraw.dd)) %>%
  
  drawVars(relevantParams, tit = "Difference of Differences: OLQ 1 - OLQ 3") %>%
  htmlwidgets::saveWidget("DataByQ_DD-All.html")


####
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
  
  # Add Stats by Quantile
  meanByQuantiles("OLQ", calcVarsByQuantileToDraw.d, relevantParams)


diffDataByQuantileSelectedFirms %>%
  
  drawVars(relevantParams, OLQ, tit = "Difference vs Base Scenario (current - Base)") %>%
  htmlwidgets::saveWidget("DataByQ_D-SelF.html")

# Difference of Difference
diffDataByQuantileSelectedFirms %>%
  
  addDiff(calcVarsByQuantileToDraw.d, "", "OLQ", 3, percent = FALSE) %>%
  
  select_at(vars(relevantParams, "tick", calcVarsByQuantileToDraw.dd)) %>%
  
  drawVars(relevantParams, tit = "Difference of Differences: OLQ 1 - OLQ 3") %>%
  htmlwidgets::saveWidget("DataByQ_DD-SelF.html")
