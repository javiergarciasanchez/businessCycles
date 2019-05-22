#Load functions
source('C:/Users/javie/git/businessCycles/businessCycles/exploreData/process.R')

#readFiles
path = "C:/Users/javie/git/businessCycles/businessCycles/output"
setwd(path)

fileID = "2019.may..22.16_05_37"

f = read_csv(paste0("Firms.", fileID, ".csv"))
p = read_csv(paste0("Firms.", fileID, ".batch_param_map.csv"))
#s = read_csv(paste0("Supply.", fileID, ".csv"))

relevantVars = f %>% names() %>% setdiff(c("run", "tick", "FirmNumID", "random_seed"))

relevantParams = getRelevantParams(p)

fscQ = f %>% 
  addScenarios(p, relevantParams) %>%
  addQuantiles(3) %>%
  mutate(optMUDivMU = OptimalMarkUp / MarkUp,
         marg = MarkUp - 1)

relevantVars = c(relevantVars, "optMUDivMU", "marg")

# Drawing
varsToDraw = c("Quantity", "MarketShare", "MedCost","marg" )
#varsToDraw = c("OpLevMean","OpLevStdDev", "OperatingLeverage", "MaxFunding", "Invest", "FlexibilityCost")
#varsToDraw = c("OperatingLeverage", "MaxFunding", "Invest")
#varsToDraw = c("Invest", "OptimalMarkUp", "MarkUp", "optMUDivMU")

#calcVarsToDraw = c("N", "NToMaxN")
calcVarsToDraw = c("N")

# VarsToDraw
fscQ %>%
  
  filter(OLQ != 2,
         recessionMagnitude == 0.1 | recessionMagnitude == 0.0
         ) %>%
  
  addDataByQuantiles("OLQ", varsToDraw, relevantParams) %>%
  meanByQuantiles("OLQ", c(varsToDraw, calcVarsToDraw), relevantParams) %>%

  drawVars(OLQ)


# Diference variables
varsToDraw.d = sapply(varsToDraw, function(x) paste0(x, ".d"))
calcVarsToDraw.d = sapply(calcVarsToDraw, function(x) paste0(x, ".d"))

# Table of differences respect to base scenario, grouped by OLQ
fQD = fscQ %>% 
  # Choose scenarios to draw
  filter(OLQ != 2, 
         recessionMagnitude == 0.1 | recessionMagnitude == 0.0
         ) %>%
  
  # Drop variables not to be drawn
  select_at(vars(setdiff(names(.), setdiff(relevantVars, varsToDraw)))) %>%
  
  # Add percentage variation per firm respect to base scenario
  addDiff(varsToDraw, c("run", "OLQ", "QQ","OLQQ"), "recessionMagnitude", 0, percent = TRUE) %>%
  set_names(~sub("\\.x","",.)) %>%
  
  # Add Stats by Quantile
  addDataByQuantiles("OLQ", varsToDraw.d, relevantParams) %>%
  meanByQuantiles("OLQ", c(varsToDraw.d, calcVarsToDraw), relevantParams) %>%
  
  
  # Add percentage variation of number of firm per quantile respect to base scenario
  addDiff(calcVarsToDraw, varsToDraw.d, "recessionMagnitude", 0, percent = TRUE) %>%
  set_names(~sub("\\.x","",.)) %>%

  # Eliminate base scenario no difference against itself
  filter(recessionMagnitude != 0) %>%
  
  # Keep vars to draw
  select_at(vars(relevantParams, "tick", "OLQ", varsToDraw.d, calcVarsToDraw.d))



fQD %>% drawVars(OLQ)


# Difference of Difference
varsToDraw.dd = sapply(varsToDraw.d, function(x) paste0(x, ".d"))
calcVarsToDraw.dd = sapply(calcVarsToDraw.d, function(x) paste0(x, ".d"))

fQD %>%

  addDiff(c(varsToDraw.d, calcVarsToDraw.d), c(""), "OLQ", 3, percent = FALSE) %>%
  filter(OLQ.x != OLQ.y) %>%
  select_at(vars(relevantParams, "tick", c(varsToDraw.dd, calcVarsToDraw.dd))) %>%
  
  drawVars()

### Individual Firms

fscQ %>%
  filter(tick == 100, OLQ == 1, recessionMagnitude == 0.1) %>%
  select(random_seed, FirmNumID) %>% unique() #%>% .$FirmNumID

#FirmNumID %in% firmSel$FirmNumID,

fscQ %>%
  
  filter( FirmNumID == 27,
          random_seed == 1,
          recessionMagnitude == 0.1 | recessionMagnitude == 0.0,
          recessionStart == 20,
          tick > 15,
          tick < 50) %>%
  
  select(random_seed, tick, FirmNumID, varsToDraw, relevantParams)  %>%
  
  addDiff(varsToDraw, c(""), "recessionMagnitude", 0, percent = TRUE) %>%
  set_names(~sub("\\.x","",.)) %>%
  
  
  addScenariosNames() %>%
  select(-recessionMagnitude, -recessionStart, -recessionDuration) %>%
  select_at(vars(varsToDraw, "random_seed", "sc", "tick", "FirmNumID")) %>%
  
  gather(varToDraw, value, -c(sc, tick, FirmNumID, random_seed)) %>%
  
  ggplot(aes(tick, value, color = as.factor(FirmNumID))) +
  geom_line() +
  facet_grid(varToDraw ~ sc, scales = "free_y")

ggplotly()
