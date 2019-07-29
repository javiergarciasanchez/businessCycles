### Aggregated market data
s = read_csv(paste0("Supply.", fileID, ".csv"))

splus = s %>%
  mutate(TotalUsedCapacity = TotalQuantity/ TotalCapitalAfterEntry,
         TotalSales = Price * TotalQuantity)

ssc = splus %>% 
  addScenarios(p, relevantParams)

filteredssc = ssc %>%
  
  filter(recessionMagnitude == 0.5 | recessionMagnitude == 0.0,
         recessionDuration == 3)

#Vars
aggIdxVars = c("run", "random_seed", "tick")

aggVarsToDraw = c("TotalUsedCapacity", "TotalSales")


filteredssc %>%
  
  select_at(vars(aggIdxVars, relevantParams, aggVarsToDraw)) %>%
  
  group_by_at(vars(relevantParams, "tick")) %>%
  summarise_at( vars(aggVarsToDraw), mean, na.rm = TRUE) %>%
  ungroup() %>%
  
  drawVars(relevantParams, tit = "Total Variables at Different Scenarios") %>%
  htmlwidgets::saveWidget("Tot-Vars.html")

# OverImposed Vars
#overImpVarsToDraw = c("EstimatedPriceForPlanning", "Price", "AvgShortTermMarginalCost" )
overImpVarsToDraw = c("TotalCapitalAfterEntry", "TotalQuantity", "TotalCapitalAfterExit")

filteredssc %>%
  
  select_at(vars(aggIdxVars, relevantParams, overImpVarsToDraw)) %>%
  
  group_by_at(vars(relevantParams, "tick")) %>%
  summarise_at( vars(overImpVarsToDraw), mean, na.rm = TRUE) %>%
  ungroup() %>%
  
  drawOverImposedVars(relevantParams, tit = "Variables at Different Scenarios") %>%
  htmlwidgets::saveWidget("Tot-OvImp-Vars.html")


# Differences
aggVarsToDraw.d = sapply(aggVarsToDraw, function(x) paste0(x, ".d"))
sscG = ssc %>% 
  select_at(vars(relevantParams, aggVarsToDraw, c("run","tick","random_seed"))) %>%
  
  # Add percentage variation per firm respect to base scenario
  addDiff(aggVarsToDraw, c("run"), "recessionMagnitude", 0, percent = FALSE) %>%
  set_names(~sub("\\.x","",.)) %>%
  
  
  group_by_at(vars(relevantParams, "tick")) %>%
  summarise_at( vars(aggVarsToDraw, aggVarsToDraw.d), mean, na.rm = TRUE) %>%
  ungroup()  

