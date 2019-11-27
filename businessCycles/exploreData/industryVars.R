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
