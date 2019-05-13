#Firms with Scenarios (relevant parameters values) and Quantiles
addScenarios = function(f, p, relevantParams) {
  tmp = inner_join(f, select(p, "run", relevantParams), by="run") %>%
    mutate(sc = ifelse(recessionMagnitude == 0,
                       "Base",
                       paste0("S:", recessionStart,
                              " M:", recessionMagnitude,
                              " D:", recessionDuration)))
  
  tmp$sc = factor(tmp$sc)
  
  return(tmp)
}

# Calculate differences of relevant variables respect base scenario
# df should include scenario columns
addDifferences = function(df, relevantParams, relevantVars ) {
  
  df = df %>% select(run, tick, FirmNumID, random_seed, relevantParams, sc, relevantVars)
  
  dfBase = filter(df, recessionMagnitude == 0)
  #  dfSS = filter(df, recessionMagnitude != 0)
  
  tmpKey = c("tick", "FirmNumID", "random_seed", relevantParams)
  tmpKey = tmpKey[tmpKey != "recessionMagnitude"]
  
  dfDiff = full_join(df, dfBase, by = tmpKey)
  
  # Keep only one base scenario
  firstSC =  dfDiff %>% filter(sc.x == "Base") %>% select(recessionDuration, recessionStart) %>% unique() %>% slice(1)
  tmpRD =  firstSC$recessionDuration
  tmpRS =  firstSC$recessionStart
  dfDiff = dfDiff %>% filter(sc.x != "Base" |
                               (recessionDuration == tmpRD & recessionStart == tmpRS))
  
  varsSS = sapply(relevantVars, function(x) paste0(x, ".x"))
  varsBase = sapply(relevantVars, function(x) paste0(x, ".y"))
  varsD = sapply(relevantVars, function(x) paste0(x, ".d"))
  dfDiff[varsD]= (dfDiff[varsSS] - dfDiff[varsBase])/dfDiff[varsBase] * 100
  
  # take out .x of variable names
  dfDiff %>% set_names(~sub("\\.x","",.))
  
}


#Calculates stats by group
sumByQuantiles = function(df) {
  df %>%
    group_by(sc, tick, OLQ, random_seed) %>%
    summarise(tmpSum = mean(Quantity.d/Quantity.y *100, na.rm = TRUE)) %>%
    group_by(sc, tick, OLQ) %>%
    summarise(OLQ_Q = mean(tmpSum, na.rm = TRUE))
}
