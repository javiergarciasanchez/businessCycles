library(tidyverse)
library(plotly)

# Params not unique
getRelevantParams = function(p) {
  tmp = p %>% select(-"run", -"randomSeed") %>% summarise_all(funs(n_distinct(.)))
  relevantParams = names(tmp)[tmp > 1]
}

#Firms with Scenarios (relevant parameters values)
addScenarios = function(f, p, relevantParams) {
  inner_join(f, select(p, "run", relevantParams), by="run")
}

# Keep one base scenario
keepOneBaseScenario = function(df){
  
  firstSC =  df %>%
    filter(recessionMagnitude == 0) %>%
    select(recessionDuration, recessionStart) %>% 
    unique() %>% 
    slice(1)
  tmpRD =  firstSC$recessionDuration
  tmpRS =  firstSC$recessionStart
  
  df %>% filter(recessionMagnitude != 0 |
                       (recessionDuration == tmpRD & recessionStart == tmpRS))
  
}

# add Scenarios names and drop copies of base 
addScenariosNames = function(df){
  
  # Keep only one base scenario
  if (0 %in% df$recessionMagnitude %>% unique()) {
    df = keepOneBaseScenario(df)
  }
    
  
  # add labels
  df = df %>%
    mutate(sc = ifelse(recessionMagnitude == 0,
                     "Base",
                     paste0("S:", recessionStart,
                            " M:", recessionMagnitude,
                            " D:", recessionDuration)))
  
  df$sc = factor(df$sc)
  
  return(df)
  
}

# Add quantiles, according to Operating Leverage and Quantity at birth
# Should have Age, OperatingLeverage and Quantity
addQuantiles = function(df, n) {
  q = df %>%
    filter(Age == 0) %>%
    group_by(run, tick) %>%
    mutate(OLQ = ntile(OperatingLeverage, n),
           QQ = ntile(Quantity, n)) %>%
    ungroup() %>%
    select(run, FirmNumID, OLQ, QQ)
  
  q$OLQ = factor(q$OLQ)
  q$QQ = factor(q$QQ)
  
  inner_join(df, q, c("run", "FirmNumID"))
  
}


# Generic difference Calculation
# diffVars: Variables to calculate difference
# keepVars: columns that should be kept, but are not part of index nor diffVars
# varDif: variable to separate x and y
# percent: TRUE or FALSE. Percentage or absolute differenc
addDiff = function(df, diffVars, keepVars, varDif, y, percent = TRUE){
  
  idxVars = setdiff(names(df), diffVars)
  
  df.y = filter(df, (!!as.name(varDif))  == y)
  
  tmpKey = setdiff(idxVars, c(keepVars, varDif))
  dfD = full_join(df, df.y, by = tmpKey)
  
  vars.x = sapply(diffVars, function(x) paste0(x, ".x"))
  vars.y = sapply(diffVars, function(x) paste0(x, ".y"))
  varsD = sapply(diffVars, function(x) paste0(x, ".d"))
  
  if (percent) {
    
    dfD[varsD]= (dfD[vars.x] - dfD[vars.y])/dfD[vars.y]
    
  } else { 
    
    dfD[varsD]= dfD[vars.x] - dfD[vars.y]
    
    }
  
  return(dfD)

}

#Calculates stats by group
sumByQuantiles = function(df, Q, varsToSum, relevantParams) {

  df %>%
    
    group_by_at(vars(relevantParams, "tick", Q, "random_seed")) %>%
    mutate(N = n()) %>%
    summarise_at( vars(varsToSum, "N"), mean, na.rm = TRUE) %>%
  
    group_by_at(vars(relevantParams, "tick", Q)) %>%
    summarise_at( vars(varsToSum, "N"), mean, na.rm = TRUE) %>%
    
    ungroup()
    
}

# Draw variables
drawVars = function(df, v ){
  
  if (missing(v)){
    
    df %>%
      addScenariosNames() %>%
      select(-recessionMagnitude, -recessionStart, -recessionDuration) %>%
      
      gather(varToDraw, value, -c(sc, tick)) %>%
      
      ggplot(aes(tick, value, color = "red")) +
      geom_line() +
      facet_grid(varToDraw ~ sc, scales = "free_y")
    
    ggplotly()  
    
  } else {
    
    v = enquo(v)    
    
    df %>%
      addScenariosNames() %>%
      select(-recessionMagnitude, -recessionStart, -recessionDuration) %>%
      
      gather(varToDraw, value, -c(sc, tick, !!v)) %>%
      
      ggplot(aes(tick, value, color = !!v)) +
      geom_line() +
      facet_grid(varToDraw ~ sc, scales = "free_y")
    
    ggplotly()  
    
  }

}