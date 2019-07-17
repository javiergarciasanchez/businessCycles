library(tidyverse)
library(plotly)

# Params not unique
getRelevantParams = function(p) {
  p %>%
    select(-"run", -"randomSeed") %>%
    summarise_all(list(~n_distinct(.))) %>%
    
    select_if(list(~first(. > 1))) %>%
    names()
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
           QQ = ntile(Quantity, n),
           OLQQ = paste0(OLQ, "-", QQ)) %>%
    ungroup() %>%
    select(run, FirmNumID, OLQ, QQ, OLQQ)
  
  q$OLQ = factor(q$OLQ)
  q$QQ = factor(q$QQ)
  q$OLQQ = factor(q$OLQQ)
  
  inner_join(df, q, c("run", "FirmNumID"))
  
}


# Generic difference Calculation
# diffVars: Variables to calculate difference
# keepVars: columns that should be kept, but are not part of index nor diffVars
# varDif: variable to separate x and y
# percent: TRUE or FALSE. Percentage or absolute difference.
#
# Percentage variation SHOULD NOT be used, when values could have different sign
# All columns that are not in diffVars, keepVars o varDif are considered part of index to join
addDiff = function(df, diffVars, keepVars, varDif, y, percent = FALSE){
  
  df.y = filter(df, (!!as.name(varDif))  == y)
  
  tmpKey = setdiff(names(df), c(diffVars, keepVars, varDif))
  dfD = full_join(df, df.y, by = tmpKey)
  
  vars.x = sapply(diffVars, function(x) paste0(x, ".x"))
  vars.y = sapply(diffVars, function(x) paste0(x, ".y"))
  varsD = sapply(diffVars, function(x) paste0(x, ".d"))
  
  if (percent) {
    
    dfD[varsD]= (dfD[vars.x] - dfD[vars.y])/dfD[vars.y]
    
  } else { 
    
    dfD[varsD]= dfD[vars.x] - dfD[vars.y]
    
    }
  
  # Eliminate difference with itself
  dfD = dfD %>%
    filter((!!as.name(paste0(varDif, ".x")))  != y) %>%
    set_names(~sub("\\.x","",.))
  
  return(dfD)

}


#Add data by Quantile
addDataByQuantiles = function(df, Q, relevantParams){
  df %>%
    
    #Data by Tick
    group_by_at(vars(relevantParams, "tick", Q, "random_seed")) %>%
    mutate(N = n(),
           NDeath = sum(if_else(Death == tick, 1, 0)),
           NBorn = sum(if_else(Born == tick, 1, 0)) ) %>%
    
    # Data by Random Seed
    group_by_at(vars(relevantParams, Q, "random_seed")) %>%
    mutate(MaxN = max(N, na.rm = TRUE),
           NToMaxN = N/MaxN) %>%
    
    ungroup()
  
}

#Calculates stats by group
meanByQuantiles = function(df, Q, varsToSum, relevantParams) {
  
  if ("random_seed" %in% names(df)) {
    
    df %>%
      
      group_by_at(vars(relevantParams, "tick", Q, "random_seed")) %>%
      summarise_at( vars(varsToSum), mean, na.rm = TRUE) %>%
      
      group_by_at(vars(relevantParams, "tick", Q)) %>%
      summarise_at( vars(varsToSum), mean, na.rm = TRUE) %>%
      
      ungroup()
    
  } else {
    
    df %>%
      
      group_by_at(vars(relevantParams, "tick", Q)) %>%
      summarise_at( vars(varsToSum), mean, na.rm = TRUE) %>%
      
      ungroup()
    
  }
  
  
}

# Draw variables
drawVars = function(df, relevantParams, v , tit){
  
  if (missing(v)){
    
    df %>%
      addScenariosNames() %>%
      select_at(vars(-relevantParams)) %>%
      
      gather(varToDraw, value, -c(sc, tick)) %>%
      
      ggplot(aes(tick, value, color = "red")) +
      geom_line() +
      facet_grid(varToDraw ~ sc, scales = "free_y") +
      labs(title = tit)
    
    ggplotly()  
    
  } else {
    
    v = enquo(v)    
    
    df %>%
      addScenariosNames() %>%
      select_at(vars(-relevantParams)) %>%
      
      gather(varToDraw, value, -c(sc, tick, !!v)) %>%
      
      ggplot(aes(tick, value, color = !!v)) +
      geom_line() +
      facet_grid(varToDraw ~ sc, scales = "free_y") +
      labs(title = tit)
    
    ggplotly()  
    
  }

}

drawOverImposedVars = function(df, relevantParams, tit) {
  df %>%
    addScenariosNames() %>%
    select_at(vars(-relevantParams)) %>%
    
    gather(varToDraw, value, -c(sc, tick)) %>%
    
    ggplot(aes(tick, value, color = varToDraw)) +
    geom_line() +
    facet_wrap( ~ sc, scales = "free_y") +
    labs(title = tit)
  
  ggplotly() 
}
