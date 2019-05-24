fQD %>%
  
  filter(OLQ != 2, 
         recessionMagnitude == 0.1 | recessionMagnitude == 0.0) %>%
  
  sumByQuantiles("OLQ", varsToDraw, relevantParams) %>%
  
  addScenariosNames() %>%
  
  gather(varToDraw, value, 
         -c(sc, recessionMagnitude, recessionStart, recessionDuration, tick, OLQ)) %>%
  
  ggplot(aes(tick,value,color=OLQ)) +
  geom_line() +
  facet_grid(varToDraw ~ sc, scales = "free_y")
ggplotly()

# Difference variables
# old version
fDQSum.d = fDQ %>%
  
  group_by(sc, recessionMagnitude, recessionStart, recessionDuration, 
           tick, OLQ, random_seed) %>%
  mutate(N = n()) %>%
  summarise_at( vars(varsToDraw.d, "N"), mean, na.rm = TRUE) %>%
  
  group_by(sc, recessionMagnitude, recessionStart, recessionDuration,
           tick, OLQ) %>%
  summarise_at( vars(varsToDraw.d, "N"), mean, na.rm = TRUE) 


fDQSum.d %>%
  filter(OLQ != 2,
         recessionMagnitude == 0.1) %>%
  gather(varToDraw, value, 
         -c(sc, recessionMagnitude, recessionStart, recessionDuration,
            tick, OLQ))%>%
  
  ggplot(aes(tick,value,color=OLQ)) +
  geom_line() +
  facet_grid(varToDraw ~ sc, scales = "free_y")
ggplotly()

#Difference of Differences


NEEDS to use new fiunction of differences
see previous drawings

fQDSumD = addDiffOfDiffByOLQ(fQDSum, relevantParams, 1, 3)

fDQSumD %>%
  filter(recessionMagnitude == 0.1) %>%
  gather(varToDraw, value, 
         -c(sc, recessionMagnitude, recessionStart, recessionDuration,
            tick, OLQ))%>%
  
  ggplot(aes(tick,value,color=OLQ)) +
  geom_line() +
  facet_grid(varToDraw ~ sc, scales = "free_y")
ggplotly()


# Calculates Diff of Diff THIS ONE SHOULD BE REPLACE BY THE NEW GENERIC ONE
addDiffOfDiffByOLQ = function(df, relevantParams, x, y){
  
  keys = c("sc",relevantParams, "tick", "OLQ")
  tmpRelevantVars = names(fDQSum)[!(names(fDQSum) %in% keys)]
  
  fDQ.x = fDQSum %>% filter(OLQ == x)
  
  fDQ.y = fDQSum %>% filter(OLQ == y)
  
  tmpKey = keys[keys != "OLQ"]
  fDQD = full_join(fDQ.x, fDQ.y, by = tmpKey)
  
  vars.x = sapply(tmpRelevantVars, function(x) paste0(x, ".x"))
  vars.y = sapply(tmpRelevantVars, function(x) paste0(x, ".y"))
  varsD = sapply(tmpRelevantVars, function(x) paste0(x, ".dd"))
  fDQD[varsD]= fDQD[vars.x] - fDQD[vars.y]
  
  fDQD %>% set_names(~sub("\\.x",paste0(".",x),.))
  fDQD %>% set_names(~sub("\\.y",paste0(".",y),.))
}

#Vars
sscG %>%
  filter( 
    recessionMagnitude == 0.1 | recessionMagnitude == 0.0
  ) %>%
  select(relevantParams, tick, aggVarsToDraw) %>%
  drawVars(relevantParams)

#Differences
sscG %>%
  filter( 
    recessionMagnitude == 0.1
  ) %>%
  select(relevantParams, tick, aggVarsToDraw.d) %>%
  drawVars(relevantParams)

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


plotScenario = function(df, scenario, varToD) {
  df %>%
    filter(sc == scenario,
           varToDraw == !!varToD) %>%
    plot_ly(x = ~tick, y = ~value, color = ~OLQ,
            type = "scatter", mode="line") %>%
    layout(yaxis = list(title=varToD))
}


transform(id = as.integer(factor(sc))) %>%
  group_by(sc) %>%
  plot_ly(x = ~tick, y = ~value, color = ~OLQ, line=~sc, yaxis = ~paste0("y",id)) %>%
  add_lines() %>%
  subplot(nrows = 3, shareX = TRUE)



#filtering
filteredData = fDiffQ %>%
  filter(
    recessionMagnitude==0.1,
    OLQ == 1 | OLQ == 4
    #         recessionStart == 25,
    #         recessionDuration == 1,
    #         tick > 20, tick < 31,
    #         FirmNumID == 35,
    #         random_seed == 1
  )

#Quantity %
graphQ = filteredData %>%
  group_by(sc, tick, OLQ, random_seed) %>%
  summarise(tmpSum = mean(Quantity.d/Quantity.y *100, na.rm = TRUE)) %>%
  group_by(sc, tick, OLQ) %>%
  summarise(OLQ_Q = mean(tmpSum, na.rm = TRUE)) %>%
  ggplot(aes(tick, OLQ_Q, color = factor(OLQ))) +
  geom_line() +
  facet_wrap(~sc, ncol = 2)
dev.new()
plot(graphQ)  

#Med Cost
graphMS = filteredData %>%
  group_by(sc, tick, OLQ, random_seed) %>%
  summarise(tmpSum = mean(MedCost.d/MedCost.y * 100, na.rm = TRUE)) %>%
  group_by(sc, tick, OLQ) %>%
  summarise(OLQ_C = mean(tmpSum, na.rm = TRUE)) %>%
  ggplot(aes(tick, OLQ_C, color = factor(OLQ))) +
  geom_line() +
  facet_wrap(~sc, ncol = 2)
dev.new()
plot(graphMS)

fDiff %>% 
  filter(recessionMagnitude==0.01,
         recessionStart == 25,
         recessionDuration == 1,
         tick > 24, tick < 35
         #         random_seed == 16
  ) %>%
  group_by(sc.x, tick, OLQ.x, random_seed) %>%
  summarise(OLQ_cant_r = n()) %>%
  group_by(sc.x, tick, OLQ.x) %>%
  summarise(OLQ_cant = mean(OLQ_cant_r)) %>%
  ggplot(aes(tick, OLQ_cant, group = factor(OLQ.x), color = factor(OLQ.x))) +
  geom_line()



fDiff %>%
  filter(recessionMagnitude==0.3,
         recessionStart == 25,
         recessionDuration == 1,
         random_seed == 16) %>%
  plot_ly( x = ~tick, y = ~Profit.d,
           type= "scatter",
           mode = "lines")
