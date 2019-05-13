#Working exploporatory

tmp = fDiff %>%
  filter(
    recessionMagnitude==0.1,
    recessionStart == 25,
    recessionDuration == 1,
#    run == 201,
    random_seed == 1,
#    tick == 28,
    tick > 20,
    tick < 31,
    FirmNumID == 35,
    OLQ.x ==3) %>%
  select(FirmNumID, tick, Profit.x, Profit.y, Profit.d, recessionMagnitude, Born.d, random_seed) %>% arrange(FirmNumID, tick)



#Keep only Difference columns
shortFDiff = fDiff %>% select(tmpKey, vars, "run.x", "recessionMagnitude", "run.y")

# Add Firm string labels  
fDiff["FirmID"] = sapply(fDiff["FirmNumID"], function(x) paste0("F:", x))

#working space
fsc %>%
  group_by(run, tick) %>%
  mutate(OLQ = ntile(OperatingLeverage, 3),
         OLmean = mean(OperatingLeverage),
         QuantQ = ntile(Quantity, 3),
         Qmean = mean(Quantity)) %>%
  ungroup()

fscQ %<>% 
  group_by(run, FirmNumID) %>% 
  mutate(OL_tmp = if_else(tick==Born, OLQ, NA_integer_),
         Q_tmp =  if_else(tick==Born, QuantQ, NA_integer_),
         Born_OLQ = mean(OL_tmp, na.rm = TRUE),
         Born_QuantQ = mean(Q_tmp, na.rm = TRUE)) %>%
  select( -OL_tmp, -Q_tmp)


tmp = fscQ %>% 
  group_by(run, FirmNumID) %>% 
  filter(n_distinct(OLQ)>1) %>%
  select(run, tick, FirmNumID, OLmean, OperatingLeverage, OLQ, Born_OLQ, QuantQ, Born_QuantQ)

fDiff %>%
  filter(
    recessionMagnitude != 0,
    recessionStart == 25,
    recessionDuration == 1,
    !is.na(run.x),
    !is.na(run.y),
    random_seed == 1,
    FirmNumID == 191,
    tick > 20
  ) %>%
  select("random_seed", "tick", "run.x", "run.y", "FirmNumID",
         starts_with("Profit"), 
         starts_with("recession"))

inner_join(fscQ %>% filter(tick == 24 & recessionMagnitude > 0 & recessionStart == 25),
           fscQ %>% filter(tick == 27, recessionMagnitude > 0 & recessionStart == 25),
           by=c("run","FirmNumID")) %>%
  select("run", "FirmNumID", "Profit.x", "Profit.y")

#check different quantiles for same firm
fscQ %>% group_by(run, FirmNumID) %>% summarize(OLQs = n_distinct(OLQ))
