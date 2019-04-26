library(tidyverse)
library(plotly)

#readFiles
path = "C:/Users/javie/git/businessCycles/businessCycles/output"
setwd(path)
  
#library(readr) included in tidyverse

fileID = "2019.abr..25.18_14_16"
f <- read_csv(paste0("Firms.", fileID, ".csv"))
p <- read_csv(paste0("Firms.", fileID, ".batch_param_map.csv"))

# Params not unique
tmp = p %>% select(-"run", -"randomSeed") %>% unique()
relevantParams = names(tmp)[tmp %>% summarise_all(funs(n_distinct(.))) > 1]
rm(tmp)

#Firms with Scenarios (relevant parameters values)
fsc = inner_join(f, select(p, "run", relevantParams), by="run")

# Distinguish between index columns and value columns
index = c("run",'tick',"FirmNumID", relevantParams, "random_seed")
vars = names(fsc)[!(names(fsc) %in% index)]

# Add quantiles
groupKey = c(relevantParams, "run", "tick")

fscG = fsc %>% 
  group_by(run, tick,
           recessionDuration, recessionMagnitude, recessionStart)

fscG2 = fsc %>% 
  group_by_at(.vars = vars(one_of(groupKey)))

fscQ = fscG %>% mutate(OLF = ntile(OperatingLeverage, 3))

# Calculate differences from base scenario (Base scneario: the one with 0 recession magnitude)
fBase = filter(fsc, recessionMagnitude == 0)
fSS = filter(fsc, recessionMagnitude != 0)
join_key = index[!(index %in% c("run","recessionMagnitude"))]

fDiff = full_join(fSS, fBase, by = join_key)
varsX = sapply(vars, function(x) paste0(x, ".x"))
varsY = sapply(vars, function(x) paste0(x, ".y"))
varsD = sapply(vars, function(x) paste0(x, ".d"))

fDiff[varsD] = fDiff[varsX]-fDiff[varsY]
fDiff = fDiff %>% rename(recessionMagnitude = recessionMagnitude.x)

#Keep only Difference columns
shortFDiff = fDiff %>% select(join_key, vars, "run.x", "recessionMagnitude", "run.y")

# Add Firm string labels  
fDiff["FirmID"] = sapply(fDiff["FirmNumID"], function(x) paste0("F:", x))

# Explore data
fDiff %>%
  filter(recessionMagnitude==0.3,
         recessionStart == 25,
         recessionDuration == 1,
         random_seed == 1,
         FirmNumID == 141,
         tick > 20) %>%
  select("tick", "run.x", "run.y", "FirmNumID", starts_with("Profit"))

inner_join(fsc %>% filter(tick == 24 & recessionMagnitude > 0 & recessionStart == 25),
           fsc %>% filter(tick == 27, recessionMagnitude > 0 & recessionStart == 25),
           by=c("run","FirmNumID")) %>%
  select("run", "FirmNumID", "Profit.x", "Profit.y")

#Drawing
fDiff %>%
  filter(recessionMagnitude==0.3,
         recessionStart == 25,
         recessionDuration == 1,
         random_seed == 1) %>%
  plot_ly( x = ~tick) %>% 
  add_lines( y = ~AcumProfit)