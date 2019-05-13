#Load functions
source('C:/Users/javie/git/businessCycles/businessCycles/exploreData/process.R')

#readFiles
path = "C:/Users/javie/git/businessCycles/businessCycles/output"
setwd(path)

fileID = "2019.abr..30.17_01_26"
#fileID = "2019.abr..25.18_14_16"
f <- read_csv(paste0("Firms.", fileID, ".csv"))
p <- read_csv(paste0("Firms.", fileID, ".batch_param_map.csv"))

relevantVars = names(f)[!(names(f) %in% c("run", "tick", "FirmNumID", "random_seed"))]

varsToDraw = c("Quantity", "Profit", "MedCost","Price")

relevantParams = getRelevantParams(p)

fscQ = f %>% 
  addScenarios(p, relevantParams) %>%
  addQuantiles(3)

fQD = fscQ %>%
  select_at(vars(setdiff(names(.), setdiff(relevantVars, varsToDraw)))) %>%
  addDiff(varsToDraw, c("run", "OLQ", "QQ"), "recessionMagnitude", 0, percent = TRUE) %>%
  set_names(~sub("\\.x","",.))

# Drawing

# VarsToDraw
fQD %>%
  filter(OLQ != 2, 
         recessionMagnitude == 0.1 | recessionMagnitude == 0.0) %>%
  sumByQuantiles("OLQ", varsToDraw, relevantParams) %>%
  drawVars(OLQ)


# Diference variables
varsToDraw.d = sapply(varsToDraw, function(x) paste0(x, ".d"))

fQD %>%
  filter(OLQ != 2, 
         recessionMagnitude == 0.1) %>%
  sumByQuantiles("OLQ", varsToDraw.d, relevantParams) %>%
  drawVars(OLQ)
  

# Difference of Difference
varsToDraw.dd = sapply(varsToDraw.d, function(x) paste0(x, ".d"))

fQD %>%
  filter(OLQ != 2, recessionMagnitude == 0.1) %>%
  sumByQuantiles("OLQ", varsToDraw.d, relevantParams) %>%
  addDiff(c(varsToDraw.d, "N"), c(""), "OLQ", 3, percent = FALSE) %>%
  filter(OLQ.x != OLQ.y) %>%
  select_at(vars(relevantParams, "tick", varsToDraw.dd)) %>%
  
  drawVars()
