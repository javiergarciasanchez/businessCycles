#Load functions
#setwd('C:/Users/javie/git/businessCycles/businessCycles/exploreData/')
source('C:/Users/javie/git/businessCycles/businessCycles/exploreData/process.R')

#readFiles
path = "C:/Users/javie/git/businessCycles/businessCycles/output/output/"
setwd(path)

fileID = "2019.nov..06.23_45_20"
# "2019.nov..02.15_50_00" Elast 2
#  "2019.oct..26.15_22_10"


f = read_csv(paste0("Firms.", fileID, ".csv"))
p = read_csv(paste0("Firms.", fileID, ".batch_param_map.csv"))

#rm(list=setdiff(ls(), c("f", "p")))

relevantParams = getRelevantParams(p)

#Add calculated variables
fPlus = f %>% 
  
  # Add year of Death
  group_by(run, FirmNumID) %>%
  summarise(Death = max(tick)) %>%
  ungroup() %>%
  inner_join(f, by = c("run", "FirmNumID")) %>%
  
  # Add Margin and optimal Mark Up divided by Mark up
  mutate(Sales = Quantity * LocalPrice,
         Margin = (LocalPrice - MedCost)/LocalPrice,
         excFund = MaxFunding - Invest,
         relatMarkUp = OptimalFullCapacityMarkUp / CurrentMarkUpForPlanning)

idxVars = c("run", "random_seed", "tick", "FirmNumID")

relevantVars = fPlus %>%
  names() %>% 
  setdiff(idxVars)

# Add Quantiles and Scenarios
fScQ = fPlus %>%
  addQuantiles(3) %>%
  addScenarios(p, relevantParams)

quantileVars = c("OLQ", "QQ", "OLQQ")