library(tidyverse)

# Process data
createScenariosTable = function(param){
  #library(dplyr)  included in tidyverse
  
  sc = p %>% select( -"randomSeed") %>% unique()
  
  relevantParams = names(sc)[sc %>% summarise_all(funs(n_distinct(.))) > 1]
  
  sc = select(sc, relevantParams)

}

addScenarioToData = function(f, p){
  
  
}