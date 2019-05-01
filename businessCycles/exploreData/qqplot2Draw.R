library(reshape2) # to melt and cast, from wide to long
library(ggplot2)

#Set working Directory
#path <- "C:/Users/javie/git/businessCycles/businessCycles/BusinessC_ExploreD/R"
path <- "C:/Users/javie/OneDrive - AUSTRAL/Investigaci?n - JGS/Penrosian version of Recessions and Comp Leapfroging/SMJ/R&R/Data"
setwd(path)

#load csv
FileID <- "test"
FirmsFile <- paste("Firms." , FileID , ".csv", sep="")
ParamsFile <- paste("Firms." , FileID , ".batch_param_map.csv", sep="")
library(readr)
f <- read_csv(FirmsFile)
p <- read_csv(ParamsFile)

prepare_FirmsDF <- function(firmsRaw, runsToDraw, varsToDraw) {
  # Select the run
  df <- firmsRaw[firmsRaw$run %in% runsToDraw ,]
  
  # Select the variables to draw
  df <- df[, c(c("run", "tick", "FirmID"), varsToDraw)]
  
  #Change from wide to long
  df = melt(df, id.vars = c("run", "tick", "FirmID"))
  
  return(df)
  
}

plot_multi_var <- function(firmsRaw, runToDraw, varsToDraw) {
  # Get prepared df
  df <- prepare_FirmsDF(firmsRaw, runToDraw, varsToDraw)
  
  p <- ggplot(df) +
    aes(x = tick, y = value, color = FirmID) +
    geom_line() +
    theme(legend.position="bottom") +
    facet_wrap(~ variable, scales = "free")
  
  return(p)
  
}

plot_multi_run <- function(firmsRaw, runsToDraw, varToDraw) {
  # Get prepared df
  df <- prepare_FirmsDF(firmsRaw, runsToDraw, varToDraw)
  
  p <- ggplot(df) +
    aes(x = tick, y = value, color = FirmID) +
    geom_line() +
    theme(legend.position="bottom") +
    facet_wrap(~ run, scales = "free")
  
  return(p)
  
}