library(reshape2) # to melt and cast, from wide to long
library(plotly)

date <- "2019.feb..07.22_43_15"
directory <- "C:/Users/javie/git/demandSide/demandSide/output"


#load csv
FirmsFile <- paste(c(directory,"/Firms.",date,".csv"), collapse = "")
setwd("C:/Users/javie/git/demandSide/demandSide/output")
Firms <- read.csv(FirmsFile)

plot_Firms <- function(firmsDF, runToDraw, varToDraw, showL = TRUE){
  
  df <- firmsDF[firmsDF$run == runToDraw,]
  pTitle = paste0("R: ", runToDraw, ", V: ", varToDraw)
  
  annot <- list(
    text = pTitle,
    x = 0.5,
    y = 1,
    yref = "paper",
    xref = "paper",
    xanchor = "left",
    yanchor = "top",
    showarrow = F
  )
  
  p <- plot_ly(df, x = ~tick) %>% 
    add_lines( y = as.formula(paste0("~",varToDraw)), color = df$FirmID, legendgroup = df$FirmID, showlegend = showL) %>% 
    add_annotations(text = pTitle, x = 0, y = 1,
                    yref = "paper", xref = "paper",
                    xanchor = "left", yanchor = "top",
                    showarrow = F) %>%
    layout(legend = list(orientation = 'h'))
  
  return(p)
  
}

plot_multi_var <- function(firmsDF, runToDraw, varsToDraw){
  
  varsN <- length(varsToDraw)
  
  vars <- list()
  vars[[1]] <- list(v = varsToDraw[1], showLeg = TRUE)
  for (i in 2:varsN){
    vars[[i]] <- list(v = varsToDraw[i], showLeg = FALSE)
  }
  
  var_plots <- lapply(vars, function(var){
    plot_Firms(firmsDF, runToDraw, var$v, var$showLeg)
  })
  
  subplot(var_plots, nrows = trunc(sqrt(length(var_plots))))
  
}

plot_multi_run <- function(firmsDF, runsToDraw, varToDraw){
  
  runsN <- length(runsToDraw)
  
  runs <- list()
  runs[[1]] <- list(r = runsToDraw[1], showLeg = TRUE)
  for (i in 2:runsN){
    runs[[i]] <- list(r = runsToDraw[i], showLeg = FALSE)
  }
  
  run_plots <- lapply(runs, function(run){
    plot_Firms(firmsDF, run$r, varToDraw, run$showLeg)
  })
  
  subplot(run_plots, nrows = trunc(sqrt(length(run_plots))))
  
}