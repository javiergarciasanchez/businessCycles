library(tidyverse)

epsilon = 2
psi = 2
meanPhi = 0.6
sigmaPhi = 0.5
mu = 0.05
tau = 2
z = 1

Delta = 600
elast = 2

sup_elast = 2

price = function(q) {
  (1- recMag) * Delta * q^(- 1 / elast)
}


demand = function(p) {
  p = p / (1-recMag)
  (Delta/p)^elast 
}


# firms has p and k columns
getQuantAndProfit = function(firms){
  
  #Group by price
  tmpPerPrice =  firms %>%
    group_by(p) %>%
    summarize(k = sum(k)) %>% 
    add_column(q = 0, resD=0) %>%
    arrange(p)
  
  # Calculate residual demand and quantity per price
  for (i in 1:nrow(tmpPerPrice)) {
    if (i == 1) {
      tmpPerPrice$resD[i] = demand(tmpPerPrice$p[i])
    } else if (tmpPerPrice$resD[i-1] == 0) {
      tmpPerPrice$resD[i] = 0
    } else {
      tmpPerPrice$resD[i] = demand(tmpPerPrice$p[i]) * ( 1 - tmpPerPrice$q[i-1] / tmpPerPrice$resD[i-1])
    }
    
    tmpPerPrice$q[i] = min(tmpPerPrice$k[i], tmpPerPrice$resD[i]) 
    
  }

  # Split q per price among firms with that price   
  tmpPerPrice %>%
    rename(totK = k) %>%
    full_join(firms, by = "p") %>%
    mutate(q = q * k / totK,
           resD = resD * k/totK ) %>%
    select(-totK) %>%
    
    #Calculate profit
    mutate(profit = profit(p, q, mc))
  
}

profit = function(p, q, mc) {
  (p - mc) * q
}


firmIDProfitPerP = function(firms, f, newP) {
  
  firms %>%
    mutate(p = if_else(id == f, newP, p)) %>%
    getQuantAndProfit() 
  
}

profitToOptimize = function(price, firmsDF, fID) {
  firmIDProfitPerP(firmsDF, fID, price) %>%
    filter(id == fID) %>%
    pull(profit)
}

improveFirm = function(firmsDF, fID)  {
  opt = optimize(profitToOptimize, 
           priceRanteToOptimize, 
           maximum = TRUE, 
           firms = firmsDF,
           firmID = fID)
  
  firmsDF = firmsDF %>%
    mutate(p = if_else(id == fID, opt$maximum, p))
}

#-----------------------------------------------------------
  
sup_Delta = function(q0, p0){
  p0 / q0^( 1 / sup_elast)
}

supply = function(q, q0, p0) {
  sup_Delta(q0, p0) * q^( 1 / sup_elast)
}

ms = function(p){
  
  retval = 0
  
  firms = getFirms()
  
  for (firm in firms){
    retval = retval + (p/(p-mc(firm))+psi)^(-1)
  }
  
  return(retval * (epsilon + psi))
  
}

getFirms = function(){

  f %>%
    filter(run == runVal,
           tick == tickVal) %>%
    select(FirmNumID)
  
}

v = function(firm, v){
  
  f %>%
    filter(run == runVal,
           tick == tickVal,
           FirmNumID == firm) %>%
    select_at(vars(v))
  
}

