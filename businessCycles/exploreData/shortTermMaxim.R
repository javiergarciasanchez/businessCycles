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

demand = function(q) {
  Delta * q^(- 1 / elast)
}

inverseDemand = function(p) {
  (Delta/p)^elast 
}

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

mc = function(firm){
  
  phi = v(firm , "OperatingLeverage")
  
  l = tau * v(firm, "AcumQuantity") ^ log2(v(firm, "LearningRate")) + z
  
  flexCost = 1 + mu * (meanPhi - phi) / sigmaPhi
  
  return(l * flexCost * (1 - phi))
  
}

runVal = p %>%
  filter(recessionMagnitude == 0.05,
         recessionStart == 5,
         recessionDuration == 1,
         exitOnRecession == TRUE,
         randomSeed == 1) %>%
  select(run) %>%
  .[[1]]

tickVal = 5
