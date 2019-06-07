library(stats)

Delta = 60 # Demand Parameter
elast = 2 # Elasticity of Demand
lr = 0.9  # Learning Rate
fuc = 20.0 # First Unit Cost
CostOfCapital = 0.08
beta = 1/(1+CostOfCapital)
IniQ = 4.0 # Inicital Capital
maxQ = 2000

aSeries = c(0)
qSeries = c(IniQ)

  
cost = function(a) {
  if (a < 1){
    return(fuc)
  } else {
    expon = log2(lr)
    fuc * a^expon
  }
}

derivC = function(a){
  fuc / a * cost(a)
}
  

P = function(q) {
  Delta * q^(-1/elast)
}

derivP = function(q){
  - P(q) / (q * elast)
}

mr = function(q) {
  P(q) + q * derivP(q)
}

F = function(q, a){
  q * (P(q) - cost(a))
}

s = function(qSeries){
  
  a = 0
  retval = 0
  
  for(i in 1:length(qSeries)){
    q = qSeries[i]
    retval = retval + beta^(i-1) * F(q,a)
    a = a + q
  }
  
  return(retval)
}

bel = function(qMas, q, a){
  mr(q)-cost(a) - beta * (mr(qMas) - cost(q + a)) - beta * qMas * derivC(q+a)
}


solveStepBel = function(aS, qS){
  a = last(aS)
  q = last(qS)
  
  uniroot(bel, c(q,maxQ), q, a, tol = 0.01)
  
}

