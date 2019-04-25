'''
Created on 14 feb. 2019

@author: javie
'''
import numpy as np
from scipy.optimize import minimize_scalar, minimize
from math import floor, factorial

gini = 0.7
pLambda = (1 + gini) / (2 * gini)
population = 4000
thetaMin = 0.1
cParam = 0.01
gamma = 2
delta = 0.5
z = 1000
discountQ = 0.7
probRichest = 0.99
probPositiveProfit = 0.5

hiP = 300

def demMin(p, realQ):
    return floor(z / (p - cost(realQ)))

# Prob (d = dem)
def probDem(p, realQ, dem):
    if dem > population :
        return 0
    
    p0 = factorial(population) // factorial(population-dem) // factorial(dem) * (thetaMin / loLim(p, perceived(realQ))) ** (pLambda * dem )
    p1 = (1 - (thetaMin / loLim(p, perceived(realQ))) ** pLambda) ** (population - dem)
    return p0 * p1
    
# Prob (d <= dem)
def probDemAcum (p, realQ, dem):
    s = 0
    for d in range(0, dem - 1):
        s = s + probDem(p, realQ, d)   
    return s

def minPrice(realQ):
    return max(perceived(realQ) ** delta * thetaMin, cost(realQ))

def maxPrice(realQ):
    return maxLim() * perceived(realQ) ** delta

def expDemand(p, percQ):
    return population * (thetaMin / loLim(p, percQ)) ** pLambda

def maxLim():
    return thetaMin / (1-(1-probRichest)**(1/population))**(1/pLambda)

def loLim(p, percQ):
    return max(p / percQ ** delta, thetaMin)

def perceived(realQ):
    return realQ * discountQ

def cost(realQ):
    return cParam * realQ ** gamma

def optimalTheoricalPrice(realQ):
    return pLambda * cost(realQ) / (pLambda - 1)

def profit(p, realQ):
    return(p - cost(realQ)) * expDemand(p, perceived(realQ))

def negProfit(x):
    return -profit(x[0], x[1])

def verifyMaxPrice(x):
    return maxPrice(x[1]) - x[0]

def verifyMinDemand(x):
    return 1 - probPositiveProfit - probDemAcum(x[0],x[1], demMin(x[0],x[1]))

def maxProfit():
    res = minimize(negProfit,
                       x0 = (1 , 1),                       
                       constraints = [{"type":"ineq", "fun": verifyMaxPrice},
                                      {"type":"ineq", "fun": verifyMinDemand}],
                       options=dict(maxiter=100))
    
    return res