# -*- coding: utf-8 -*-
"""
Created on Thu Mar 28 15:57:09 2019

@author: javie
"""


# SymPy Imports
from sympy import init_session
init_session()


gini = 0.7
pLambda = (1 + gini) / (2 * gini)
population = 4000
thetaMin = 0.1
cParam = 0.01
gamma = 2
delta = 0.5
cFijo = 0
discountQ = 0.7
probRichest = 0.99

p, q, th = symbols('p q th', real=True)
dem = symbols('dem', integer=True)

from sympy.stats import P, E, variance, Binomial, cdf, density, Pareto

pa = Pareto("pa", thetaMin, pLambda)
bi = Binomial("bi", population, 1-cdf(pa)(th))
