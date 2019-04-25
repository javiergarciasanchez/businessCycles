# -*- coding: utf-8 -*-
"""
Created on Fri Mar 22 11:51:37 2019

@author: javie
"""
import plotly_express as px
from plotly.offline import plot

def pl(df, r, var):
    tmp = df[df.randomSeed.isin(r)]
    plot(px.line(tmp, height=300 * len(r), x="tick",
             y = var, 
             color="FirmNumID",
             line_dash="scenario",
             facet_row="randomSeed"
             ))


# Several variables melting columns
def plMelt(df, r, vars, id_vars=["randomSeed","scenario","tick","FirmNumID"]):
    tmp = df[df.randomSeed.isin(r)]
    tmp = tmp.melt(id_vars=id_vars, value_vars=vars)
    
    plot(px.line(tmp, height=300 * len(r), x="tick",
             y= "value", 
             color="FirmNumID",
             line_dash="scenario",
             facet_col="variable",
             facet_row="randomSeed"
             ))
