'''
Created on 13 feb. 2019

@author: javie
'''

import seaborn as sns
import numpy as np
import pandas as pd

#replace 'Infinity' as higher limit and join lolimit and hilimit to draw a range
def joinLimits(df):

    #Replace 'Infinity'
    retval = df.copy()
    retval.loc[retval.ExpectedHighLimit == 'Infinity', 'ExpectedHighLimit'] = max(retval.ExpectedLowLimit) * 1.5
    retval["ExpectedHighLimit"] = retval["ExpectedHighLimit"].apply(pd.to_numeric)

    dfL = retval.rename(columns = {"ExpectedLowLimit":"ExpectedLimit"})
    dfL.drop('ExpectedHighLimit', axis = 1, inplace = True)

    retval.rename(columns = {"ExpectedHighLimit":"ExpectedLimit"}, inplace = True)
    retval.drop('ExpectedLowLimit', axis = 1, inplace = True)

    return retval.append(dfL)


# Define ploting functions
# Examples:
#    firmsPlot(FtD, range(2, 3), ["Quality"], "scenGr")
#    firmsPlot(FtD, range(4, 5), ["Quality", "Price", "Demand"], "scenGr", facet_kws=dict(sharey=False))
def firmsPlot(df, rSeeds, varsToDraw, firmGroup = "FirmID", facet_kws = {}, cols = 5, rows = 3):

    if 'ExpectedLimit' in varsToDraw: df = joinLimits(df)
    
    fk = dict(sharey=False)
    fk.update(facet_kws)

    argsPlot = {'x': "tick",
                'y': "value",
                'col': "variable",
                'hue' : firmGroup,
                'kind' : "line",
                'facet_kws' : facet_kws
                }
    
    sns.set()
    sns.set_context("poster")

    if len(rSeeds) == 1:
        firmsOneRandSeedPlot(df, rSeeds, varsToDraw, firmGroup, argsPlot, cols)
    elif len(varsToDraw) == 1:
        firmsOneVarPlot(df, rSeeds, varsToDraw, firmGroup, argsPlot, cols, rows)
    else:
        firmsMultiPlot(df, rSeeds, varsToDraw, firmGroup, argsPlot, cols, rows)

    return

def firmsOneRandSeedPlot(df, rSeed, varsToDraw, firmGroup, argsPlot, cols):

    tmpDF = df[df.randomSeed==rSeed[0]].loc[:, ['tick', firmGroup] + varsToDraw]

    tmpDF = tmpDF.melt(id_vars=['tick', firmGroup], value_vars = varsToDraw)

    cols = min(cols, len(varsToDraw))

    argsPlot.update({'data' : tmpDF, 'col_wrap' : cols })

    sns.relplot(**argsPlot)
    
    return

def firmsOneVarPlot(df, rSeeds, varsToDraw, firmGroup, argsPlot, cols, rows):

    tmpDF = df.loc[df.randomSeed.isin(rSeeds), ['randomSeed','tick', firmGroup] + varsToDraw]
 
    n = cols * rows  
    pagsOfSeeds = [rSeeds[i:i+n] for i in range(0,len(rSeeds),n)]
    
    for pagOfSeeds in pagsOfSeeds:
        tmpDF = df.loc[df.randomSeed.isin(pagOfSeeds), ['randomSeed','tick', firmGroup] + varsToDraw]
        argsPlot.update({'y' : varsToDraw[0], 
                         'col' : "randomSeed",
                         'data' : tmpDF,
                         'col_wrap' : cols
                         })        
            
        sns.relplot(**argsPlot)

    return

def firmsMultiPlot(df, rSeeds, varsToDraw, firmGroup, argsPlot, cols, rows):

    #Truncate randoSeeds and vars to proper size
    rSeeds = rSeeds[:cols]
    varsToDraw = varsToDraw[:rows]

    tmpDF = df.loc[df.randomSeed.isin(rSeeds), ['randomSeed','tick', firmGroup] + varsToDraw]
    tmpDF = tmpDF.melt(id_vars=['randomSeed','tick', firmGroup], value_vars = varsToDraw)

    argsPlot.update({'col' : 'randomSeed', 'row' : 'variable', 'data' : tmpDF})
    sns.relplot(**argsPlot)

    return

# firmsPlot(Firms, range(1, 5), ["Price","Quality",'ExpectedLimit'])

#sns.jointplot(x="Price",y="Quality", data= Firms[Firms.tick==50],kind="kde")
#sns.jointplot(x="ExpectedLowLimit",y="Quality", data= Firms[Firms.tick==50],kind="kde")
#sns.jointplot(x="Demand",y="Quality", data= Firms[Firms.tick==50],kind="kde")
