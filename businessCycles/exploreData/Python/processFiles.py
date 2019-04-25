# -*- coding: utf-8 -*-


import numpy as np
import pandas as pd


# =============================================================================
# Creates a Table of the Scenarios in a Run
# 
# This function should be called to create table with scenarios
# Then scenarios should be assigned a name (leaving with empty name the ones
# to be discarded)
# 
# Then setScenariosToData should be applied
# =============================================================================
def createScenariosTable(param):

    paramTab = pd.DataFrame({'scenName':[""],'scenLabel':[""]})
    
    relevantParam = []

    for c in param.columns:
    
        if (c != 'run') & (c != 'randomSeed'):
            
            flatParam = param.loc[:,c].unique()
            
            if len(flatParam) > 1:
                relevantParam.append(pd.DataFrame(data = flatParam, columns=[c]))
    
    paramTab = paramTab.assign(key=1)
    for p in relevantParam:
        p = p.assign(key=1)
        paramTab = paramTab.merge(p, on='key', how='outer')
    
    paramTab = paramTab.drop(columns=['key'])
    
    return paramTab


#Hay que poner los nombres de los escenarios antes de llamar a la función
def setScenariosToData(data, param, scenTab):
    
    sT = scenTab[scenTab.scenName != ""]
    
    runsXScen = pd.DataFrame()
    
    relevantParam = sT.drop(columns=['scenName','scenLabel']).columns
    
    for i, s in sT.iterrows():
        paramsWScen = param.copy()
        for pName in relevantParam:
            paramsWScen = paramsWScen.loc[param[pName] == s[pName]]
        
        paramsWScen['scenario'] = s.scenName
        paramsWScen['scenLabel'] = s.scenLabel
        
        runsXScen = runsXScen.append(paramsWScen)

    return data.merge(runsXScen, on='run')

# =============================================================================
# 
# Firms grouping
# First groups should be created using percentile
# Then firms groups should be combined with scenarios in order to be drawn
#    
# =============================================================================

# Percentile grouping
def setPercentile(df, var , var_lab, bins = 3):
    
    gdf = pd.DataFrame(columns= ["run","FirmNumID", "G_" + var_lab])

    lab =list(map(lambda x : var_lab + '_' + str(x), range(1,bins+1)))
    
    for r in df.run.unique():
        
        rDF = df.loc[(df.run == r) & (df.tick == 1),["run","FirmNumID", var]]
        rDF['G_' + var_lab] = pd.qcut(rDF.loc[:, var], bins, labels = lab)       
        gdf = gdf.append(rDF.drop(columns=var))
 
    return df.merge(gdf, how = "left", on = ["run", "FirmNumID"])


def joinScenarioAndFirmGroup(df, scenCol="scenario", grCol="FirmNumID", conj = "_", newColName = 'scenGr'):
    retDF = df.copy()
    retDF[newColName] = retDF.loc[:,scenCol].apply(str) + conj + retDF.loc[:,grCol].apply(str)
    
    return retDF
           

def write(data, fileName):
    with pd.ExcelWriter(fileName) as writer:
        data.to_excel(writer)

# Get raw Scenarios
def getRawScenarios(param):

    paramSet = dict()

    for c in param.columns:
    
        if (c != 'run') & (c != 'randomSeed'):

            flatParam = param.loc[:,c].unique()
    
            if len(flatParam) > 1:
                paramSet.update({c : flatParam})
    
    return paramSet

# Adds an scenario Dif with the Differences between Scenario A and B
# on variables in varsToDif
    
def setDiferences(df, scenA, scenB, varsToDif, id_vars=["randomSeed","scenario","tick","FirmNumID"]):
    tmp = df.set_index(id_vars)
    tmp = tmp.unstack("scenario")
    
    for v in varsToDif:
        tmp[v,"Dif"]=tmp[v,scenA]-tmp[v,scenB]
        
    return tmp.stack().reset_index()