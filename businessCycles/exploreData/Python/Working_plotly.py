# -*- coding: utf-8 -*-
"""
Created on Fri Mar 22 16:53:32 2019

@author: javie
"""

#Process raw file
path = u"C:\\Users\\javie\OneDrive - AUSTRAL\\Investigación - JGS\\Penrosian version of Recessions and Comp Leapfroging\\SMJ\\R&R\\Data"
rawID =  "test"
newID = "test"
renameFiles(rawID, newID)
f, p = readDataFiles(path, newID)
sc = createScenariosTable(p)

#set scenarios names
names = list(map(lambda x : "S:" + str(x.recessionStart)+"-M:" + str(x.recessionMagnitude)+"-D:" + str(x.recessionDuration), sc.itertuples()))
sc["scenName"]=names
sc["scenLabel"]=names
FirmWSc = setScenariosToData(f, p, sc)
del f, p, sc, rawID, newID, path, names

id_vars=["randomSeed","scenario","tick","FirmNumID"]
vars = ["Quality","Price","Demand","Profit","ExpectedLowLimit","DemandShare"]
SS1cDif = setDiferences(SS1, "SS", "NSS", vars)
SS1_Dif = SS1cDif[SS1cDif.scenario == "Dif"]

r = range(1,15)
pl(SS1, r, "Quality")
pl(SS1, r, "Price")
pl(SS1, r, "Demand")
pl(SS1, r, "DemandShare")
pl(SS1, r, "ExpectedLowLimit")

pl(SS_Dif, r, "Quality")
pl(SS_Dif, r, "Demand")
pl(SS_Dif, r, "DemandShare")
pl(SS_Dif, r, "ExpectedLowLimit")

plMelt(SS3, r, ["Quality","Demand"])

# Create SS3 with only lowest q firms
tmp = SS3.set_index(["randomSeed","scenario","tick"])
tmp["minQ"] = SS3.loc[:, ["randomSeed","scenario","tick","Quality"]].groupby(by=["randomSeed","scenario","tick"]).min()
SS3minQ = tmp[tmp.Quality == tmp.minQ].reset_index()
del tmp

SS3minQcDif = setDiferences(SS3minQ, "SS", "NSS", vars)
SS3minQ_Dif = SS3minQcDif[SS3minQcDif.scenario == "Dif"]
plMelt(SS3minQ_Dif, r, ["Quality","Demand","Profit"])
