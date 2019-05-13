
# Distinguish between index columns and value columns
indexVars = c("run",'tick', "FirmNumID", relevantParams, "random_seed")
groupVars = c("OLQ", "QuantQ", "sc")
modelVars = names(fsc)[!(names(fsc) %in% c(indexVars, groupVars))]


# Calculate differences from base scenario (Base scneario: the one with 0 recession magnitude)
fBase = filter(fscQ, recessionMagnitude == 0)
fscQSS = filter(fscQ, recessionMagnitude != 0)
tmpKey = indexVars[!(indexVars %in% c("run", "recessionMagnitude"))]
fDiff = full_join(fscQSS, fBase, by = tmpKey)
rm(tmpKey)

varsX = sapply(modelVars, function(x) paste0(x, ".x"))
varsY = sapply(modelVars, function(x) paste0(x, ".y"))
varsD = sapply(modelVars, function(x) paste0(x, ".d"))
fDiff[varsD]= fDiff[varsX]-fDiff[varsY]

fDiff = fDiff %>% rename(recessionMagnitude = recessionMagnitude.x)