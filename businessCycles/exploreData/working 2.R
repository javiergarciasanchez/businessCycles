source('C:/Users/javie/git/businessCycles/businessCycles/exploreData/shortTermMaxim.R')

q0 = 43000
p0 = 2.9
recMag = 0.4
amc = 1.6
nFirm = 4

estimatedP = amc * nFirm * elast / (nFirm * elast - 1)
estimatedQ = inverseDemand(estimatedP / (1-recMag))

tibble(x = seq(15000,60000, by=100), 
       demand = demand(x), 
       supply = supply(x, q0, p0),
       shiftDemand = (1-recMag) * demand ) %>%
  gather(demand, supply, shiftDemand, key = key, value = value) %>%
  ggplot(aes(x, value, color = key )) +
  geom_line() +
  geom_point(aes(x= estimatedQ, y=estimatedP), color = "blue")
    

