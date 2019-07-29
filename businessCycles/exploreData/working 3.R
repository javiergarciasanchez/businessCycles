library(plotly)
setwd("C:/Users/javie/git/businessCycles/businessCycles/exploreData")
source('C:/Users/javie/git/businessCycles/businessCycles/exploreData/shortTermMaxim-nv.R')

recMag = 0

repPrice = 3

firmNum = 10
minK = 2000
maxK = 6000
minMC = 1
maxMC = 3

prices = runif(firmNum - repPrice, 3, 7)
repPrices = sample(1:(firmNum-repPrice), repPrice)
prices = c(prices, sapply(1:repPrice, function(x) prices[repPrices[x]] ))

firms = tibble(id= 1:firmNum,
               p = prices,
               k = runif(firmNum, minK, maxK),
               mc = runif(firmNum, minMC, maxMC)
               )

# firms %>% write_excel_csv("firms.csv")



#Set new price
firmID = 3
newP = 5.09
firms = firms %>%
  mutate(p = if_else(id == firmID, newP, p))

df = getQuantAndProfit(firms)

priceRanteToOptimize = c(0.1, 10)

optimize(profitToOptimize, c(0.1, 8), maximum = TRUE, firmsDF = firms, fID = 3)




# Daw profit and q
firmToDraw = 3
pricesToDraw = seq(0,10, by=0.1)

tibble(x = pricesToDraw, 
       prof = sapply(x, 
                     function(p) firmIDProfitPerP(firms, firmToDraw, p) %>%
                       filter(id == firmToDraw) %>%
                       pull(profit)),
       quant = sapply(x, 
                      function(p) firmIDProfitPerP(firms, firmToDraw, p) %>%
                        filter(id == firmToDraw) %>%
                        pull(q))) %>%
  gather(prof, quant, key = key, value = value) %>%
  ggplot(aes(x, value, color = key )) +
  geom_line()
ggplotly()

