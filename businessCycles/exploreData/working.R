#Drawing


graph = fDiff %>% 
  filter(
    recessionMagnitude==0.01
#         recessionStart == 25,
#         recessionDuration == 1,
#         tick > 20, tick < 31,
#         FirmNumID == 35,
#         OLQ.x ==3
#         random_seed == 1
         ) %>%
  group_by(sc.x, tick, OLQ.x, random_seed) %>%
  summarise(tmpSum = mean(Quantity.d/Quantity.x, na.rm = TRUE)) %>%
  group_by(sc.x, tick, OLQ.x) %>%
  summarise(OLQ_Q = mean(tmpSum, na.rm = TRUE)) %>%
  ggplot(aes(tick, OLQ_Q, color = factor(OLQ.x))) +
  geom_line() +
  facet_wrap(~sc.x, ncol = 2)

plot(graph)  

fscQ %>% 
  filter(
    #recessionMagnitude==0.01,
         recessionStart == 25,
         recessionDuration == 1,
         tick > 24, tick < 35,
         OLQ ==3
  ) %>%
#  group_by(tick, sc) %>%
#  summarise(Profit = mean(Profit)) %>%
  ggplot(aes(tick, Profit, color = factor(FirmNumID))) +
  geom_line() +
  theme(legend.position = "none")+
  facet_wrap(~sc, ncol = 2)

fDiff %>% 
  filter(recessionMagnitude==0.01,
         recessionStart == 25,
         recessionDuration == 1,
         tick > 24, tick < 35
         #         random_seed == 16
  ) %>%
  group_by(sc.x, tick, OLQ.x, random_seed) %>%
  summarise(OLQ_cant_r = n()) %>%
  group_by(sc.x, tick, OLQ.x) %>%
  summarise(OLQ_cant = mean(OLQ_cant_r)) %>%
  ggplot(aes(tick, OLQ_cant, group = factor(OLQ.x), color = factor(OLQ.x))) +
  geom_line()

fscQ %>% 
  ggplot(aes(tick, Profit, group = factor(OLQ), color = factor(OLQ))) +
  geom_line()+
  geom_line(aes(x = tick, y = mean(Profit), )) +
  facet_wrap(~sc, ncol = 5)


fDiff %>%
  filter(recessionMagnitude==0.3,
         recessionStart == 25,
         recessionDuration == 1,
         random_seed == 16) %>%
  plot_ly( x = ~tick, y = ~Profit.d,
           type= "scatter",
           mode = "lines")
