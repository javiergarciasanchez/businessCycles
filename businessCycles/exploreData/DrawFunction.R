Delta = 600
elast = 5

price = function(q) {
  Delta * q^(- 1 / elast)
}

# Daw profit and q
pricesToDraw = seq(0,10, by=0.1)

tibble(x = pricesToDraw,
       price = sapply(x, price)) %>%
  gather(price, key = key, value = value) %>%
  ggplot(aes(x, value, color = key )) +
  geom_line()
ggplotly()
