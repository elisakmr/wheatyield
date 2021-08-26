library(raster)
library(tidyverse)

r <- raster("C:/Users/wal716/Documents/projects/digiscape/data/yield_machine_learning/prediction_masked2015.tif")
names(r) <- "yield"
df <- as.data.frame(r[])
colnames(df) <- "yield"


df_plot <- df %>% 
  filter(yield > 0) %>% 
  na.omit()


P <-  ecdf(df_plot$yield)

z <- seq(min(df_plot$yield, na.rm = T), max(df_plot$yield, na.rm = T),  length.out = 100) # The values at which we want to evaluate the empirical CDF
p <- 1 - P(z)

df_plot2 <- data.frame(p = p, yield = z)

ggplot(df_plot2, aes(x = p, y = yield)) + 
  xlab("Probability") + ylab("Wheat yield (t/ha)") +
  ylim(c(0.5,5.5)) + 
  geom_line() +
  theme_minimal()
ggsave("W:/projects/yield_machinelearning/figures/data_vis/probability_exceedance_yield_wheat_2015.png", dpi = 300, width = 6, height = 4)
