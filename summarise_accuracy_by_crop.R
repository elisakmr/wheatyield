library(tidyverse)
# barley
load(file.path(dir_data,"Performance","performance_barley.RData"))
barley_sum <- df_performance %>% 
  group_by(Algorithm, Accuracy) %>% 
  summarise(mean = round(mean(Value), 3))
barley_sum <- spread(barley_sum, Algorithm, mean)
write_csv(barley_sum, file.path(dir_data, 'perfsum_barley.csv'))

# canola
load(file.path(dir_data,"Performance","performance_canola.RData"))
canola_sum <- df_performance %>% group_by(Algorithm, Accuracy) %>% summarise(mean = mean(Value))
canola_sum <- spread(canola_sum, Algorithm, mean)
write_csv(canola_sum, file.path(dir_data, 'perfsum_canola.csv'))

# wheat
load(file.path(dir_data,"Performance","performance_wheat.RData"))
wheat_sum <- df_performance %>% group_by(Algorithm, Accuracy) %>% summarise(mean = mean(Value))
wheat_sum <- spread(wheat_sum, Algorithm, mean)
write_csv(wheat_sum, file.path(dir_data, 'perfsum_wheat.csv'))

