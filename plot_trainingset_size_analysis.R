library(tidyverse)

dir_in <- "W:/projects/yield_machinelearning/data/out_pearcy"
csv_list <- list.files(dir_in, "^samplesize_performance_wheat.*.csv$", full.names = T)


data <- csv_list %>%
  map(read_csv) %>%    # read in all the files individually, using
  # the function read_csv() from the readr package
  reduce(rbind) 

df <- data %>% 
  group_by(Algorithm, Accuracy, size) %>% 
  dplyr::summarise(Value = mean(Value))

df <- filter(df, Accuracy == "Rsquare") %>% 
  ungroup() %>% 
  mutate(Algorithm = recode(Algorithm, "Random Forest" = "RF", "Gaussian Processes" = "GP","Nearest neighbours"= "kNN", 
                  "Support Vector Machine linear" = "SVMl", "Support Vector Machine rbf" = "SVMr", 
                  "Neural Network" = "MLP", "XgBoost"= "XB", "Earth" = "MARS", "Cubist"= "CUB")) %>% 
  mutate(Algorithm = factor(Algorithm, levels = c("GP", "SVMr", "RF", "kNN", "XB", "CUB", "MLP", "MARS", "SVMl")))
                  

ggplot(df, aes(size, Value, group = Algorithm, colour = Algorithm))+
  ylim(0.4, .8) + xlim(c(0,2000))+
  geom_smooth(size=0.8, se=FALSE) +
  ylab("RÂ²")+ xlab("Number of training samples")+
  scale_color_manual(values = c("#004B87", "#41B6E6", "#44693D", "#78BE20", "#6D2077", "#DF1995","#FFB81C", "#E87722", 
                                "#E4002B"))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(title.position = "left", nrow = 2, byrow = TRUE))
ggsave("W:/projects/yield_machinelearning/figures/data_vis/sensitivity_analysis_training_wheat_2015.png", dpi = 300, width = 6.5, height = 5)



### SAME - but for n paddocks-------------------
dir_in <- "W:/projects/yield_machinelearning/data/out_pearcy"
csv_list <- list.files(dir_in, "npaddock_samplesize_performance_wheat.*.csv$", full.names = T)


data <- csv_list %>%
  map(read_csv) %>%    # read in all the files individually, using
  # the function read_csv() from the readr package
  reduce(rbind) 

df <- data %>% 
  group_by(Algorithm, Accuracy, size) %>% 
  dplyr::summarise(Value = mean(Value))

df <- filter(df, Accuracy == "Rsquare") %>% 
  ungroup() %>% 
  mutate(Algorithm = recode(Algorithm, "Random Forest" = "RF", "Gaussian Processes" = "GP","Nearest neighbours"= "kNN", 
                            "Support Vector Machine linear" = "SVMl", "Support Vector Machine rbf" = "SVMr", 
                            "Neural Network" = "MLP", "XgBoost"= "XB", "Earth" = "MARS", "Cubist"= "CUB")) %>% 
  mutate(Algorithm = factor(Algorithm, levels = c("GP", "SVMr", "RF", "kNN", "XB", "CUB", "MLP", "MARS", "SVMl")))


ggplot(df, aes(size, Value, group = Algorithm, colour = Algorithm))+
  #geom_line() +
  ylim(0.3, .8) + xlim(c(30,120))+
  geom_smooth(size=0.8, se=FALSE) +
  ylab("R²")+ xlab("Number of fields in the training set")+
  scale_color_manual(values = c("#004B87", "#41B6E6", "#44693D", "#78BE20", "#6D2077", "#DF1995","#FFB81C", "#E87722", 
                                "#E4002B"))+
  scale_x_continuous(breaks=seq(25,125,25))+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_text(face="bold"))+
  guides(col = guide_legend(title.position = "left", nrow = 2, byrow = TRUE))
ggsave("W:/projects/yield_machinelearning/figures/sensitivity_analysis_npaddocks_training_wheat_2015.png", dpi = 300, width = 6.5, height = 5)

