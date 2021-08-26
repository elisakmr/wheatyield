if ((Sys.info()["nodename"]) == "sc-05-cdc") {
  dir_data <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/data"
  dir_plot <- "//OSM/CBR/AF_DIGI_RS/work/projects/yield_machinelearning/figures"
} else if ((Sys.info()["nodename"]) == "MULLET-SL") {
  dir_data <- "X:/projects/yield_machinelearning/data"
  dir_plot <- "X:/projects/yield_machinelearning/figures"
}

library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
### create data

crop_list <- c("barley", "canola", "wheat")
for (crop in crop_list){
df_now <- read_csv(file.path(dir_data, "Performance", paste0("sum_performance_now_", crop, ".csv")))
df_w <- read_csv(file.path(dir_data, "Performance", paste0("sum_performance_", crop, ".csv"))) %>% 
  filter(Accuracy == "RMSPE") %>%
  filter(Algorithm == "Ensemble weight r2") %>%
  group_by(Algorithm, Accuracy)

df_student <- df_now %>%
  filter(Accuracy == "RMSPE") %>%
  filter(Algorithm == "Ensemble weight r2") %>%
  group_by(Algorithm, Accuracy) %>% 
  cbind(df_w) %>% 
  select(Value, Value1, Algorithm) 
write.csv(df_student, file.path(dir_data, "data_figures", paste0("df_student_", crop, ".csv")))
}

###  data observation
library(ggthemes)
library(reshape)
library(ggsci)
library(ggbeeswarm)
library(ggpubr)

crop_list <- c("barley", "canola", "wheat")
df_wil <- data.frame()
for (crop in crop_list){
df_student <- read_csv(file.path(dir_data, "data_figures", paste0("df_student_", crop, ".csv")))
  BOXPLOT
 mytheme = theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
       panel.background = element_blank(), axis.line.x = element_blank(), axis.title.x=element_blank(), 
       axis.text.y = element_text(colour="grey"), axis.ticks.y= element_line(colour="grey"))
 
 p_list[[crop]] <- ggplot(melt(100*df_student[,c(-1,-2, -5)]), aes(x = variable, y = value)) + 
   geom_boxplot(outlier.colour = NULL, aes_string(colour="variable", fill="variable"), width=0.5)  +
   labs(y = "RMSPE (%)", x="") +
   stat_summary(geom = "crossbar", width=0.45, fatten=0, color="white", 
              fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) } ) +
   scale_y_continuous(breaks = seq(40, 160, 40))+
   scale_x_discrete(expand = c(0.05, 0.05)) +
   theme_minimal() +
   theme(axis.text.x = element_blank(),
         axis.text.y =  element_text(size = 12),
         axis.ticks = element_blank(), 
         axis.ticks.y = element_blank(),
         legend.position = "none", panel.grid.major.x = element_blank(), 
         panel.border = element_blank(),  axis.title.y=element_text(size=12, angle = 0, margin=margin(r=10)),
         plot.margin = margin(t = 40, r = 10, b = 0, l = 0))+
   ylim(20,100)
 p_all <- ggarrange(p_list$barley, p_list$canola, p_list$wheat,
                    labels=c("Barley", "Canola", "Wheat"),
                    font.label = list(size = 11, color = "black", face = "bold"),
                    vjust = 1.7,
                    hjust = - 3.7,
                    ncol = 3, nrow = 1,
                    common.legend = TRUE, legend = "bottom")
 p_all
 ggsave("X:/projects/yield_machinelearning/figures/algorithm_training/var_imp/imp_weather_svr.png",
        width = 9.53, height = 4.6, dpi = 400)
 
 ### Plot for wheat -----
 library(wesanderson)
 p <- ggplot(melt(100*df_student[,c(-1,-2, -5)]), aes(x = variable, y = value)) + 
   geom_boxplot(outlier.colour = NULL, aes_string(colour="variable", fill="variable"), width=0.5)  +
   scale_fill_manual(values=c("palegreen2","lightskyblue"))+
   scale_color_manual(values=c("palegreen2","lightskyblue"))+
   labs(y = "RMSPE (%)", x="") +
   stat_summary(geom = "crossbar", width=0.45, fatten=0, color="white", 
                fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) } ) +
   scale_y_continuous(breaks = seq(20, 100, 20), lim = c(20,100))+
   #scale_x_discrete(expand = c(0.1, 0.1)) +
   theme_minimal() +
   scale_x_discrete(labels=c("Value" = "Without weather variable", "Value1" = "With weather variables"))+
   theme(axis.text.y =  element_text(size = 12),
         axis.text.x = element_text(),
         axis.ticks = element_blank(), 
         axis.ticks.y = element_blank(),
         legend.position = "none",
         panel.grid.major.x = element_blank(), 
         panel.border = element_blank(),
         axis.title.y=element_text(size=12, angle = 0, margin=margin(r=10)),
         plot.margin = margin(t = 40, r = 10, b = 0, l = 0))
 
 p

 ggsave("X:/projects/yield_machinelearning/figures/imp_weather_svr_wheat.png",
        width = 6.53, height = 4.6, dpi = 400)
  # NORMALITY

    # compute the difference
#library(data.table)
#d <- unlist(df_student[,2]-df_student[,3])
d1 <- unlist(df_student[,3])
d2 <- unlist(df_student[,4])



    # Shapiro-Wilk normality test for the differences
# shapiro.test(d) # => p-value << 0.05
# 
#     # visual assessment : data not normal 
# par(mfrow=c(1,2))
# hist(d1,freq=F)
# qqnorm(d1)
# qqline(d1)
# library(ggpubr)
# ggqqplot(d2)
# 
### WILCOXON test (sign + rank)

w <- wilcox.test(x = d2, y = d1 ,alternative = "less", paired = TRUE)
if (w$p.value < 0.05) {
  out <- data.frame("< 0.05", crop)
} else {
  out <- c(unlist(w$p.value), crop)
  out <- t(data.frame (out))
}
colnames(out) <- c("p value","Crop")
df_wil <- rbind(df_wil,out)
colnames(df_wil) <- c("p value","Crop")

}
df_wil
write.csv(df_wil, "wilcoxon_test.csv", row.names=F, col.names=T)

