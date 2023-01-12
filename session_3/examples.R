
library(ggplot2)
library(ggpubr)
library(dplyr)

mpg %>% filter(class=="pickup")

mpg_data = filter(mpg,class=="pickup")
