library(tidyverse)
library(GGally)
library(ggthemes)
library(stringr)
library(plotly)

read_file_fifaplayers = function(){
  
  ldf_fplayers = read.csv("C:\\Users\\cestevez\\Dropbox\\Cloud PC\\Thinkpad\\Thinkpad Desktop\\Master Data Science SMU\\Class_Sessions\\Data Science Sessions\\Repository\\SMU_MSDS_6306\\session_3\\FIFA Players.csv",
                          header = TRUE)
}


#--->Reading the initial file Football players
df_fifa_players = read_file_fifaplayers()
count_players = nrow(df_fifa_players)

df_fifa_players$height_inches = c()
df_fifa_players$weight_lbs = c()
for(index in 1:count_players){
  str_height = df_fifa_players$Height[index]
  str_weight = df_fifa_players$Weight[index]
  
  #Height conversion
  lst_height = unlist(str_split(str_height,"'"))
  total_heigth = as.numeric(lst_height[1])*12+as.numeric(lst_height[2])
  df_fifa_players$height_inches[index] = total_heigth
  
  #Weight conversion
  num_weigth = as.numeric(unlist(str_replace_all(str_weight,"[^[:digit:]]","")))
  df_fifa_players$weight_lbs[index] = num_weigth
}


df_fifa_players =  filter(df_fifa_players,Position!="")
df_fifa_lb_lm  = filter(df_fifa_players,Position == "LM" | Position == "LB")

df_fifa_lb_lm %>% select(height_inches,weight_lbs,Position) %>% 
  ggpairs(aes(color=Position),columnLabels = c("Height Inches", "Weight Lbs", "Player's position"))+
  labs(title = "Fifa Football Players",
       subtitle = "Relationship between Height and Weight by Position")



df_fifa_lb_lm %>% select(ID,Name,height_inches,weight_lbs,Position) %>% ggplot(aes(x=height_inches,y=weight_lbs))+
  geom_point(aes(col=Position))+geom_smooth()+
  ylab("Weight in LBS")+xlab("Height in Inches")+
  labs(title="Relationship between Height and Weight Football Players")



# 
# df_fifa_players %>% select(ID,Name,height_inches,weight_lbs,Position) %>% ggplot(aes(x=height_inches,y=weight_lbs))+
#                     geom_point(aes(col=Position))+geom_smooth()+
#                     ylab("Weight in LBS")+xlab("Height in Inches")+
#                     labs(title="Relationship between Height and Weight Football Players")
