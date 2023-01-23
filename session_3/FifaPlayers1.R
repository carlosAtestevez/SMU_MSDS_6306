library(tidyverse)
library(GGally)
library(ggthemes)

read_file_fifaplayers = function(){
  
  ldf_fplayers = read.csv("C:\\Users\\cestevez\\Dropbox\\Cloud PC\\Thinkpad\\Thinkpad Desktop\\Master Data Science SMU\\Class_Sessions\\Data Science Sessions\\Repository\\SMU_MSDS_6306\\session_3\\FIFA Players.csv",
                         header = TRUE)
}


#--->Reading the initial file Football players
df_fplayers = read_file_fifaplayers()
sprintf("Number of records Init file: %i",nrow(df_fplayers))
str(df_fplayers)

#--->Part_1_Act_1: Football players position: Left Midfielders and Left Forwards
#We have two different ways to do it--> Using | or using %in%
#df_fpla_lm_lf = filter(df_fplayers,Position == 'LM' | Position == 'LF')
df_fpla_lm_lf = filter(df_fplayers,Position %in% c('LM','LF') )
df_fpla_lm = filter(df_fplayers,Position %in% c('LM') )
df_fpla_lf = filter(df_fplayers,Position %in% c('LF') )
sprintf("Number of records LM&LF: %i",nrow(df_fpla_lm_lf))



#--->Part_1_Act_2:  
#df_p1_ac1 = select(df_fpla_lm_lf,ID,Name,Position,Acceleration,Agility)
df_fpla_lm_lf %>% select(Acceleration,Agility,Position) %>% ggpairs(aes(color=Position))

nr_correlation_total = cor(df_fpla_lm_lf$Agility,df_fpla_lm_lf$Acceleration,use = "complete.obs")
nr_correlation_total              

df_cor_1 = filter(df_fpla_lm_lf, Position=='LF') 
nr_correlation_lf = cor(df_cor_1$Agility,df_cor_1$Acceleration,use = "complete.obs")
nr_correlation_lf 

df_cor_2 = filter(df_fpla_lm_lf, Position=='LM') 
nr_correlation_lm = cor(df_cor_2$Agility,df_cor_2$Acceleration,use = "complete.obs")
nr_correlation_lm      

df_fpla_lm_lf %>% ggplot(aes(x=Acceleration,y=Agility,color=Position))+geom_point()+geom_smooth()


#--->Part_1_Act_4

#Automatic calculation
t_results = t.test(df_fpla_lm$Agility, df_fpla_lf$Agility, var.equal=TRUE)
t_results

#Manual Calculation
mean_lm = mean(df_fpla_lm$Agility)
mean_lf = mean(df_fpla_lf$Agility)
sm = sd(df_fpla_lm$Agility)
sf = sd(df_fpla_lf$Agility)
n_lm = nrow(df_fpla_lm)
n_lf = nrow(df_fpla_lf)
df = n_lm + n_lf - 2
df
sp = sqrt(((n_lm-1)*sm^2+(n_lf-1)*sf^2)/df)
t = (mean_lm-mean_lf)/(sp*(sqrt(1/n_lf+1/n_lm)))

df_fpla_lf %>% ggplot(aes(x=Agility))+geom_histogram()+geom_vline(aes(xintercept=mean(Agility),size=0.5,color="orange"))+
  ggtitle("Distribution Left-Forwards")+theme_economist()
df_fpla_lm %>% ggplot(aes(x=Agility))+geom_histogram()+geom_vline(aes(xintercept=mean(Agility),size=0.01,color="orange"))+
  ggtitle("Distribution Left-Midfielders")+theme_economist()

#EDA Analysis
df_players_1 = df_fplayers
df_factor = cut(df_players_1$Overall, breaks = c(46,62,75,94), labels = c("Bad","Good","Very Good"))
df_players_1 = mutate(df_players_1,overall_desc = df_factor)
df_players_1 %>% select(Vision,ShortPassing,LongPassing,overall_desc) %>% ggpairs(aes(color=overall_desc))
df_players_1 %>% select(Vision,ShortPassing,LongPassing,overall_desc,Skill.Moves) %>% ggpairs(aes(color=overall_desc))

df_fplayers %>% ggplot(aes(x=Overall))+geom_histogram()
#df_players_1 %>% ggplot(aes(x=Skill.Moves))+geom_histogram()

