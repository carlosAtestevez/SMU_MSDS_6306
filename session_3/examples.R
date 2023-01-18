
library(tidyverse)

#Filters
mpg %>% filter(class=="pickup")
mpg_data = filter(mpg,class=="pickup")

df_1 = data.frame(id = c(1,2,3),name=c("Carlos","Jose","Eduardo"),age=c(12,23,NA))
df_2 = filter(df_1,!is.na(age))


#Arrange  
df_3 = arrange(df_1,name)
df_3

df_1 %>% arrange(name) %>% print(n=1)

#Exercise with NA

df %>% filter(is.na(Years))


#

mpg %>% select(class,cty,hwy) %>% ggpairs(aes(color=class))


#Select
df_fl = select(flights,year,day)
flights %>% select(year,day)

#Group by 

df = data.frame(Name = c("Jack","Julie","Cali","Sunny","James"),
                Age = c(3,4,2,1,5), Height = c(23,25,30,29,24), 
                Gender = c("Male","Female","Female","Female","Male"))

df %>% group_by(Gender) %>% summarize(MeanHeight = mean(Height))



displ_Factor = cut(mpg$displ, breaks = c(1,4,6,8), labels = c("Low","Medium","High"))

#Exercise Diamonds

gg_miss_var(diamonds)
sapply(diamonds, function(x) sum(is.na(x)))


#Using filters
# library(tidyverse)
# sd1 = sd(diamonds$z)
# md1 = sd(diamonds$z)
# dfg = filter(diamonds,z==0 | z > 20)
# 
# 


#Replacing values
dai_m1 = filter(diamonds,z==0 | z > 20)
sprintf("Number of Diamonds: %i",nrow(dai_m1))

dai_m2 = filter(diamonds,z<3 | z>20)
sprintf("Number of Diamonds: %i",nrow(dai_m2))

dai_m3 = diamonds %>% mutate(z=ifelse(z<3|z>20,NA,z))
dai_m3
sapply(dai_m3, function(x) sum(is.na(x)))


dai_m4 = diamonds
diamonds %>% ggplot(aes(x=z))+geom_histogram()
summary(diamonds$z)
3*sd(diamonds$z)

diamonds %>% ggplot(aes(x=z))+geom_boxplot()

dai_m5 = filter(dai_m3,z>4)
nrow(dai_m5)


#Count
diamonds %>% count(color,cut)

p = diamonds %>% count(color,cut) %>% ggplot(aes(x=color,y=cut))+
  geom_tile(mapping = aes(fill=n))
ggplotly(p)



