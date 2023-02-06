library(tidyverse)
library(dplyr)
library(class)
library(caret)
library(e1071)
library(ggthemes)
library(ggforce)


#Selecting seed to keep the same sample 
set.seed(7)
split_percentage = 67 #600/891

#Reading the titatic file

df_titanic_raw = read.csv("C:\\Users\\cestevez\\Dropbox\\Cloud PC\\Thinkpad\\Thinkpad Desktop\\Master Data Science SMU\\Class_Sessions\\Data Science Sessions\\Repository\\SMU_MSDS_6306\\session_6\\titanic_train.csv",
                            header = TRUE)

#Cleaning the data
df_titanic_raw_1 = filter(df_titanic_raw,!is.na(Age))
nrow(df_titanic_raw_1)


#Making adjustment titanic file
df_titanic_1 = select(df_titanic_raw_1,Age,Pclass,Survived) 
df_titanic_1$Survived_desc = factor(df_titanic_1$Survived,levels=c(0,1),labels=c("No Survived","Survived"))
df_titanic_1$Class_desc = factor(df_titanic_1$Pclass,levels=c(1,2,3),labels=c("First class","Second class","Third class"))
len_df_titanic = nrow(df_titanic_1)


#Plotting the data before running KNN
df_titanic_1 %>% ggplot(aes(x=Class_desc,y=Age,color=Survived_desc))+geom_point()+
  labs(title = "Titanic data analysis",subtitle = "Survival analysis using Age and Class")+
  xlab("Titanic Passenger Class")+ylab("Passenger Age")+theme_economist_white()

#Selecting random indexes training dataset
lst_trset_index = sample(1:len_df_titanic,round((split_percentage/100)*len_df_titanic))
df_tit_training = df_titanic_1[lst_trset_index,]
df_tit_test = df_titanic_1[-lst_trset_index,]
sprintf("Length training set %i",nrow(df_tit_training))
sprintf("Length testing set %i",nrow(df_tit_test))

#Runnning KNN Algorithm
results_knn = knn(df_tit_training[,1:2],df_tit_test[,1:2],df_tit_training$Survived_desc,prob = TRUE,
    k=3)
result_table = table(results_knn,df_tit_test$Survived_desc)
confusionMatrix(result_table)

#Estimating the likelihood of a middle-age person in first class
df_est_1 = data.frame(Age=33.0,Pclass=1)
knn(df_titanic_1[,1:2],df_est_1,df_titanic_1$Survived_desc,prob = TRUE,k=3)

#Plotting the data before running KNN
df_titanic_1 %>% ggplot(aes(x=Class_desc,y=Age,color=Survived_desc))+geom_point()+
  geom_point(aes(x="First class",y=33.0),size = 5,color="blue")+geom_jitter()+
  labs(title = "Titanic data analysis",subtitle = "Survival analysis using Age and Class")+
  xlab("Titanic Passenger Class")+ylab("Passenger Age")

#Estimating the likelihood of a middle-age person in third class
df_est_1 = data.frame(Age=33.0,Pclass=3)
knn(df_titanic_1[,1:2],df_est_1,df_titanic_1$Survived_desc,prob = TRUE,k=3)




#Making adjustment titanic file women and Men
df_titanic_2 = select(df_titanic_raw_1,Age,Pclass,Sex,Survived) 
df_titanic_2$Survived_desc = factor(df_titanic_2$Survived,levels=c(0,1),labels=c("No Survived","Survived"))
df_titanic_2$Class_desc = factor(df_titanic_2$Pclass,levels=c(1,2,3),labels=c("First class","Second class","Third class"))
df_titanic_2 =  mutate(df_titanic_2,Gender = ifelse(Sex=="male",1,0))
df_titanic_2 = select(df_titanic_2,Age,Pclass,Gender,Sex,Survived,Class_desc,Survived_desc) 

#Plotting the data before running KNN
df_titanic_2 %>% ggplot(aes(x=Class_desc,y=Age,color=Survived_desc,shape=Sex))+
  geom_point()+geom_jitter()+
  labs(title = "Titanic data analysis",subtitle = "Survival analysis using Age,Class, and Gender")+
  xlab("Titanic Passenger Class")+ylab("Passenger Age")+theme_economist_white()


#Estimating the likelihood of a olf-age man and woman in second class
df_est_2 = data.frame(Age=50,Pclass=2,sex = 1)
knn(df_titanic_2[,1:3],df_est_2,df_titanic_2$Survived_desc,prob = TRUE,k=5)


