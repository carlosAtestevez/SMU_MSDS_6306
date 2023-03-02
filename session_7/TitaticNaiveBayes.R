#Loading libraries

library(tidyverse)
library(caret)
library(dplyr)
library(e1071)
library(mice)
library(ggthemes)


#Reading the titatic file
df_titanic_raw = read.csv("C:\\Users\\cestevez\\Dropbox\\Cloud PC\\Thinkpad\\Thinkpad Desktop\\Master Data Science SMU\\Class_Sessions\\Data Science Sessions\\Repository\\SMU_MSDS_6306\\session_7\\titanic_train.csv",
                          header = TRUE)


#Filtering and cleaning data
df_titanic_1 = select(df_titanic_raw,Age,Pclass,Sex,Survived)
nr_mean_age = round(mean(df_titanic_1$Age))
#df_titanic_2 = mutate(df_titanic_1,Age = ifelse(is.na(Age),nr_mean_age,Age))
#df_filter_1 = filter(df_titanic_1,!is.na(Age))
#df_titanic_3 = mutate(df_titanic_1,Age = complete(mice(df_titanic_1, method = "pmm"))$Age)
df_titanic_2 = filter(df_titanic_1,!is.na(Age))
df_titanic_2$Psurvived = factor(df_titanic_2$Survived,levels = c(0,1),labels=c("Not survived","Survived"))
df_titanic_2$class = df_titanic_2$Pclass
df_titanic_2 = rename(df_titanic_2,Page = Age)
df_titanic_2$Pclass = factor(df_titanic_2$class,levels=c(1,2,3),labels=c("First class","Second class","Third class"))
df_titanic_3 = select(df_titanic_2,Page,Pclass,class,Psurvived,Sex,Survived) %>%mutate(Gender = ifelse(Sex=="male",1,0))
df_titanic_2 = select(df_titanic_2,Page,Pclass,class,Psurvived,Survived)
df_titanic_3 = select(df_titanic_3,Page,Pclass,Gender,class,Psurvived,Survived)



#Plotting the data before running KNN
df_titanic_2 %>% ggplot(aes(x=Pclass,y=Page,color=Psurvived))+geom_point()+
  labs(title = "Titanic data analysis using Naive Bayes",subtitle = "Survival analysis using Age and Class")+
  xlab("Titanic Passenger Class")+ylab("Passenger Age")+theme_economist_white()+geom_jitter()


#Running Naive Bayes for a passenger of 30 years of age , first class, second class and third class
model_tic_1 = naiveBayes(df_titanic_2[,c(1,3)],df_titanic_2$Survived)
df_30_fc = data.frame(Page = 30,class = 1) #First class pass 30 years old
df_30_sc = data.frame(Page = 30,class = 2) #Second class pass 30 years old
df_30_tc = data.frame(Page = 30,class = 3) #Third class pass 30 years old
df_33_fc = data.frame(Page = 33,class = 1) #Third class pass 30 years old
predict(model_tic_1,df_30_fc,type = "raw")
predict(model_tic_1,df_30_sc,type = "raw")
predict(model_tic_1,df_30_tc,type = "raw")
predict(model_tic_1,df_33_fc,type = "raw")


#Creating training and testing set(Using different Seeds: 4,5,6,7)
set.seed(7)
nr_observations = dim(df_titanic_2)[1]
nr_percentage = 0.70
lst_index_train = sample(1:nr_observations,round(nr_percentage*nr_observations))
df_tic_train_set = df_titanic_2[lst_index_train,]
df_tic_test_set = df_titanic_2[-lst_index_train,]

model_tic_train = naiveBayes(df_tic_train_set[,c(1,3)],df_tic_train_set$Survived)
result_model_1 = predict(model_tic_train,df_tic_test_set[,c(1,3)])
result_table_1 = table(result_model_1,df_tic_test_set$Survived)
confusionMatrix(result_table_1)



nr_iterations = 100
#Creating a Loop for 100 different seeds
df_acom_stat = data.frame(accu=numeric(nr_iterations),sens=numeric(nr_iterations),spec=numeric(nr_iterations))

for(i in 1:nr_iterations){
  set.seed(i)
  lst_index_train = sample(1:nr_observations,round(nr_percentage * nr_observations))
  df_tic_train_set = df_titanic_2[lst_index_train,]
  df_tic_test_set = df_titanic_2[-lst_index_train,]
  model_tic_train = naiveBayes(df_tic_train_set[,c(1,3)],df_tic_train_set$Survived)
  result_model_1 = predict(model_tic_train,df_tic_test_set[,c(1,3)])
  result_table_1 = table(result_model_1,df_tic_test_set$Survived)
  cm = confusionMatrix(result_table_1)
  df_acom_stat$accu[i] = cm$overall[1]
  df_acom_stat$sens[i] = cm$byClass[1]
  df_acom_stat$spec[i] = cm$byClass[2]
}


means_acom = colMeans(df_acom_stat)


#Adding sex to the model, df_titanic_3
set.seed(4)
nr_observations = dim(df_titanic_3)[1]
percentage = 0.7
lst_index_train = sample(1:nr_observations,round(percentage*iterations))
df_train_data = df_titanic_3[lst_index_train,]
df_test_data = df_titanic_3[-lst_index_train,]

model_ticgen_train = naiveBayes(df_train_data[,c(1,3,4)],df_train_data$Survived)
result_model_1 = predict(model_ticgen_train,df_test_data[,c(1,3,4)])
result_table_1 = table(result_model_1,df_test_data$Survived)
confusionMatrix(result_table_1)




nr_iterations = 100
nr_percentage = 0.70
#Creating a Loop for 100 different seeds
df_acom_stat = data.frame(accu=numeric(nr_iterations),sens=numeric(nr_iterations),spec=numeric(nr_iterations))

for(i in 1:nr_iterations){
  set.seed(i)
  lst_index_train = sample(1:nr_observations,round(nr_percentage * nr_observations))
  df_tic_train_set = df_titanic_3[lst_index_train,]
  df_tic_test_set = df_titanic_3[-lst_index_train,]
  model_tic_train = naiveBayes(df_tic_train_set[,c(1,3,4)],df_tic_train_set$Survived)
  result_model_1 = predict(model_tic_train,df_tic_test_set[,c(1,3,4)])
  result_table_1 = table(result_model_1,df_tic_test_set$Survived)
  cm = confusionMatrix(result_table_1)
  df_acom_stat$accu[i] = cm$overall[1]
  df_acom_stat$sens[i] = cm$byClass[1]
  df_acom_stat$spec[i] = cm$byClass[2]
}

means_acom = colMeans(df_acom_stat)



#Multi-nomial example IRIS data 

View(iris)
nr_iterations = 100
nr_percentage = 0.70
nr_observations = dim(iris)[1]
lst_index_train = sample(1:nr_observations,round(nr_percentage*nr_observations))
iris_data = select(iris,Sepal.Length,Sepal.Width,Species)
iris_train = iris_data[lst_index_train,]
iris_test = iris_data[-lst_index_train,]

model_iris = naiveBayes(iris_train[,c(1,2)],iris_train$Species)
result_model_iris= predict(model_iris,iris_test[,c(1,2)])
result_table_1 = table(result_model_iris,iris_test$Species)
confusionMatrix(result_table_1)

#Multi-nomial 100 iterations example IRIS data 


df_acom_stat = data.frame(accu=numeric(nr_iterations),sens=numeric(nr_iterations),spec=numeric(nr_iterations))

for(i in 1:nr_iterations){
  set.seed(i)
  lst_index_train = sample(1:nr_observations,round(nr_percentage*nr_observations))
  iris_data = select(iris,Sepal.Length,Sepal.Width,Species)
  iris_train = iris_data[lst_index_train,]
  iris_test = iris_data[-lst_index_train,]
  model_iris = naiveBayes(iris_train[,c(1,2)],iris_train$Species)
  result_model_iris= predict(model_iris,iris_test[,c(1,2)])
  result_table_1 = table(result_model_iris,iris_test$Species)
  cm = confusionMatrix(result_table_1)
  df_acom_stat$accu[i] = cm$overall[1]
  df_acom_stat$sens[i] = cm$byClass[1]
  df_acom_stat$spec[i] = cm$byClass[2]
}

means_acom_naive = colMeans(df_acom_stat)


#Calculation using KNN model, df_titanic_3
nr_iterations = 100
nr_ks = 100
nr_percentage = 0.7
nr_observations = dim(df_titanic_3)[1]
df_acom_stat = data.frame(item=numeric(nr_iterations), k = numeric(nr_iterations),accu=numeric(nr_iterations),
                          sens=numeric(nr_iterations),spec=numeric(nr_iterations))

mat_acc = matrix(nrow = nr_iterations, ncol = nr_ks)
mat_spec = matrix(nrow = nr_iterations, ncol = nr_ks)
mat_sens = matrix(nrow = nr_iterations, ncol = nr_ks)
for(i in 1:nr_iterations){
  set.seed(i)
  for(j in 1:nr_ks){
    lst_index_train = sample(1:nr_observations,round(nr_observations * nr_percentage))
    df_knn_train = df_titanic_3[lst_index_train,]
    df_knn_test = df_titanic_3[-lst_index_train,]
    knn_result = knn(df_knn_train[,c(1,3,4)],df_knn_test[,c(1,3,4)],df_knn_train$Survived,k=j,prob = TRUE)
    knn_table = table(knn_result,df_knn_test$Survived)
    knn_co = confusionMatrix(knn_table)
    mat_acc[i,j] = knn_co$overall[1]
    mat_sens[i,j] = knn_co$byClass[1]
    mat_spec[i,j] = knn_co$byClass[2]
  }
}
mean_acc = colMeans(mat_acc)
mean_sens = colMeans(mat_sens)
mean_spec = colMeans(mat_spec)

sprintf("Accuracy %f",max(mean_acc))
sprintf("Sensitivity %f",max(mean_sens))
sprintf("Specificity %f",max(mean_spec))

