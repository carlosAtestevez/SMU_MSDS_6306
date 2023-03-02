
library(tidyverse)
library(dplyr)
library(class)
library(caret)
library(ggthemes)
library(e1071)

set.seed(7)

#In this example we will use Sepal with and length to predict
#The IRIS flower type
split_per = 0.70
iris_data_raw = iris
iris_data = select(iris_data_raw,Sepal.Length, Sepal.Width,Species)
length_iris_data = dim(iris_data)[1]

#Training and Testing data
lst_index = sample(1:length_iris_data,round(split_per*length_iris_data))
iris_train = iris_data[lst_index,]
iris_test = iris_data[-lst_index,]

knn_result = knn(iris_train[,1:2],iris_test[,1:2],iris_train$Species,prob = TRUE,k=3)
knn_table = table(knn_result,iris_test$Species)
confusionMatrix(knn_table)

lst_ki_index = 90

#lst_knn_data = data.frame(k=lst_ki_index ,accuracy=lst_ki_index )
lst_knn_data = data.frame(k=numeric(lst_ki_index) ,accuracy=numeric(lst_ki_index) )
for(ki in 1:lst_ki_index){
  knn_result = knn(iris_train[,1:2],iris_test[,1:2],iris_train$Species,prob = TRUE,ki)
  knn_table = table(knn_result,iris_test$Species)
  co_matrix = confusionMatrix(knn_table)
  lst_knn_data$k[ki] = ki
  lst_knn_data$accuracy[ki] = round(co_matrix$overall[1] * 100,2)
}

lst_knn_data %>% ggplot(aes(x=k,y=accuracy,colour= k))+geom_line()+
  labs(title="KNN and Accuracy")+ylab("Accuracy in Percentage")+xlab("K Value")


#Cross validation test
lst_knn_data_cv = data.frame(k=numeric(lst_ki_index),accuracy=numeric(lst_ki_index),sens = numeric(lst_ki_index),spec = numeric(lst_ki_index))
for(ki in 1:lst_ki_index){
  knn_result = knn.cv(iris_data[,1:2],iris_data$Species,k=ki)
  knn_table = table(knn_result,iris_data$Species)
  co_table = confusionMatrix(knn_table)
  lst_knn_data_cv$k[ki] = ki
  lst_knn_data_cv$accuracy[ki] =  round(co_table$overall[1] * 100,2)
  lst_knn_data_cv$spec[ki] =  round(co_table$byClass[1] * 100,2)
  lst_knn_data_cv$sens[ki] =  round(co_table$byClass[2] * 100,2)
  
}

means_aco = colMeans(lst_knn_data_cv)

lst_knn_data_cv %>% ggplot(aes(x=k,y=accuracy,colour= k))+geom_line()+
  labs(title="KNN and Accuracy Cross Validation")+ylab("Accuracy in Percentage")+xlab("K Value")





