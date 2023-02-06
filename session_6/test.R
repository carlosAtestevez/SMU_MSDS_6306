library(class)
library(caret)
library(e1071)
library(tidyverse)



#Variables -->Sepal Width = 6.2  and Petal Length 
irisVersVirg = iris 
df = data.frame(Sepal.Width = 6.20 , Petal.Length = 4.9 )
knn(irisVersVirg[,c(2,3)], df, irisVersVirg$Species, k = 15, prob = TRUE)
knn(irisVersVirg[,c(2,3)], df, irisVersVirg$Species, k = 5, prob = TRUE)

irisVersVirg %>% ggplot(aes(x=Sepal.Width,y=Petal.Length,color=Species))+geom_point()+
  geom_point(aes(x=6.20,y=4.8,size = 20,color="white")) 


Emails = data.frame(Predicted = c("Spam","Ham","Ham", "Ham", "Ham", "Spam", "Ham", "Spam", "Ham", "Spam"), 
                    Actual = c("Spam", "Spam", "Ham", "Ham", "Spam", "Ham", "Spam","Ham","Spam","Spam" ))

table(Emails)
confusionMatrix(table(Emails$Predicted,Emails$Actual))


#Species Iris data
irisVersVirg = iris %>% filter(Species == "versicolor" | Species == "virginica")
irisVersVirg = droplevels(irisVersVirg,exclude = "setosa")
irisVersVirg %>% ggplot(aes(x = Sepal.Length,Sepal.Width,color = Species)) + geom_point()

classifications = knn.cv(irisVersVirg[,1:2],irisVersVirg$Species, k = 10)
confusionMatrix(table(classifications,irisVersVirg$Species))

#train = irisVersVirg[trainIndices,]
#test = irisVersVirg[-trainIndices,]
#classifications = knn.cv(dfZTrain[,1:2],dfZTrain$Qualify, k = 3)
#confusionMatrix(classifications,dfTrain$Qualify)
#classifications = knn(train[,c(1,2)],test[,c(1,2)],train$Species, prob = TRUE, k = 3)