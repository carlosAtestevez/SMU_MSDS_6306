library(xgboost)
library(caTools)
library(dplyr)
library(caret)


head(iris)
sample_split <- sample.split(Y = iris$Species, SplitRatio = 0.7)

iris$Species