


#Creating a vector
age = seq(from = 21, to = 26, by= 1)
year_math = seq(from = 4, to = 9, by = 1)
df1 = data.frame(Age = age, Years = year_math)
print(df1)

a = c("Mary","Kin","Jose","Armando","Pedro","Juan")
b = c("English","Business","Math","Physics","Spanish","Esperanto")

#Creating data frame
df2 = data.frame(Name=a,Major=b)
df3 = data.frame(Age = age,Years=year_math,Name = a,Major = b)

#Binding vectors
df4 = cbind(df1,df2)
df3
#Barplot
barplot(df3$Years, names.org=df3$Name)


#Summary
summary(df3)
#Class
class(df2)
class(df4$Name)

#Add a row
new_std = c(23,7,"Math","Telecomunications")
df5 = rbind(df4,new_std)

#Reading a file

data_file = read.csv("C:\\Users\\cestevez\\Desktop\\Data Science Sessions\\6306\\session_1\\R\\ProductSales.csv",header = TRUE)
print(data_file)

#Reading a file using dialog
class(data_file$Date)

file2 = file.choose()
print(file2)
data_file = read.csv(file.choose(),header = TRUE)
print(data_file)


#Exercises

a = 1:6
b = c(3,4,5,5,6,7)
c = c(1,2,3,2,3,4)
d = c("a","d","c","w","a","w")
df = cbind(a,b,c,d)
X1 = df[,2]
X1

#Business Sales

Example1 = read.csv("C:\\Users\\cestevez\\Desktop\\Data Science Sessions\\6306\\session_1\\R\\BusinessSales.csv",header = TRUE)
head(Example1)

#ScatterPlot
library(ggplot2)
plot(Example1$ad_tv,Example1$sales,pch=15,xlab = "Ad TV",ylab="Sales",main="TV Advertising")

#SPlot Iris Pedal length vs Sepal Length
data_iris = iris
plot(data_iris$Petal.Length,data_iris$Sepal.Length,xlab= "Petal",ylab="Sepal",main = "Pedal length vs Sepal Length")


#Using filters
vect1 = seq(from = 1, to = 10, length.out = 20)
vect2 = vect1[vect1>2]
vect2

#Plot only virginica

iris_1 = iris[iris$Species=="virginica",]
plot(iris_1$Sepal.Length,iris_1$Petal.Length,
     xlab = "Sepal",ylab="Petal",col="blue",main="Petal Length vs Sepal Length",xlim=c(4,8),ylim=c(0,7))

iris_2 = iris[iris$Species=="versicolor",]
points(iris_2$Sepal.Length,iris_2$Petal.Length,col="red")
iris_3 = iris[iris$Species=="setosa",]
points(iris_3$Sepal.Length,iris_3$Petal.Length,col="green")

par(mfrow=c(1,2))

#Histogram
hist(mpg$cty,col="blue")

#Boxplot
boxplot(cty~class,data = mpg,main="BoxPlot")

dev.off()