

#Function to read the players file
read_file_players=function(){

dfl_players = read.csv("C:\\Users\\cestevez\\Desktop\\Data Science Sessions\\6306\\session_2\\R\\PlayersBBall.csv",header = TRUE)
return (dfl_players)  
  
}



example_ggplot=function(){
  #Iris Scatterplot
  ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Petal.Length)) + geom_point()
  
  #With pipe!  %>% = "and then"
  iris %>% ggplot(aes(x = Sepal.Length,y = Petal.Length)) + geom_point() #Labels but not TITLE! AHHH!!!!
  
  iris %>% ggplot(aes(x = Sepal.Length,y = Petal.Length)) + geom_point() + ggtitle("City MPG v. Highway MPG")
  
  # You Try It! (mpg)
  # plot (scatter plot) city mile per gallon (y) versus highway miles per gallon (x).
  
  mpg %>% ggplot(aes(x = hwy, y = cty)) + geom_point() + ggtitle("City MPG v. Highway MPGmpg")
}

example_barchart= function(){
  #With base barplot
  summary(mpg$class)
  
  mpg$classFact = as.factor(mpg$class)
  head(mpg)
  summary(mpg$classFact)
  
  #barplot(summary(mpg$classFact))
  
  # With ggplot /  geom_bar()
  mpg %>% ggplot(aes(x = class)) + geom_bar() + ggtitle("Distribution of Class")

}



# example_barchart()
#example_color_1()

mpg %>% ggplot(aes(x=hwy,y=cty,color=drv))+geom_point()
mpg4 = mpg
