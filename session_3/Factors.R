
library(tidyverse)
sex <- factor(c("male", "female", "female", "male","male"))
lst_sex_content = levels(sex) #This is the set of strings
i_levels = nlevels(sex) #This is the number of the levels
            
summary(lst_sex_content)
str(lst_sex_content)


y = factor(c("Bike","Car","Cycle","Truck","Car","Bike","Cycle","Truck","Car","Bike"))
y


mpg %>% ggplot(aes(x=cty,y=cyl,color=class))+geom_point()

mpg %>%
  ggplot(aes(x = hwy, y = cty, color = cyl)) +
  geom_point()

mpg$cyl
fact_cyl = factor(mpg$cyl)
fact_cyl
str(mpg)

df_car_sales = read.csv("C:\\Users\\cestevez\\Dropbox\\Cloud PC\\Thinkpad\\Thinkpad Desktop\\Master Data Science SMU\\Class_Sessions\\Data Science Sessions\\Repository\\SMU_MSDS_6306\\session_3\\CarSales.csv",header = TRUE)
df_car_sales

df_car_sales %>% filter(Car.Type==1) %>% ggplot(aes(x=Year,y=Sold.Cars,color=Car.Type))+geom_point()


