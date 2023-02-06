
#Loading libraries
library(ggplot2)
library(dplyr)
library(XML)
library(rvest)
library(ggthemes)


#Retrieving restaurant's data
url_rest = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
data_rest = getURL(url_rest)
doc_rest = xmlParse(data_rest)
lst_name_rest = xpathSApply(doc_rest,"//name",xmlValue)
lst_zip_code = xpathSApply(doc_rest,"//zipcode",xmlValue)
lst_cou_dist = xpathSApply(doc_rest,"//councildistrict",xmlValue)
lst_neighborhood = xpathSApply(doc_rest,"//neighborhood",xmlValue)
lst_location_1 = xpathSApply(doc_rest,"//location_1/@human_address")


#Initial DataFrames
df_rest_1 = data.frame(name=lst_name_rest,zip_code=lst_zip_code,councildistrict=lst_cou_dist)
df_rest_2 = data.frame(name=lst_name_rest,zip_code=lst_zip_code,neighborhood=lst_neighborhood,councildistrict=lst_cou_dist,location=lst_location_1)
total_restaurants = nrow(df_rest_2)
sprintf("Total Restarants %i",total_restaurants)

#Sushy Restaurants
nr_rest_sushy = length(grep("sushi",df_rest_1$name,ignore.case = TRUE))
sprintf("Suchy Restaurants in Baltimore %i",nr_rest_sushy)

#Filters

df_rest_down = filter(df_rest_1,councildistrict == 11)

#Sushy restaurants in downtown
nr_sushy_down = length(grep("sushi",c,ignore.case = TRUE))
sprintf("Suchy Restaurants in Baltimore's downtown %i",nr_sushy_down)


#Plots
lst_bool_sushi = grepl("sushi",df_rest_1$name,ignore.case = TRUE)
df_rest_3 = mutate(df_rest_1,suchi_restaurant=ifelse(grepl("sushi",name,ignore.case = TRUE)==TRUE,TRUE,FALSE))
df_rest_4 = cbind(df_rest_1,lst_bool_sushi)
df_rest_1 %>% ggplot(aes(x=councildistrict,fill=councildistrict))+geom_bar()+labs(title="Restaurants by Districts ")+
  xlab("Districts")+ylab("Number of Restaurants")
df_rest_3 %>% ggplot(aes(x=councildistrict,fill=suchi_restaurant))+geom_bar(position = "dodge")+
  xlab("Type of Restuarants")+ylab("Number of Restaurants")+labs(title="Sushi Restaurants and Others")



#if(length(grep("sushi",c(name),ignore.case = TRUE))>=1){suchi_rest = TRUE}else{suchi_rest=FALSE

