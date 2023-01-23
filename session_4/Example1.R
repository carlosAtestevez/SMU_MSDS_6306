library(XML) #xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL


#Basics of Scraping XML

data <-getURL("https://www.w3schools.com/xml/simple.xml")
doc <- xmlParse(data)
names <- xpathSApply(doc,"//name",xmlValue)
price <- xpathSApply(doc,"//price",xmlValue)
description <- xpathSApply(doc,"//description",xmlValue)
names
price
description

df1 = data.frame(name = names,description=description,price = price)
df1


#Basic 1
data_2 = getURL("https://www.w3schools.com/xml/plant_catalog.xml")
doc_2 = xmlParse(data_2)
plants = xpathSApply(doc_2,"//LIGHT",xmlValue)
plants

# #Method reading a file
# 
# datax = xmlParse("C:\\Users\\cestevez\\Dropbox\\Cloud PC\\Thinkpad\\Thinkpad Desktop\\Master Data Science SMU\\Class_Sessions\\Data Science Sessions\\Repository\\SMU_MSDS_6306\\session_4\\test.xml")
# datax
# doc1 <- xmlParse(datax)
# name_owner = xmlParse(doc1,"//id",xmlValue)
# name_owner
