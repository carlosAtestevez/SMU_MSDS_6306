


library(RCurl) 
library(tidyverse)
library(aws.s3)
library(tidyverse)

library(aws.s3)
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIA5HRB7XJNIOKHMHYK",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION"="us-east-2")



# Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIA5HRB7XJNIOKHMHYK",
#            "AWS_SECRET_ACCESS_KEY" = "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")

obj <- get_object("Beers.csv", bucket = "ds6306t1")
obj2 <- get_object("Breweries.csv", bucket = "ds6306t1")


df1=read.csv(text = rawToChar(obj), sep=",", header = TRUE)
df2=read.csv(text = rawToChar(obj2), sep=",", header = TRUE)


# png("histtest.png")
# df1 %>% ggplot(aes(x=ABV))+geom_histogram(fill="blue")
# dev.off()
# 
# 
# put_object("histtest.png", object = "beer_study_img/histtest.png", bucket = "ds6306t1")
# 
# 


