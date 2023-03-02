library(tidyverse)
library(stringr)


#Reading files

df_beers = read.csv("session_8/Beers.csv",header = TRUE)
df_breweries_1 = read.csv("session_8/Breweries.csv",header = TRUE)
df_states = read.csv("session_8/States.csv",header=TRUE)
df_breweries_1$State = str_trim(df_breweries_1$State)
df_breweries_2 = merge(df_breweries_1,df_states,by = "State")



#1.How many breweries are present in each state
df_sum_brew = df_breweries_2 %>% group_by(State,Name_State) %>% summarize(NumberBreweries = n())


#2.Merge beer data with the breweries data. Print the first 6 observations and the 
#last six observations to check the merged file

df_merge_1 = merge(df_beers,df_breweries_2,by.x = "Brewery_id",by.y = "Brew_ID")

#3.Address the missing values in each column.




