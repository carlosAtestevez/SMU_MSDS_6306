

#Reading the file
df_eincome_raw = read.csv("C:\\Users\\cestevez\\Desktop\\Data Science Sessions\\6306\\session_2\\R\\Education_Income.csv",header = TRUE)
df_eincome_raw$education_nr = 0
count_income = nrow(df_eincome_raw)

#Converting level to numeric
for(index in 1:count_income){
  str_level = df_eincome_raw[index,2]
  num_level = 0
  if(str_level==""){
    num_level = 0
  }else if(str_level == "<12"){
    num_level = runif(1,1,11)
  }else if(str_level == "13-15"){
    num_level = 14
  }
  else if(str_level == ">16"){
    num_level = runif(1,17,25)
  }else{
    num_level = as.numeric(str_level)
  }
  df_eincome_raw[index,4] = num_level
}


df_eincome = df_eincome_raw
#df_eincome$Educ = as.numeric(df_eincome_raw$Educ)
df_eincome %>% ggplot(aes(x = education_nr ,y = Income2005)) + geom_point()

#Plotting the data
df_eincome %>% ggplot(aes(x=education_nr,y=Income2005))+
  geom_smooth()+xlab("Education Level")+
  ylab("Income")




