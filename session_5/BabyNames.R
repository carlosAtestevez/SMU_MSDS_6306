library(tidyverse)
library(stringr)


#Reading the files
df_y2016 = read.csv("C:\\Users\\cestevez\\Dropbox\\Cloud PC\\Thinkpad\\Thinkpad Desktop\\Master Data Science SMU\\Class_Sessions\\Data Science Sessions\\Repository\\SMU_MSDS_6306\\session_5\\yob2016.txt",
                          sep = ";",header = FALSE)

df_y2015 = read.csv("C:\\Users\\cestevez\\Dropbox\\Cloud PC\\Thinkpad\\Thinkpad Desktop\\Master Data Science SMU\\Class_Sessions\\Data Science Sessions\\Repository\\SMU_MSDS_6306\\session_5\\yob2015.txt",
                          sep = ",",header = FALSE)


colnames(df_y2015) = c("Name","Gender","Count")
colnames(df_y2016) = c("Name","Gender","Count")

tail(df_y2015,n = 10)

df_y2015 = arrange(df_y2015,Name)
df_y2016 = arrange(df_y2016,Name)


df_babies = merge(df_y2015,df_y2016,by="Name")
colnames(df_babies) = c("Name","Gender2015","Count2015","Gender2016","Count2016")

df_babies = mutate(df_babies,Total=Count2015+Count2016)
df_babies %>% summarize(Sum = sum(Total))
df_babies = select(df_babies,Name,Gender2015,Total)
colnames(df_babies) = c("Name","Gender","Total")

df_girls = filter(df_babies,Gender=="F")
df_girls = head(arrange(df_girls,desc(Total)),n=100)
df_boys = filter(df_babies,Gender=="M")


df_babies_top = arrange(df_babies,desc(Total))
df_babies_top_xx = head(arrange(df_babies,desc(Total)),n=30)

df_babies_girl = filter(df_babies,Gender=="F")
df_top_girls = head(arrange(df_babies_girl,desc(Total)),n=10) %>% select(Name,Total)



df_babies_top_xx %>% ggplot(aes(x=reorder(Name, -Total),y=Total,fill=Gender))+geom_bar(stat="identity")+
  theme_wsj()+
  theme(
    axis.text.x=element_text(angle=70, hjust=1, size=10),
  )+labs(title="Thirty Most popular babies names by gender",
         subtitle = "Analysis for babies born in 2015 and 2016")+xlab("Babies Names")+ylab("Total")



lst_cust_list = read.csv("C:\\Users\\cestevez\\Dropbox\\Cloud PC\\Thinkpad\\Thinkpad Desktop\\Master Data Science SMU\\Class_Sessions\\Data Science Sessions\\Repository\\SMU_MSDS_6306\\session_5\\baby_cust_list.csv",
                         header=TRUE)

lst_cust_list_girls = filter(lst_cust_list,Gender=="F")
lst_cust_list_boys = filter(lst_cust_list,Gender=="M")


lst_cust_list_girls$Percentage = c()
count_list = nrow(lst_cust_list_girls)
total_names_girls = sum(df_girls$Total)
for(index in 1:count_list){
  #total_sel_names = str_count(df_babies$Name,lst_cust_list$Name[index])
  selected_baby_name = lst_cust_list_girls$Name[index]
  selected_gender = lst_cust_list_girls$Gender[index]
  count_girls = filter(df_girls,Name == selected_baby_name & Gender == selected_gender ) %>% summarize(Total = sum(Total))
  print(selected_baby_name)
  print(count_girls)
  
  lst_cust_list_girls$Percentage[index] = count_girls / total_names_girls

}



