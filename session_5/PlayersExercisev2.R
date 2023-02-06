library(tidyverse)
library(dplyr)


#Function to read the players file
read_file_players=function(){
  dfl_players = read.csv("C:\\Users\\cestevez\\Dropbox\\Cloud PC\\Thinkpad\\Thinkpad Desktop\\Master Data Science SMU\\Class_Sessions\\Data Science Sessions\\Repository\\SMU_MSDS_6306\\session_2\\R\\PlayersBBall.csv",header = TRUE)
  dfl_players = dfl_players[!is.na(dfl_players$position),]
  dfl_players = dfl_players %>% filter(position!="")
  return (dfl_players)  
}

#This function is used to convert height to numbers(6-12-->6.42)
convert_height = function(p_df_player){
  df_r_players = p_df_player
  df_r_players$height_nr = 0
  count_players = nrow(df_r_players)
  print(count_players)
  index = 0
  for(index in 1:count_players){
    str_height = df_r_players[index,5]
    if(str_height!=''){
      lst_h0 = strsplit(df_r_players[index,5],"-")
      lst_h1 = matrix(unlist(lst_h0),ncol=2,byrow=T)
      num_h1 = as.numeric(lst_h1[1,1])
      num_h2 = as.numeric(lst_h1[1,2])
      df_r_players[index,9] = num_h1 + num_h2/12
    }
  }
  return(df_r_players)
}


convert_height_regex = function(p_df_player){
  df_r_players = p_df_player
  df_r_players
  df_r_players$TotalInches = c()
  for(index in 1:nrow(df_r_players)){
    str_height = df_r_players[index,5]
    str_height
    # if(str_height!=''){
    #   lst_measures = unlist(str_extract_all(str_height,"[:digit:]"))
    #   total_height = as.numeric(lst_h1[1,1])
    #   num_h2 = as.numeric(lst_h1[1,2])
    #   df_r_players[index,9] = num_h1 + num_h2/12
    # }
  }
  return(df_r_players)
}



#Initial Data
df_players_raw = read_file_players()
df_players_raw$TotalInches = c()
count_players = nrow(df_players_raw)
for(index in 1:count_players){
  str_height = df_players_raw[index,5]
  lst_measures = unlist(str_split(str_height,"-"))
  total = as.numeric(lst_measures[1])*12+ as.numeric(lst_measures[2])
  df_players_raw$TotalInches[index] = total
}


total_players = df_players_raw %>% select(position,TotalInches) %>% 
  group_by(position) %>% dplyr::summarize(TotalHeight=sum(TotalInches))

total_avg_players = df_players_raw %>% select(position,TotalInches) %>% 
  group_by(position) %>% dplyr::summarize(TotalHeight=mean(TotalInches))


total_players %>% ggplot(aes(x=position,y=TotalHeight,fill=position))+geom_bar(stat="identity")+
                  labs(title="NBA Players Height by Position", 
                       subtitle="Total NBA Players height",
                       )+xlab("Total Height")+ylab("Player Position")
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) 

total_avg_players %>% ggplot(aes(x=position,y=TotalHeight,fill=position))+geom_bar(stat="identity")+
    labs(title="NBA Players Height by Position", 
         subtitle="Average Height by Position",
    )+xlab("Average Height")+ylab("Player Position")
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

