

library(ggplot2)
library(ggpubr)

#Function to read the players file
read_file_players=function(){
  
  dfl_players = read.csv("C:\\Users\\cestevez\\Desktop\\Data Science Sessions\\6306\\session_2\\R\\PlayersBBall.csv",header = TRUE)
  return (dfl_players)  
  
}

convert_to_numbers = function(p_df_player){
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

get_number_players_by_pos=function(p_df_players){
  df_players$posfact = as.factor(p_df_players$position)
  df_sum_players = summary(df_players$posfact)
  #head(df_players)
  return(df_sum_players)
}



df_players_raw = read_file_players()
df_players = convert_to_numbers(df_players_raw)

# 
# 
# df_players$height_num=as.factor(df_players$height)
# df_players$posfact = as.factor(df_players$position)
# summary(df_players$posfact)

#df_sum_players = get_number_players_by_pos(df_players)
#barplot(df_sum_players,main = "Number of players by position")


#Activity 1: The number of players in each position
df_players %>% ggplot(aes(x = position,fill=position)) + geom_bar() + ggtitle("Distribution of Players by Position")+xlab("Player Position")
df_players %>% ggplot(aes(x = position,fill=position)) + geom_bar() + ggtitle("Distribution of Players by Position")+coord_polar()
df_players %>% ggplot(aes(x = position,fill=position)) + geom_bar() + ggtitle("Distribution of Players by Position")+xlab("Player Position")+theme_fivethirtyeight()
#act1_res = df_players %>% ggplot(aes(x = position,fill=position)) + geom_bar() + ggtitle("Distribution of Players by Position")+theme_fivethirtyeight()
#ggplotly(act1_res)


#Activity 2: Distribution of Weight Centers and Forwards
df_players_filter_1 = df_players[df_players$position=="C" |df_players$position=="F", ]
df_players_filter_1 %>% ggplot(aes(x = weight,fill=position)) + geom_histogram(color="black")
df_players_filter_1 %>% ggplot(aes(x = weight,fill=position)) + geom_histogram(color="black",position = "dodge") + ggtitle("Distribution of Weight")



#Activity 3: Distribution of Height Centers and Forwards
df_players_filter_2 = df_players[df_players$position=="C" |df_players$position=="F", ]
df_players_filter_2 %>% ggplot(aes(x = height_nr,fill=position)) + geom_histogram(color="black") + ggtitle("Distribution of Height")


#Activity 4: Distribution of Height any position
df_players_filter_3 = df_players
df_players_filter_3 %>% ggplot(aes(x = height_nr,fill=position)) + geom_histogram(color="black") + ggtitle("Distribution of Height any position")

  
#Activity 5: Height related player's height
nr_correlation = cor(df_players$height_nr,df_players$height_nr,use = "complete.obs")
str_title_1 = paste("Height vs Weight,correlation:",nr_correlation)
df_players %>% ggplot(aes(x=height_nr,y=weight,col=position))+geom_point()+ggtitle(str_title_1)+xlim(c(4, 8))+
  geom_abline(intercept = -300, slope = 80)

#Activity 6: Height related player's height by position
df_players_filter_0 = df_players[TRUE,]
nr_correlation = cor(df_players_filter_0$height_nr,df_players_filter_0$weight,use = "complete.obs")
str_title_1 = paste("Height vs Weight by position,correlation:",nr_correlation)

df_players_filter_0 %>% ggplot(aes(x=height_nr,y=weight,col=position))+geom_point()+
geom_smooth(method="lm", se=F)+ggtitle(str_title_1)+xlim(c(5, 8))

df_players_filter_0 %>% ggplot(aes(x=height_nr,y=weight,col=position,linetype = position))+geom_point()+
  geom_smooth()+ggtitle(str_title_1)+xlim(c(5, 8))

df_players_filter_0 %>% ggplot(aes(x=height_nr,y=weight,col=position,linetype = position))+geom_point()+
  geom_smooth()+ggtitle(str_title_1)+xlim(c(5, 8))+facet_wrap(~position)



#Activity 7: Change of height over the years
players_60_80 = df_players[df_players$year_start < 1980,]
players_81_22 = df_players[df_players$year_start > 1981,]
mean_60_80 = mean(players_60_80$height_nr) 
mean_81_22 = mean(players_81_22$height_nr)
str_title_2 = sprintf("Mean 1960-1980:%f and 1980-2022:%f",mean_60_80,mean_81_22)


df_players %>% ggplot(aes(x=year_start,y=height_nr))+geom_line( color="steelblue")+ggtitle(paste("Players height over the years,",str_title_2))


df_players %>% ggplot(aes(x=year_start,y=height_nr))+geom_smooth()+ggtitle(paste("Players height over the years,",str_title_2))



pdata = plot_ly(df_players,x=~height_nr,y=~weight,z=~year_start,color=~position) %>% add_markers() %>% layout(scene=list(xaxis=list(title="Height"),
                                                                                                         yaxis=list(title="Weight"),
                                                                                                         zaxis=list(title="Year")
                                                                                                         ))

pdata
