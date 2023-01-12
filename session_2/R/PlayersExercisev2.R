
#Libraries necessary for Plotting
library(ggplot2)
library(ggpubr)
library(plotly)
library(ggthemes)

#Function to read the players file
read_file_players=function(){
  dfl_players = read.csv("C:\\Users\\cestevez\\Desktop\\Data Science Sessions\\6306\\session_2\\R\\PlayersBBall.csv",header = TRUE)
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

#Initial Data
df_players_raw = read_file_players()
#Data Converted
df_players = convert_height(df_players_raw)

#Activity 1: The number of players in each position
df_players %>% ggplot(aes(x = position,fill=position))+
               geom_bar() + ggtitle("Distribution of Players by Position")+
               xlab("Player's Position")+ylab("Number of players")



#Activity 2: Distribution of Weight Centers and Forwards
df_players_filter_1 = df_players[df_players$position=="C" |df_players$position=="F", ]
df_players_filter_1 %>% ggplot(aes(x = weight,fill=position)) + 
  geom_histogram(color="black",position = "dodge") + 
  ggtitle("Distribution of Weight Center and Forward")+xlab("Player's weight")+ylab("Number of players")


#Activity 3: Distribution of Height Centers and Forwards
df_players_filter_2 = df_players[df_players$position=="C" |df_players$position=="F", ]
df_players_filter_2 %>% ggplot(aes(x = height_nr,fill=position)) + 
  geom_histogram(color="black",position = "dodge") + 
  ggtitle("Distribution of Height Center and Forward")+xlab("Player's Height")+ylab("Number of players")


#Activity 4: Distribution of Height any position
df_players_filter_3 = df_players[df_players$position!="",]
df_players_filter_3 %>% ggplot(aes(x = height_nr,fill=position)) + 
  geom_histogram(color="black",position="dodge") + 
  ggtitle("Distribution of Height any position") + facet_wrap(~position)+xlab("Player's Height")+
  ylab("Number of Players")


#Activity 5: Height related player's height
nr_correlation = cor(df_players$height_nr,df_players$weight,use = "complete.obs")
str_title_1 = paste("Height vs Weight,correlation:",nr_correlation)
df_players %>% ggplot(aes(x=height_nr,y=weight))+
  geom_smooth()+ggtitle(str_title_1)+xlim(c(5, 8))+xlab("Player's Height")+ylab("Player's Weight")
df_players %>% ggplot(aes(x=height_nr,y=weight,color=position))+geom_point()+
   geom_smooth()+ggtitle(str_title_1)+xlim(c(5, 8))+xlab("Player's Height")+ylab("Player's Weight")

#Activity 6: Height related player's height by position
df_players_filter_0 = df_players[df_players$position!="",]
df_players_filter_0[] %>% ggplot(aes(x=height_nr,y=weight,col=position,linetype = position))+geom_point()+
  geom_smooth()+ggtitle(str_title_1)+xlim(c(5, 8))+facet_wrap(~position)+xlab("Player's Height")+
  ylab("Number of Players")

#Activity 7: Player's height over the years
df_players %>% ggplot(aes(x=year_start,y=height_nr))+geom_smooth()+
  ggtitle("Player's height over the years")+xlab("Year Star")+ylab("Player's Height")

#Activity 8: 3D Plot Weight,Height,Year
pdata = plot_ly(df_players,x=~height_nr,y=~weight,z=~year_start,color=~position) %>% add_markers() %>% layout(scene=list(xaxis=list(title="Height"),
                                                                                                                         yaxis=list(title="Weight"),
                                                                                                                         zaxis=list(title="Year")
))
pdata

