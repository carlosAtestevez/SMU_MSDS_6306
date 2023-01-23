library(nbastatR)
library(stringr)
library(plotly)

#Retrieving the list of all players
assign_nba_players() 
df_lst_players = df_dict_nba_players#Getting NBA Players

#Getting LBJ Id and MJ Id
ply_lbj = filter(df_lst_players,grepl("lebron",namePlayer,ignore.case = TRUE)) 
ply_mj = filter(df_lst_players,grepl("Michael Jordan",namePlayer,ignore.case = TRUE))
    
#Getting Career Awards LJB

lst_awards_file = read.csv2("C:\\Users\\cestevez\\Dropbox\\Cloud PC\\Thinkpad\\Thinkpad Desktop\\Master Data Science SMU\\Class_Sessions\\Data Science Sessions\\Repository\\SMU_MSDS_6306\\session_4\\file_awards.csv",
                           header = TRUE,sep = ',')
lst_awards_file = mutate(lst_awards_file,Award=str_replace_all(Award,'[^a-zA-Z0-9_-]','_'))


#Lebron James
pl_career_lbj_awa = players_awards(player_ids = ply_lbj$idPlayer)
pl_career_lbj_awa_sum = pl_career_lbj_awa %>% group_by(namePlayer,nameAward) %>% summarize(number_of_awards = n())
pl_career_lbj_awa_total = pl_career_lbj_awa %>%summarize(total_awards = n())
pl_career_lbj_awa_sum = mutate(pl_career_lbj_awa_sum,nameAward=str_replace_all(nameAward,'[^a-zA-Z0-9_-]','_'))
pl_career_lbj_awa_sum$Importance = ""
for(i in 1:nrow(lst_awards_file)){
  #award_name = str_replace_all(lst_awards_file[i,1],'[^a-zA-Z0-9_-]','_')
  award_name = lst_awards_file[i,1]
  priority = lst_awards_file[i,2]
  pl_career_lbj_awa_sum$Importance[pl_career_lbj_awa_sum$nameAward==award_name] = priority
  
}
 pl_career_lbj_awa_sum %>% ggplot(aes(x=nameAward,y=number_of_awards,fill=Importance))+geom_bar(stat="identity")+coord_flip()+
xlab("Number of Awards by Type")+ylab("Type of Award")+
   labs(title="Lebron James Awards",subtitle=sprintf("Accumulated awards: %i",pl_career_lbj_awa_total$total_awards))+theme_wsj()



#Michael Jordan
pl_career_mj_awa = players_awards(player_ids = ply_mj$idPlayer)
pl_career_mj_awa_sum = pl_career_mj_awa %>% group_by(namePlayer,nameAward) %>% summarize(number_of_awards = n())
pl_career_mj_awa_total = pl_career_mj_awa %>%summarize(total_awards = n())
pl_career_mj_awa_sum = mutate(pl_career_mj_awa_sum,nameAward=str_replace_all(nameAward,'[^a-zA-Z0-9_-]','_'))
pl_career_mj_awa_sum$Importance = ""
for(i in 1:nrow(lst_awards_file)){
  #award_name = str_replace_all(lst_awards_file[i,1],'[^a-zA-Z0-9_-]','_')
  award_name = lst_awards_file[i,1]
  priority = lst_awards_file[i,2]
  pl_career_mj_awa_sum$Importance[pl_career_mj_awa_sum$nameAward==award_name] = priority
  
}
pl_career_mj_awa_sum %>% ggplot(aes(x=nameAward,y=number_of_awards,fill=Importance))+geom_bar(stat="identity")+coord_flip()+
  xlab("Number of Awards by Type")+ylab("Type of Award")+labs(title="Michael Jordan Awards",subtitle=sprintf("Accumulated awards: %i",pl_career_mj_awa_total$total_awards))+theme_wsj()



#Getting career stats and Plotting JBJ 
pl_career_lbj = players_careers(player_ids = c(ply_lbj$idPlayer),modes = c("Totals"))
pl_career_lbj_rs = filter(pl_career_lbj,nameTable == 'SeasonTotalsRegularSeason')
pl_total_lbj_rs = pl_career_lbj_rs$dataTable[[1]]
pl_off_lbj = select(pl_total_lbj_rs,pts,fgm,fg3m,fg2m,ftm)
pl_off_lbj_def = select(pl_total_lbj_rs,oreb,dreb,blk)
pl_off_lbj_avg = select(pl_total_lbj_rs,pctFG,pctFG3)
pl_off_lbj_total_f1 =  gather(pl_off_lbj,keyfigure, total, pts:ftm, factor_key=TRUE)
pl_off_lbj_total_def_f1 =  gather(pl_off_lbj_def,keyfigure, total, oreb:blk, factor_key=TRUE)
pl_off_lbj_total_avg_f1 =  gather(pl_off_lbj_avg,keyfigure, total, pctFG:pctFG3, factor_key=TRUE)
pl_off_lbj_total_f1 = group_by(pl_off_lbj_total_f1,keyfigure) %>% summarise(total=sum(total))
pl_off_lbj_total_def_f1  = group_by(pl_off_lbj_total_def_f1 ,keyfigure) %>% summarise(total=sum(total))
pl_off_lbj_total_avg_f1 = group_by(pl_off_lbj_total_avg_f1,keyfigure) %>% summarise(total=mean(total))

gglb = pl_off_lbj_total_f1 %>% ggplot(aes(x=keyfigure,y=total,fill=keyfigure))+geom_bar(stat="identity")+
  xlab("Offensive Skills")+ylab("Value")+labs(title="Accumulated Off.Skills LBJ",subtitle="Accumulated awards of LBJ")+theme_wsj()
ggplotly(gglb)

pl_off_lbj_total_avg_f1 %>% ggplot(aes(x=keyfigure,y=total,fill=keyfigure))+geom_bar(stat="identity")+
  xlab("Offensive Skills %")+ylab("Value")+labs(title="Offensive Skills % LBJ",subtitle="Accumulated awards of LBJ")+theme_wsj()

pl_off_lbj_total_def_f1 %>% ggplot(aes(x=keyfigure,y=total,fill=keyfigure))+geom_bar(stat="identity")+
  xlab("Defensive Skills")+ylab("Value")+labs(title="Defensive Skills LBJ",subtitle="Defensive Skills of LBJ")+theme_wsj()




#Getting career stats and Plotting MJ 
pl_career_mj = players_careers(player_ids = c(ply_mj$idPlayer),modes = c("Totals"))
pl_career_mj_rs = filter(pl_career_mj,nameTable == 'SeasonTotalsRegularSeason')
pl_total_mj_rs = pl_career_mj_rs$dataTable[[1]]
pl_off_mj = select(pl_total_mj_rs,pts,fgm,fg3m,fg2m,ftm)
pl_off_mj_avg = select(pl_total_mj_rs,pctFG,pctFG3)
pl_off_mj_def = select(pl_total_mj_rs,oreb,dreb,blk)
pl_off_mj_total_avg_f1 =  gather(pl_off_mj_avg,keyfigure, total, pctFG:pctFG3, factor_key=TRUE)
pl_off_lbj_total_def_f1 =  gather(pl_off_mj_def,keyfigure, total, oreb:blk, factor_key=TRUE)
pl_off_mj_total_f1 =  gather(pl_off_mj,keyfigure, total, pts:ftm, factor_key=TRUE)
pl_off_mj_total_f1 = group_by(pl_off_mj_total_f1,keyfigure) %>% summarise(total=sum(total))
pl_off_mj_total_avg_f1 = group_by(pl_off_mj_total_avg_f1,keyfigure) %>% summarise(total=mean(total))
pl_off_mj_total_def_f1  = group_by(pl_off_mj_total_def_f1 ,keyfigure) %>% summarise(total=sum(total))

ggmj =pl_off_mj_total_f1 %>% ggplot(aes(x=keyfigure,y=total,fill=keyfigure))+geom_bar(stat="identity")+
  xlab("Offensive Skills")+ylab("Value")+labs(title="Accumulated Off.Skills MJ",subtitle="Accumulated awards of MJ")+theme_wsj()
ggplotly(ggmj)

pl_off_mj_total_avg_f1 %>% ggplot(aes(x=keyfigure,y=total,fill=keyfigure))+geom_bar(stat="identity")+
  xlab("Offensive Skills %")+ylab("Value")+labs(title="Offensive Skills % MJ",subtitle="Accumulated awards of MJ")+theme_wsj()

pl_off_mj_total_def_f1 %>% ggplot(aes(x=keyfigure,y=total,fill=keyfigure))+geom_bar(stat="identity")+
  xlab("Defensive Skills")+ylab("Value")+labs(title="Defensive Skills MJ",subtitle="Defensive Skills of MJ")+theme_wsj()




