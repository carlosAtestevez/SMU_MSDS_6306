
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
assign_nba_players()


head(df_dict_nba_players)

lbj = filter(df_dict_nba_players,grepl("lebron",namePlayer,ignore.case = TRUE))
lbj$urlPlayerPhoto

players_careers(players = c("LeBron James"))

players_careers(player_ids = c(2544))


lebron_totals <- players_careers(players = c("LeBron James"),
                                 modes = c("Totals"))
head(lebron_totals)
View(lebron_totals$dataTable)

ask_nba_api_nlp_question(question = "Most points ever scored in a game",
                         return_similar_questions = T)