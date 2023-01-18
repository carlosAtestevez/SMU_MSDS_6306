

lst_id = c(1,2,3)
lst_name = c("Charles","George","Michael")
lst_last_name = c("Gates","Michael","Sheen")

df_1 = data.frame(lst_id,lst_name,lst_last_name)

df_2 = filter(df_1,lst_id > 2)

