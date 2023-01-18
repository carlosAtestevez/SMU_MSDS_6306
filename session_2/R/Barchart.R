

library(tidyverse)
library(ggthemes)


mpg%>% ggplot(aes(x=class,fill=drv))+geom_bar(position="dodge")+theme_wsj()

mpg%>% ggplot(aes(x=class,fill=drv))+geom_bar()+coord_flip()

mpg %>% ggplot(aes(x=class,fill=drv))+geom_bar(position = "stack")

mpg %>% ggplot(aes(x=class,fill=drv))+geom_bar(position = "fill")