
library(tidyverse)
library(plotly)

p = mpg%>% ggplot(aes(x=class,fill=drv))+geom_bar(position="dodge")+theme_wsj()

p + ggplotly()
ggplotly(p)