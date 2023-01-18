install.packages("maps")
library(maps)
usa = map_data("usa")
p = ggplot()+geom_polygon(data=usa,
                          aes(x=long,y=lat,group=group),fill="blue",color="black")+
  coord_quickmap()

