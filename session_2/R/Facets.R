library(tidyverse)




mpg %>% ggplot(aes(x = cty, y = hwy, color = drv)) + 
  geom_point() + facet_grid(cyl~year)

mpg %>% ggplot(aes(x = cty, y = hwy, color = drv)) + 
  geom_point() + facet_wrap(cyl~year)

