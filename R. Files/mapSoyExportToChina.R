# Maps 
# Don't bother installing if you already have them
install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))

# some standard map packages.
install.packages(c("maps", "mapdata"))
install.packages("tidyverse")
install.packages("mapdata")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(tidyverse)

states <- map_data("state")

soy_bean_producers <- states %>%
  filter(region %in% c("arkansas", "kansas", "mississippi", "missouri", "montana", "north carolina", "north dakota", "ohio", "oklahoma", "south dakota"))

soy_bean_producers_b <- states %>%
  filter(region %in% c("iowa", "minnesota", "indiana", "illinois", "nebraska"))

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = NULL, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE) + geom_polygon(data= soy_bean_producers, aes(x = long, y = lat, group = group), fill = "gold", color = "black") + 
  ggtitle("States where Oilseeds and Grains Main export to China") + 
  geom_polygon(data= soy_bean_producers_b, aes(x = long, y = lat, group = group), fill = "red", color = "black") 

