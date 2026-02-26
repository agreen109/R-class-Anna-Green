library(tidyverse)
library(sf)
install.packages("tmap")
library(tmap)
counties<-sf::read_sf("./County_Boundaries.shp") %>%
  sf::st_make_valid()
dams<-sf::read_sf("./Dam_or_Other_Blockage_Removed_2012_2017.shp") %>%
  sf::st_make_valid()
streams<-sf::read_sf("./Streams_Opened_by_Dam_removal_2012_2017.shp") %>%
  sf::st_make_valid()
bmps<-read_csv("./BMPreport2016_landbmps.csv")

glimpse(bmps)
state_summary <- bmps %>%
  group_by(Cost) %>%
  summarise(
    avg_cost = mean(cost_column, na.rm = TRUE),
    total_cost = sum(cost_column, na.rm = TRUE),
    count = n() 
  )