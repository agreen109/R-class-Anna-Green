library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
library(sf)
install.packages("dplyr")
library(dplyr)
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

bmps_summary <- bmps %>% #1.1 summary statistic for cost of BMPs for each state ----
  group_by(StateAbbreviation) %>%
  summarise(
    mean_cost = mean(Cost, na.rm = TRUE),
    median_cost = median(Cost, na.rm = TRUE),
    sd_cost = sd(Cost, na.rm = TRUE),
    total_cost = sum(Cost, na.rm = TRUE),
    count = n()
  )
glimpse(bmps_summary)


bmps_acres <- bmps %>% #1.2 scatterplot of Cost vs. TotalAmountCredited ----
  filter(Unit == "Acres")

ggplot(bmps_acres, aes(x = TotalAmountCredited, y = Cost)) +
  geom_point(color="cadetblue")+
  scale_x_log10() + #applying data transformation for heavily skewed data
  scale_y_log10() +
  labs(title = "Cost vs. Total Amount Credited (Acres)",
       x = "Total Amount Credited",
       y = "Cost")


install.packages("stringr") #1.3 make a boxplot for cover crop BMPs using stringr ----
library(stringr)
cover_crops <- bmps %>%
  filter(str_detect(str_to_lower(BMP), "cover crop"))

ggplot(cover_crops, aes(x = StateAbbreviation, y = TotalAmountCredited)) +
  geom_boxplot() +
  geom_boxplot(color="steelblue") +
  labs(title = "Cover Crop BMPs")


glimpse(dams) #1.4 scatterplot for dam dataset ----
dam_dataset <- dams %>% 
  filter(YEAR != 0) #filtering out year zero

ggplot(dam_dataset, aes(x = YEAR, y = STATE)) +
  geom_point(color = "darkblue") +
  labs(title = "Scatter plot of dam dataset") #why is it only filtering VA and PA? ----


#1.5 (NEED TO DO) one last aspatial visualization

glimpse(streams) #2.1 (NEED TO FIX) 5 longest streams
longest_streams <- streams %>%
  mutate(length = st_length(.)) %>%
  arrange(desc(LengthKM)) %>%
  slice(1:5)

glimpse(longest_streams)

glimpse(streams)
glimpse(counties)
county_streams <- st_join(streams, counties) %>% #2.2 three counties with greatest total length ----
  group_by(STATEFP10) %>%
  summarise(total_length = sum(st_length(.), na.rm = TRUE)) %>%
  arrange(desc(total_length)) %>%
  slice(1:3)
glimpse(county_streams)


glimpse(bmps)
county_costs <- bmps %>% 
  group_by(County_FIPS) %>% 
  summarise(total_bmp_cost = sum(Cost, na.rm = TRUE))

county_map_data <- counties %>%
  left_join(county_costs, by = "County_FIPS")

ggplot(county_map_data) +
  geom_sf(aes(fill = total_bmp_cost)) +
  scale_fill_viridis_c(label = scales::dollar) +
  labs(title = "Total BMP Cost by County")





