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
  labs(title = "Scatter plot of dam dataset") 

glimpse(dams)
number_of_dams <- dams %>% #1.5 one last aspatial visualization ----
  group_by(STATE) %>%
  summarize(Dam_Count = n())

bmps %>%
  group_by(StateAbbreviation) %>%
  summarize(Total_Cost = sum(Cost, na.rm = TRUE)) %>%
  inner_join(dams_per_state, by = c("StateAbbreviation" = "STATE")) %>%
  ggplot(aes(x = Dam_Count, y = Total_Cost, label = StateAbbreviation)) +
  geom_line(color = "pink", size=2) +
  scale_y_continuous(labels = scales::dollar) +
  theme_light() +
  labs(
    title = "Dam Counts vs Total BMP Cost",
    x = "Number of Dams",
    y = "Total BMP Cost"
  )


glimpse(streams) #2.1 five longest streams (not sure if this is correct output) ----
streams_length <- streams %>%
  mutate(length = st_length(geometry)) 

top_5_streams <- streams_length %>%
  arrange(desc(LengthKM)) %>%
  slice(1:5)
print(top_5_streams)


glimpse(streams)
glimpse(counties)
county_streams <- st_join(streams, counties) %>% #2.2 three counties with greatest total length (in m) ----
  group_by(STATEFP10) %>%
  summarise(total_length = sum(st_length(.), na.rm = TRUE)) %>%
  arrange(desc(total_length)) %>%
  slice(1:3)
print(county_streams)


glimpse(bmps) #2.3 (NEED TO FIX) ----
glimpse(counties)
county_costs <- bmps %>%
  group_by(Geography) %>% 
  summarize(Total_Cost = sum(Cost, na.rm = TRUE))

county_map_data <- counties %>%
  left_join(county_costs, by = c("NAME10" = "Cost"))

ggplot(data = county_map_data) +
  geom_sf(aes(fill = Total_Cost), color = "blue", size=0.4) +
  scale_fill_viridis_c(labels = scales::dollar, option = "magma", na.value = "pink") +
  theme_minimal()+
  labs(
    title = "Total BMP Cost by County",
    fill = "Total_Cost"
  )

tm_shape(county_map_data)+tm_polygons(fill="Cost")



glimpse(streams) #2.4 for each dam, closest stream segment ----
glimpse(dams)
closest_stream <- st_nearest_feature(dams, streams)

dams_spatial <- dams %>%
  mutate(Closest_Stream_ID = streams$OBJECTID_1[closest_stream])

print(dams_spatial)



dams_per_state_summary <- dams_spatial %>% #2.5 how many removed dams in each state ----
  st_drop_geometry() %>% 
  group_by(STATE) %>%
  tally(name = "Total_Removed_Dams") %>%
  arrange(desc(Total_Removed_Dams))

print(dams_per_state_summary)

