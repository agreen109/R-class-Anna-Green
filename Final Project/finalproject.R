library(tidyverse)
install.packages("dataRetrieval")
library(dataRetrieval)
library(sf)
library(leaflet)
library(leaflet.extras)
install.packages("maps")
library(maps)

#static map of chesepeake bay monitoring stations
states_map <- map_data("state")

bay_states_map <- states_map %>%
  filter(region %in% c("virginia", "maryland", "pennsylvania", "delaware", "west virginia", "new york", "district of columbia"))

static_map_maps <- ggplot() +
  geom_polygon(data = bay_states_map, aes(x = long, y = lat, group = group), 
               fill = "gray95", color = "gray50") +
  geom_point(data = site_summaries, aes(x = Longitude, y = Latitude, size = Avg_KLoad, color = Avg_Q), alpha = 0.8) +
  scale_color_viridis_c(name = "Average Flow (Q)", option = "plasma") +
  scale_size_continuous(name = "Average Load") +
  coord_quickmap(
    xlim = c(min(site_summaries$Longitude) - 1, max(site_summaries$Longitude) + 1),
    ylim = c(min(site_summaries$Latitude) - 1, max(site_summaries$Latitude) + 1)
  ) +
  theme_minimal() +
  labs(title = "Chesapeake Bay Stations (Using 'maps' package)", x = "Longitude", y = "Latitude")

print(static_map_maps)


#now making this above map in leaflet 
pal <- colorNumeric(palette = "YlOrRd", domain = site_summaries$Avg_Q)

leaflet(data = site_summaries) %>%
  addTiles() %>% 
  addCircleMarkers(
    ~Longitude, ~Latitude,
    radius = ~sqrt(Avg_KLoad) / 1000, 
    color = ~pal(Avg_Q),
    stroke = TRUE,
    fillOpacity = 0.8,
    popup = ~paste("Station ID:", STAID, "<br>Load:", round(Avg_KLoad, 0))
  ) %>%
  addLegend("bottomright", pal = pal, values = ~Avg_Q, title = "Avg Flow")

#now trying a different map format using ggplot
library(maps)

rim_data <- read_csv("RIM_2023_AnnualLoadTable.csv") %>%
  mutate(STAID = str_pad(as.character(STAID), width = 8, pad = "0"))

unique_sites <- unique(rim_data$STAID)

site_info <- readNWISsite(unique_sites) %>%
  select(site_no, dec_lat_va, dec_long_va) %>%
  rename(STAID = site_no, Latitude = dec_lat_va, Longitude = dec_long_va)

site_summaries <- rim_data %>%
  left_join(site_info, by = "STAID") %>%
  filter(`Water Year` >= 2013) %>%
  group_by(STAID, STNAM, Latitude, Longitude) %>%
  summarize(Avg_KLoad = mean(KLoad, na.rm = TRUE), .groups = "drop")

states_data <- map_data("state") %>%
  filter(region %in% c("virginia", "maryland", "pennsylvania", "delaware", "west virginia", "new york"))

ggplot() + geom_polygon(data = states_data, aes(x = long, y = lat, group = group), 
  fill = "white", color = "black") +
  geom_point(data = site_summaries, 
             aes(x = Longitude, y = Latitude, size = Avg_KLoad, color = Avg_KLoad), 
             alpha = 0.7) +
  scale_color_viridis_c(option = "mako") +
  coord_fixed(1.3) + # Corrects aspect ratio for the US
  theme_minimal() +
  labs(title = "Chesapeake Bay RIM Stations",
       subtitle = "Sized and colored by Average Load (2013-2023)",
       x = "Longitude", y = "Latitude")

#one more map for fun using tigris
library(tigris)
library(sf)

mid_atlantic <- states(cb = TRUE, resolution = "20m") %>%
  filter(STUSPS %in% c("VA", "MD", "PA", "DE", "WV", "NY", "DC"))

sites_sf <- site_summaries %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4269)

ggplot() +
  geom_sf(data = mid_atlantic, fill = "gray98", color = "gray60") +
  geom_sf(data = sites_sf, aes(size = Avg_KLoad, color = Avg_KLoad), alpha = 0.8) +
  scale_color_distiller(palette = "YlOrRd", direction = 1) +
  theme_void() + # Removes grid lines/background for a clean "map" look
  labs(title = "Chesapeake Bay Monitoring Network",
       size = "Avg Annual Load",
       color = "Avg Annual Load")


#table to show flow and load over last 5 years
rim_data <- read_csv("RIM_2023_AnnualLoadTable.csv") %>%
  mutate(STAID = str_pad(as.character(STAID), width = 8, pad = "0"))

table_summary <- rim_data %>%
  filter(`Water Year` >= 2019) %>%
  group_by(STNAM, PCODE) %>%
  summarize(
    Avg_Flow = mean(Q, na.rm = TRUE),
    Avg_Load = mean(KLoad, na.rm = TRUE),
    Max_Conc = max(KConc, na.rm = TRUE),
    .groups = "drop"
  )
 print(table_summary)
 
 
 
 #chart in ggplot (this is a terrible representation)
 
 rim_data <- read_csv("RIM_2023_AnnualLoadTable.csv") %>%
   mutate(STAID = str_pad(as.character(STAID), width = 8, pad = "0"))

 pcode_labels <- c(
   "P00600" = "Total N",
   "P00665" = "Total P",
   "P80154" = "Sus Sed"
 )
 
 table_data <- rim_data %>%
   filter(`Water Year` == 2023) %>%
   mutate(
     Param_Name = pcode_labels[PCODE],
     Short_Name = str_remove(STNAM, " AT | NEAR | RIVER"),
     Short_Name = str_trunc(Short_Name, 25)
   ) %>%
   filter(!is.na(Param_Name))

 print(table_data) 
 
 ggplot(table_data, aes(x = Param_Name, y = Short_Name)) +
   geom_tile(aes(fill = KLoad), color = "white") +
   geom_text(aes(label = scales::comma(KLoad)), size = 3) +
   scale_fill_gradient(low = "#e3f2fd", high = "#1565c0", name = "Annual Load") +
   scale_x_discrete(position = "top") +
   theme_minimal() +
   labs(
     title = "2023 Annual Loads by Station and Pollutant",
     subtitle = "Values represent KLoad (Total Annual Load)",
     x = "", y = ""
   ) +
   theme(
     axis.text.x = element_text(face = "bold"),
     panel.grid = element_blank(),
     plot.title = element_text(hjust = 0.5, face = "bold"),
     plot.subtitle = element_text(hjust = 0.5)
   )
 
 
 #this is a great chart that represents nitrogen and phophorus load trends 
 install.packages("scales")
 library(scales)
 
 rim_data <- read_csv("RIM_2023_AnnualLoadTable.csv")
 
 ts_data <- rim_data %>%
   filter(STAID == "01578310") %>% # Susquehanna River at Conowingo, MD
   filter(PCODE %in% c("P00600", "P00665")) %>%
   mutate(Parameter = ifelse(PCODE == "P00600", "Total Nitrogen", "Total Phosphorus"))

 ggplot(ts_data, aes(x = `Water Year`, y = KLoad, color = Parameter)) +
   geom_line(size = 1) +
   geom_point() +
   facet_wrap(~Parameter, scales = "free_y") +
   theme_minimal() +
   scale_y_continuous(labels = label_comma()) +
   labs(
     title = "Annual Nutrient Loads: Susquehanna River (1985-2023)",
     subtitle = "Evaluating long-term trends in Nitrogen and Phosphorus",
     y = "Annual Load (kg/yr)",
     x = "Water Year"
   ) +
   theme(legend.position = "none") 
 
 
 #scatter plot with trend line of flow vs sediment load 
 scatter_data <- rim_data %>%
   filter(PCODE == "P80154", `Water Year` >= 2010)
 
 ggplot(scatter_data, aes(x = Q, y = KLoad)) +
   geom_point(aes(color = STNAM), alpha = 0.6) +
   geom_smooth(method = "lm", color = "black", linetype = "dashed") +
   scale_x_log10(labels = label_log()) +
   scale_y_log10(labels = label_log()) +
   theme_minimal() +
   labs(
     title = "Flow vs. Sediment Load (Log Scale)",
     subtitle = "Evaluating the impact of discharge on sediment transport",
     x = "Average Discharge ($Q$)",
     y = "Annual Load ($KLoad$)"
   ) +
   theme(legend.position = "none")
 
 
 
 #boxplot of monitoring stations with median nutrient concentration 
 boxplot_data <- rim_data %>%
   filter(PCODE == "P00600") %>%
   # Shorten names for better labels
   mutate(ShortName = str_trunc(STNAM, 20))
 
 ggplot(boxplot_data, aes(x = reorder(ShortName, KConc, FUN = median), y = KConc)) +
   geom_boxplot(fill = "steelblue", outlier.alpha = 0.3) +
   coord_flip() + # Flip for easier reading of station names
   theme_classic() +
   labs(
     title = "Nitrogen Concentration Distribution by Station",
     subtitle = "Stations ordered by median concentration (1985-2023)",
     x = "Monitoring Station",
     y = "Concentration (mg/L)"
   )
 
 
 
 
 
  