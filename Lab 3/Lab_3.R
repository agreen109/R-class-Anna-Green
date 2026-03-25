
#working through prelab data ----
install.packages("tidycensus")
library(tidycensus)
library(tidyverse)
install.packages("spdep")
library(spdep)
library(sf)
library(tmap)
options(tigris_use_cache=TRUE)
d.all<-get_acs(geography="county",
               table="B15003",
               output="wide",
               geometry=T,
               year=2020)
glimpse(d.all)

df.vars<-load_variables(2020,"acs5",cache = T)
glimpse(df.vars)

d.sex.by.age<-get_acs(geography="county",
                      table="B01001",
                      output="wide",
                      geometry=T,
                      year=2020)

glimpse(d.sex.by.age)

ohio<-d.sex.by.age %>%
  mutate(.,STATEID=stringr::str_sub(GEOID,1,2)) %>%
  dplyr::filter(STATEID=="39")

tmap::tm_shape(ohio)+tm_polygons()

sf::st_crs(ohio)

ohio.projected<-ohio%>%sf::st_transform(.,"ESRI:102010")
tmap::tm_shape(ohio.projected)+tm_polygons()
nb<-spdep::poly2nb(ohio.projected,queen=TRUE)

nb
nb[[1]]

ohio.projected$NAME[1]
nb[[1]]%>%ohio.projected$NAME[.]

lw<-spdep::nb2listw(nb, style="W",zero.policy=TRUE)

lw$weights[1]

neighbors<-attr(lw$weights,"comp")$d
hist(neighbors)

F75.lag<-lag.listw(lw,ohio.projected$B01001_047E)
F75.lag
moran.test(ohio.projected$B01001_047E,lw)
moran.plot(ohio.projected$B01001_047E,lw,zero.policy=TRUE,plot=TRUE)

#starting lab 3 work ----

fivestates <- c("Ohio", "Pennsylvania", "West Virginia", 
                     "Indiana", "Michigan") #1, using 5 states ----
fiveSS <- us_states %>%
  filter(NAME %in% fivestates) %>%
  st_transform(crs = 5070)
glimpse(fiveSS)


fiveSS <- fiveSS %>% #2, total population variable ----
  mutate(
    area_sqkm = as.numeric(st_area(.)) / 1e6, 
    pop_density = total_pop_15 / area_sqkm
  )
glimpse(fiveSS)


hist(fiveSS$pop_density, #3, histogram of population density ----
     main = "Histogram of population density",
     xlab = "population density (people per km^2)",
     col = "lightgreen")
    

tm_shape(fiveSS) +    #4, chloropleth make of population density ----
  tm_polygons(col = "pop_density",
              title = "population density (per km^2)",
              palette = "Greens") +
  tm_layout(main.title = "Choropleth map of population density")


queen <- poly2nb(fiveSS, queen = TRUE) #picking queen for question 5 ----


queenlistw <- nb2listw(queen, style = "W", zero.policy = TRUE) #5.1 row standardize the W ----


numneighbors <- card(queen) #5.2 plot a histogram of number of neighbors ----
hist(numneighbors,
     main = "Number of neighbors (using queen)",
     xlab = "Number of neighbors",
     col = "darkgreen")


averageneighbors <- mean(numneighbors) #5.3 average number of neighbors ----
cat("Average number of neighbors (Queen):", round(avgerageneighbors, 2), "\n")
glimpse(averageneighbors)


moran.plot(fiveSS$pop_density, #5.4 make a moran plot ----
           listw = queenlistw,
           main = "Moran plot using queen",
           xlab = "Population density",
           ylab = "Spatially lag population density")


coordinates <- st_coordinates(st_centroid(st_geometry(fiveSS)))

nearestn2 <- knn2nb(knearneigh(coordinates, k = 1))
dist <- max(unlist(nbdists(nearestn2, coordinates)))
dist2 <- dnearneigh(coordinates, d1 = 0, d2 = dist * 1.5)
glimpse(dist2)
dists3 <- nbdists(dist2, coordinates)
idw_weights <- lapply(dists3, function(x) 1 / x)
glimpse(idw_weights)


WS_idw <- nb2listw(dist2, glist = idw_weights, style = "W", zero.policy = TRUE) #6.1 standardizing the  W using idw ---- 


number_neighbors_idw <- card(dist2) #6.2 making histogram of number of neighboors using IDW ----
hist(num_neighbors_idw,
     main = "Histogram of number of neighbors based on distance",
     xlab = "Number of neighbors",
     col = "limegreen")


average_neighbors_idw <- mean(number_neighbors_idw) #6.3 average number of neighbors using IDW ----
cat("Average number of neighbors using IDW:", round(average_neighbors_idw, 2), "\n")


moran.plot(fiveSS$pop_density, #6.4 make a moran plot using IDW ----
           listw = WS_idw,
           main = "Moran plot using IDW",
           xlab = "Population pensity",
           ylab = "Spatially lag population density")

#Question 1----
#Morans I is calculated by analyzing a value of a location compared to the locations neighbors. In a Moran I, you first calculate the spatial matrix to define the loactions neighborhood, after this you can calculate Morans I by using a moran.text and find the value.  

#Question 2----
#A spatially lagged variable is the weighted average of a the neighbors value in a certain area. 

#Question 3----
#In questions 5.1-5.4, I utilized a standard W through my calculations. This W assigns all neighbors an equal value, meaning they all have te same influence on the central veraible. using the IDW menthod however, does not assign all neighbors the same values, but rather assigns closer neighbors a stronger value and furhter neighbors a weaker value. 
#This difference in methods might affect analysis by giving different spatial lag values between the two Moran plots. The Moran plot using the W queen continuity had a spatially lagged population density of 70-110, while the Moran plot using IDW had a spatailly lagged population density of 65-100. 

#Question 4----
#If an observation falls in the H-L quadrent, this means that the observation was an outlier. The H-L quadrent represents the High-Low area of a Moran plot, so if a value falls here, it signifies that the observation had a high value but a is surrounded by neighbors with low values. 
#This may be useful to detect such observations because it detects spatial outliers within a data set, which can be a useful tool when wanting to identify outliers in a data set. Finding outliers in a data set can be useful in many different ways, and could be used to identify places of interest for study or errors in computation. 


