
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






