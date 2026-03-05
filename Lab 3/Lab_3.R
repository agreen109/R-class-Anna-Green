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
