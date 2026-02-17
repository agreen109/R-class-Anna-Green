library(tidyverse)
library(ggplot2)
library(sf)

p.counties<-"./County_Boundaries.shp"
d.counties<-sf::read_sf(p.counties)

p.stations<-"./Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"
d.stations<-sf::read_sf(p.stations)

glimpse(d.counties)
glimpse(d.stations)
d.stations %>% sf::st_is_valid() #checking for validity
d.counties%>%sf::st_is_valid() #checking for validity
d.counties%>%group_by(STATEFP10) %>% mutate(stateLandArea=sum(ALAND10)) #land area of all counties
d.counties<-d.counties%>%sf::st_make_valid() #fixing it in place
d.counties%>% #sum of all the land area in each state
  as_tibble()%>%dplyr::select(-geometry)%>%
  group_by(STATEFP10)%>%
  summarise(stateLandArea=sum(ALAND10))
land.perc<-d.counties%>%mutate(land.perc=100*(ALAND10)/(ALAND10+AWATER10))
glimpse(land.perc) #1.1 land area as percentage of the total area ----

land.water<-d.counties%>%mutate(land.water=(AWATER10)/(ALAND10+AWATER10))
glimpse(land.water)
land.water<-d.counties%>%mutate(land.water=(AWATER10)/(ALAND10+AWATER10))%>%slice_max(land.water) #1.2 largest proportion of land as water ----
glimpse(land.water)

d.counties%>%count(STATEFP10) #1.3 count number of counties in each state ----

short.name<-d.stations%>%group_by(STATION_NA)
short.name<-d.stations%>%slice_min(short.name) #1.4 station with the shortest name ---- 
glimpse(short.name)

mydf<-read_csv("./County_boundaries.shp")

glimpse(mydf)

d.counties%>% #2.1 scatter plot showing relationship between land and water area ----
  ggplot(.,aes(x=ALAND10,y=AWATER10))+
  geom_point(color="blue",shape=16)+
  labs(title="relationship between land and water area",
       x="land area",
       y="water area")
  
Drainage_A<-d.stations%>%group_by(Drainage_A)
glimpse(Drainage_A)

ggplot(data=Drainage_A,aes(x=Drainage_A))+ #2.2 histogram of drainage area for all monitoring systems ----
geom_histogram(color="blue",fill="pink")+
  labs(title = "drainage area for all monitoring stations",
       x="drainage area",
       y="stations") 

analyze_and_sort<-function(x){ #3.1 A,B,C,D write a function ----
  if(!is.numeric(x)){
    stop("Error: Input must be a numeric vector :) .")
  }
  v_mean<-mean(x,na.rm=TRUE)
  v_median<-median(x,na.rm=TRUE)
  v_max<-max(x,na.rm=TRUE)
  v_min<-min(x,na.rm=TRUE)
  v_sorted<-sort(x)
  return(list(
  A=c(mean=v_mean,median=v_median,max=v_max,min=v_min),
  B=v_sorted
  ))
}

my_vec<-c(1,0,-1) #vector test 1 ----
result<-analyze_and_sort(my_vec)
print("Statistics(A):")
print(result$A)
print("Sorted Vector (B):")
print(result$B)

my_vec<-c(10,1000,100) #vector test 2 ----
result<-analyze_and_sort(my_vec)
print("Statistics(A):")  
print(result$A)
print("Sorted Vector (B):")
print(result$B)

my_vec<-c(.1,.001,1e8) #vector test 3 ----
result<-analyze_and_sort(my_vec)
print("Statistics(A):")  
print(result$A)
print("Sorted Vector (B):")
print(result$B)

my_vec<-c("a","b","c") #vector test 4 with error message ----
result<-analyze_and_sort(my_vec)

glimpse(d.stations) #This is wrong need to fix, number of monitoring systems in each state ----
d.stations%>%count(USGS_ID)
num.stat<-d.stations%>%count(USGS_STATI)
d.stations%>%count(USGS_STATI)
