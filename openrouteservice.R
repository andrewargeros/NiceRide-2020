library(openrouteservice)
ors_api_key("5b3ce3597851110001cf6248749acdcca12b47c4bcc96d81689a334a")

trips = paste0(bike$start_station, "-", bike$end_station)
trips = as.data.frame(table(trips))

lat1 = member.bike[which(member.bike$Station=="Lake Calhoun Center"),"Latitude"]
lon1 = member.bike[which(member.bike$Station=="Lake Calhoun Center"),"Longitude"]

lat2 = member.bike[which(member.bike$Station=="Englewood Ave & N Asbury Street"), "Latitude"]
lon2 = member.bike[which(member.bike$Station=="Englewood Ave & N Asbury Street"), "Longitude"]

lat3 = member.bike[which(member.bike$Station=="IDS Center"), "Latitude"]
lon3 = member.bike[which(member.bike$Station=="IDS Center"), "Longitude"]

lat4 = member.bike[which(member.bike$Station=="Lake Nokomis"), "Latitude"]
lon4 = member.bike[which(member.bike$Station=="Lake Nokomis"), "Longitude"]
 
coords = list(c(lon1, lat1), c(lon2, lat2))
coords2 = list(c(lon3, lat3), c(lon4, lat4))
directs = ors_directions(coords)
directs2 = ors_directions(coords2)


pt1 = ors_geocode(lon1, lat1)

ors_profile("roadbike")
library(leaflet)
leaflet() %>% 
  addProviderTiles(providers$Stamen.Toner) %>% 
  addGeoJSON(directs, fill = F, color = "#EE2737", opacity = 1) %>% 
  addGeoJSON(directs2, fill = F, color = "#EE2737", opacity = 1) %>% 
  addGeoJSON(pt1, color = "#EE2737") %>% 
  fitBBox(directs$bbox)
names(providers)
