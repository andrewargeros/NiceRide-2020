library(tidyverse)
library(dplyr)
library(ggplot2)
library(osmdata)
library(cowplot)
library(patchwork)
library(ggtext)
library(sf)
library(ggpointdensity)
library(beepr)

mpl = getbb("Minneapolis, Minnesota")
stp = getbb("St Paul, Minnesota")

mx = mpl[1,1]
my = mpl[2,2]
sx = stp[1,2]
sy = stp[2,1] 

m = matrix(c(mx, sx, sy, my), ncol = 2, byrow = TRUE)
row.names(m) = c("x", "y")
colnames(m) = c("min", "max") 

#m = getbb("Minneapolis, Minnesota")

streets <- m %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- m %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- m %>%
  opq(timeout = 25*100)%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf() 

member.bike.f = filter(member.bike, 
                       between(Longitude, m[1, 1], m[1, 2]),
                       between(Latitude, m[2, 1], m[2, 2]))


gg=ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = .01,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffffff",
          size = .01,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "#ffffff",
          size = .02,
          alpha = .5) +
  coord_sf(xlim = c(7.77, 7.92), 
           ylim = c(47.94, 48.06),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#282828")
  )


gg1 = ggplot(member.bike.f) +
  geom_sf(data = small_streets$osm_lines, col = 'grey40', size = .1) +
  geom_sf(data = streets$osm_lines, col = 'grey40', size = .4) +
  geom_pointdensity(aes(Longitude, Latitude), size = 6, alpha = .8) +
  geom_sf(data = small_streets$osm_lines, col = alpha('grey40', .2), size = .1) +
  geom_sf(data = streets$osm_lines, col = alpha('grey40', .2), size = .4) +
  scale_color_viridis_c(option = 'inferno') +
  coord_sf(xlim = m[1,], ylim = m[2,], expand = FALSE) + 
  geom_blank() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#282828")
  )

ggsave('black map.png', height = 4.8, width = 4) 


p2 = ggplot(member.bike.f) +
  geom_sf(data = small_streets$osm_lines, col = 'black', size = .1) +
  geom_sf(data = streets$osm_lines, col = 'black', size = .4) +
  geom_sf(data = small_streets$osm_lines, col = alpha('black', .2), size = .1) +
  geom_sf(data = streets$osm_lines, col = alpha('black', .2), size = .4) +
  coord_sf(xlim = coord[1,], ylim = coord[2,], expand = F)


gg3 = ggplot(member.bike.f) +
  geom_sf(data = small_streets$osm_lines, col = 'grey40', size = .1) +
  geom_sf(data = streets$osm_lines, col = 'grey40', size = .4) +
  geom_pointdensity(aes(Longitude, Latitude, color= PCTmember), size = 6, alpha = .8) +
  geom_sf(data = small_streets$osm_lines, col = alpha('grey40', .2), size = .1) +
  geom_sf(data = streets$osm_lines, col = alpha('grey40', .2), size = .4) +
  scale_color_viridis_c(option = 'inferno') +
  coord_sf(xlim = m[1,], ylim = m[2,], expand = FALSE) + 
  geom_blank() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#282828")
  )

gg.total = ggplot(member.bike.f) +
  geom_sf(data = small_streets$osm_lines, col = 'grey40', size = .1) +
  geom_sf(data = streets$osm_lines, col = 'grey40', size = .4) +
  geom_pointdensity(aes(Longitude, Latitude, color= Total), size = 6, alpha = .8) +
  geom_sf(data = small_streets$osm_lines, col = alpha('grey40', .2), size = .1) +
  geom_sf(data = streets$osm_lines, col = alpha('grey40', .2), size = .4) +
  scale_color_viridis_c(option = 'inferno') +
  coord_sf(xlim = m[1,], ylim = m[2,], expand = FALSE) + 
  geom_blank() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#282828")
  )
