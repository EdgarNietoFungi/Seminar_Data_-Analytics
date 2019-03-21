
# load libraries ----------------------------------------------------------
.libPaths("P:/RLibrary")
library(tidyverse)
library(sf)
library(spData)
library(spDataLarge)
# 
# install.packages("tmap")
# install.packages("leaflet")
# install.packages("mapview")

library(tmap)
library(leaflet)
library(mapview)

# static maps -------------------------------------------------------------
data(nz) # load the new zealand data
 
# add a fill layer
tm_shape(nz) +
  tm_fill()

# add a boundary
tm_shape(nz) +
  tm_borders()

# Combining multiple elements
tm_shape(nz) +
  tm_fill()+
  tm_borders()

map_nz <- tm_shape(nz) + tm_polygons() 

map_nz +
  tm_shape(nz_elev) +
  tm_raster(alpha = 0.7) -> map_nz1 # Elevation map

nz_water <- st_union(nz) %>% st_buffer(22200) %>% 
  st_cast(to = "LINESTRING") # creating a buffer around NZ

map_nz1 +
  tm_shape(nz_water) + # add the buffer to the nz map
  tm_lines() -> map_nz2

map_nz2 +
  tm_shape(nz_height) + #
  tm_dots() -> map_nz3
map_nz3


tmap_arrange(map_nz1, map_nz2, map_nz3)

## change the aesthetics
 tm_shape(nz)  + tm_fill(col = "red")  # Change the fill colors
 tm_shape(nz)  + tm_fill(col = "red", alpha = 0.3) # Change the transparency
 tm_shape(nz)  + tm_borders(col = "blue") # Change border color
 tm_shape(nz)  + tm_borders(col = "blue", lwd = 4) # Change line width
 tm_shape(nz)  + tm_borders(col = "blue", lty = "dashed") # Change line type
 tm_shape(nz)  + tm_fill(col = "red", alpha = 0.3) + tm_borders(col = "blue", lty = "dashed") #  combine multiple aesthetics
 
 tm_shape(nz) +
   tm_fill(col = "Land_area") + # Have fills vary by the data set
   tm_borders(col = "black")
 
 head(nz)
 names(nz)
 
 #Color settings
 tm_shape(nz) + tm_polygons(col = "Median_income")  # automatic breaks
 breaks <- c(0,3,4,5) * 10000 # manually setting breaks
 tm_shape(nz) + tm_polygons(col = "Median_income", breaks = breaks) 
 tm_shape(nz) + tm_polygons(col = "Median_income", n = 10) # 10 category breaks
 tm_shape(nz) + tm_polygons(col = "Median_income", palette = "BuGn") # change the fill palette
 
 tmaptools::palette_explorer()  # allows you to look at the different color palettes
 
 map_nz + 
   tm_compass(type = "8star", position = c("left","top"))  # add in a compass
 
 map_nz + 
   tm_compass(type = "8star", position = c("left","top")) + # add in a scale bar
   tm_scale_bar(breaks = c(0,100,200), size = 1)

 
 # layouts
 map_nz + tm_layout(title = "New Zealand") # add title
 map_nz + tm_layout(title = "New Zealand", bg.color = "lightblue") # change background color
 map_nz + tm_layout(title = "New Zealand",frame = FALSE) # remove the box around map
 
 map_nz + tm_style("bw") # styles, similar to themes in ggplot2
 map_nz + tm_style("classic")
 map_nz + tm_style("cobalt")
 tm_shape(nz) + tm_polygons(col = "Median_income") + tm_style("col_blind")
 
 
 us_states_map <- tm_shape(us_states, projection = 2163) + # create base US map
   tm_polygons() + 
   tm_layout(frame = FALSE)
   
 us_states_map
 
 hawaii_map <- tm_shape(hawaii) + # create a hawaii map, with no box, no background color, and title in the bottom left
   tm_polygons() +
   tm_layout(title = "Hawaii", frame = FALSE, bg.color = NA, title.position = c("LEFT", "BOTTOM"))
 hawaii_map
#Creating map insets
us_states_map # first display the base map
print(hawaii_map, vp = grid::viewport(0.35,0.1,width = 0.2,height = 0.1)) # print the hawaii map on the US map


## Challenge add Alaska
alaska_map <- tm_shape(alaska) + 
  tm_polygons() +
  tm_layout(title = "Alaska", frame = FALSE, bg.color = NA)

us_states_map # first display the base map
print(hawaii_map, vp = grid::viewport(0.35,0.1,width = 0.2,height = 0.1)) # print the hawaii map on the US map
print(alaska_map, vp = grid::viewport(0.15,0.15,width = 0.3,height = 0.3)) # print the alaska map on the US map


## Animated maps

tm_shape(world) +
  tm_polygons() + 
  tm_shape(urban_agglomerations) +
  tm_dots(size = "population_millions", col = "red", alpha = 0.5) + 
  tm_facets(along = "year", free.coords = FALSE) -> urban_anim

tmap_animation(urban_anim, filename = "urban_pop.gif", delay = 25)

# Interactive maps
tmap_mode("view")
map_nz

world_coffee <- left_join(world, coffee_data, by = "name_long")
head(world_coffee)
facets <- c("coffee_production_2016", "coffee_production_2017")
 
tm_shape(world_coffee) +
  tm_polygons(facets) + 
  tm_facets(nrow = 1)
