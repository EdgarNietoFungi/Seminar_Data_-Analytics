#libray 

# goo ---------------------------------------------------------------------

library(c(tidyverse, sf, spData, spDataLarge
          ))
library(tidyverse)
library(sf)
library(spData)# Spata Large in github
library(spDataLarge)
library(shinyjs)

install.packages("mapview")
install.packages("tmap")
# more libraries ----------------------------------------------------------
library(tmap)
library(leaflet)
library(mapview)

# static map --------------------------------------------------------------

data(nz)
# add afill layer

tm_shape(nz) +
  tm_fill()

# add a boundary
tm_shape(nz) +
  tm_borders()
# combining multiple elements
tm_shape(nz) +
  tm_fill() +
    tm_borders()
map_nz <- tm_shape(nz )+ tm_polygons()
map_nz1 <- map_nz +
  tm_shape(nz_elev)+
  tm_raster(alpha = 0.7)
nz_water <- st_union(nz) %>% st_buffer(22200) %>% 
  st_cast(to = "LINESTRING")# LINE AROUND
map_nz2 <- map_nz1 +
  tm_shape(nz_water)+
  tm_lines()

map_nz3 <- map_nz2 +
  tm_shape(nz_height)+
  tm_dots()

##
tmap_arrange(map_nz1,map_nz2, map_nz3)

# change the asethetics
tm_shape(nz)+
  tm_fill(col = "red")# change the fill colors

tm_shape(nz)+
  tm_fill(col = "red", alpha = 0.3)# change the trasnparent

#ct
tm_shape(nz)+ tm_borders(col = "blue")# change border colors
tm_shape(nz)+ tm_borders(col = "blue", lwd = 4)# change line widt
tm_shape(nz)+ tm_borders(col = "blue", lty = "dashed")# change line
# type
tm_shape(nz)+ tm_fill(col = "red", )

tm_shape(nz)+
  tm_fill(col = "Land_area")+# have fills vary by the data set
  tm_borders(col = "black")
head(nz)

# color setting
tm_shape(nz)+
  tm_polygons(col = "Median_income")
#breaks 
 breaks <- c(0,3,4,5)*10000 # manually setting breaks
tm_shape(nz)+
  tm_polygons(col = "Median_income", breaks = breaks)

breaks <- c(0,3,4,5)*10000 # manually setting breaks
tm_shape(nz)+
  tm_polygons(col = "Median_income",  n= 10) # 10 categories breaks

tm_shape(nz)+
  tm_polygons(col = "Median_income",  palette= "BuGn") 

tmaptools::palette_explorer()# allow you to look at the different color palettes

map_nz +
  tm_compass(type = "8star", position = c("left", "top"))

map_nz +
  tm_compass(type = "8star", position = c("left", "top"))+
  tm_scale_bar(breaks = c(1, 100, 200), size = 1)
map_nz +
  tm_compass(type = "8star", position = c("left", "top"))+
  tm_scale_bar(breaks = c(1, 100, 200), size = 1)

#layouts
map_nz + tm_layout(title = "New Zealand")
map_nz + tm_layout(title = "New Zealand", bg.color = "lightblue")
map_nz + tm_layout(title = "New Zealand", frame = FALSE)

map_nz + tm_style("bw")
map_nz + tm_style("classic")
map_nz + tm_style("cobalt")
map_nz + tm_polygons(col = "Median_income")
map_nz + tm_style("Col_blind")
tm_shape(nz)+ tm_polygons(col = "Median_income")+
  tm_style("col_blind")

us_states_map <- tm_shape(us_states, projection = 2163) +
  tm_polygons()+
  tm_layout(frame = FALSE )

hawaii_map <- tm_shape(hawaii)+
  tm_polygons()+
  tm_layout(title = "Hawaii", frame = FALSE,bg.color = NA, title.position =  c("LEFT", "TOP"))
#runn together
us_states_map
print(hawaii_map, vp = grid::viewport(0.35, 0.1, width = 0.2,height = 0.1))



Alaska_map <- tm_shape(alaska)+
  tm_polygons()+
  tm_layout(title = "Alaska", frame = FALSE,bg.color = NA, title.position =  c("LEFT", "TOP"))

us_states_map
print(Alaska_map, vp = grid::viewport(0.2, 0.85, width = 0.2,height = 0.2))

us_states_map
print(Alaska_map, vp = grid::viewport(0.2, 0.85, width = 0.2,height = 0.2))
print(hawaii_map, vp = grid::viewport(0.35, 0.1, width = 0.2,height = 0.1))

# Animated  maps

urban_anim <- tm_shape(world)+
  tm_polygons()+
  tm_shape(urban_agglomerations)+
  tm_dots(size = "population_millions",col = "red", alpha= 0.5)+
  tm_facets(along = "year", free.coords = FALSE)

tmap_animation(urban_anim, filename = "urban_pop.gif", delay = 25)

#required ImageMagick
install.packages("ImageMagick")
library(ImageMagick)
# R in ackage how to creatte slides for ghere
#interactive ma
#urbanmpr in gitHub
tmap_mode("view")
map_nz
# this is using shiny

world_coffee <- left_join(world, coffee_data, by = "name_long")
facets <- c("coffee_production_2016","coffee_production_2017" )
tm_shape(world_coffee)+
  tm_polygons(facets)+
  tm_facets(nrow = 1,sync = TRUE)
