# load libraries ----------------------------------------------------------

.libPaths("P:/RLibrary")

library(sf)
library(raster)
library(spData)
library(tidyverse)


# data features -----------------------------------------------------------

data(nz)
head(nz)
data(nz_height)

plot(nz["Sex_ratio"])

# create an object called canterbury that is just the canterbury area

canterbury <- nz %>%  filter(Name == "Canterbury") #

canterbury_height <- nz_height[canterbury,]  # find the spatial points that are within canterbury
canterbury_height

nz_height[canterbury, , op = st_disjoint]

st_intersects(x = nz_height, y = canterbury)

nz_height %>% 
  filter(st_intersects(x = ., y = canterbury, sparse =FALSE))



#  more spatial operators -------------------------------------------------

# create a polygon
a_poly <- st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,-1))))

a = st_sfc(a_poly)

#create a line
l_line <- st_linestring(x = matrix(c(-1,-1,-0.5,1), ncol = 2))
l <- st_sfc(l_line)

# create points
p_matrix <- matrix(c(0.5,1,-1,0,0,1,0.5,1), ncol = 2)
p_multi <- st_multipoint(x = p_matrix)
p <- st_cast(st_sfc(p_multi), "POINT")

plot(a, col = "lightgrey")
plot(l, add = TRUE, col = "red")
plot(p, add = TRUE, col = "blue")


# which of the points in p intersect with the polygon
st_intersects(p, a)
st_intersects(p, a, sparse = FALSE)

# which points are outside of the polygon
st_disjoint(p, a)

# which points are within the polygon
st_within(p, a)

# which points touch the polygon
st_touches(p, a)

# which points are within a distance of 0.9 of the polygon
st_is_within_distance(p, a, dist = 0.9)



# spatial joining ---------------------------------------------------------

set.seed(2018)
data(world)
(bb_world <- st_bbox(world))

random_df <- tibble(
                x = runif(n = 10, min = bb_world[1], max = bb_world[3]),
                y = runif(n = 10, min = bb_world[2], max = bb_world[4])
                )
random_df

random_df  %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(4326) -> randomPoints # set geographic CRS

randomPoints

randJoined <- st_join(randomPoints, world["name_long"])

plot(randJoined)

plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE))

# non-overlapping join

# first transform to a UTM
cycle_hire_P <- st_transform(cycle_hire, 27700)

cycle_hire_osm_P <- st_transform(cycle_hire_osm, 27700)

# find within a distance
sel <- st_is_within_distance(cycle_hire_P,cycle_hire_osm_P, dist = 20)

summary(lengths(sel)>0)

z <- st_join(cycle_hire_P, cycle_hire_osm_P, st_is_within_distance, dist = 20)

nrow(cycle_hire)
nrow(z)


# geometric operations ----------------------------------------------------

seine
plot(seine)

seine_simp <- st_simplify(seine,dTolerance = 2000)
plot(seine_simp)

object.size(seine)
object.size(seine_simp)


usStates2163 <- st_transform(us_states, 2163)
plot(st_geometry(usStates2163))


usStatesSimp <- st_simplify(usStates2163,dTolerance = 100000)
plot(st_geometry(usStatesSimp), col = "lightgrey")
usStates2163$AREA <- as.numeric(usStates2163$AREA)

usStatesSimp2 <- rmapshaper::ms_simplify(usStates2163, 
                                         keep = 0.01,
                                         keep_shapes = TRUE)
                                         
plot(st_geometry(usStatesSimp2))

# buffers

seine_buff_5km <- st_buffer(seine, dist = 5000) # 5km 
plot(seine_buff_5km)

seine_buff_50km <- st_buffer(seine, dist = 50000) # 5km 
plot(seine_buff_50km)

# clipping 
b <- st_sfc(st_point(c(0,1)), st_point(c(1,1)))
b <- st_buffer(b, dist = 1) 

plot(b)
text(x = c(-0.5,1.5), y = 1, labels = c("x","y"))

x <- b[1]
y <- b[2]


x_and_y <- st_intersection(x, y)
plot(b)
plot(x_and_y, col = "lightgrey", add = TRUE)

yx_diff <- st_difference(y,x)
plot(b)
text(x = c(-0.5,1.5), y = 1, labels = c("x","y"))
plot(yx_diff, col = "lightgrey", add = TRUE)

xy_diff <- st_difference(x,y)
plot(b)
plot(xy_diff, col = "lightgrey", add = TRUE)
text(x = c(-0.5,1.5), y = 1, labels = c("x","y"))

xy_union <- st_union(x,y)
plot(b)
plot(xy_union, col = "lightgrey", add = TRUE)
text(x = c(-0.5,1.5), y = 1, labels = c("x","y"))

xy_symdiff <- st_sym_difference(x,y)
plot(b)
plot(xy_symdiff, col = "lightgrey", add = TRUE)
text(x = c(-0.5,1.5), y = 1, labels = c("x","y"))
