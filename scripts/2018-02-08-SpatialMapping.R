
# load libraries ----------------------------------------------------------
#urbnmapr package
.libPaths("P:/RLibrary")

library(sf)
library(raster)
library(spData)
library(tidyverse)


# data features -----------------------------------------------------------
data(world)  # load world data from spData

names(world) # look at column names
glimpse(world) # look at more detail; notice the nested dataframes in the geom column

plot(world) # plot each map with fills for each column

# descriptive statistics
summary(world["lifeExp"])  
summary(world$lifeExp)

# subset data 
worldMini <- world[1:2,1:3]
worldMini
plot(worldMini)


# basic plotting ----------------------------------------------------------
plot(world[3:6])
plot(world["pop"])

worldAsia <- world[world$continent == "Asia",]
plot(worldAsia)
asia <- st_union(worldAsia)
plot(asia)

#combine plots
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")# with the add function

# plot centroid in each country scaled by population size
plot(world["continent"], reset = FALSE)
cex <- sqrt(world$pop) / 10000

worldCentr <- st_centroid(world, of_largest = TRUE)
plot(st_geometry(worldCentr), add = TRUE, cex = cex)

india <- world[world$name_long == "India",]
plot(st_geometry(india), col = "grey", lwd = 3)
plot(st_geometry(india), expandBB = c(0,0.2,0.1,1), col = "grey", lwd = 3)
plot(worldAsia[0], add = TRUE)


# rasters -----------------------------------------------------------------
library(spDataLarge)

devtools::install_github("Nowosad/spDataLarge")

rasterFilePath <- system.file("raster/srtm.tif", package = "spDataLarge") # creates a path to a raster file in the spDataLarge folder

newRaster <- raster(rasterFilePath)
newRaster

plot(newRaster)

# raster bricks = stack of rasters
multiRaster <- system.file("raster/landsat.tif", package = "spDataLarge") # creates a path to a raster file in the spDataLarge folder

rasterBrick <- brick(multiRaster)
rasterBrick

nlayers(rasterBrick)

plot(rasterBrick)


rasterOndisk <- raster(rasterBrick, layer = 1) # brings in raster on desk
rasterInmemory <- raster(xmn = 301905, xmx = 335745, # set the dimensions of a new raster
                         ymn = 4111245, ymx = 4154085,
                         res = 30)
values(rasterInmemory) <- sample(seq_len(ncell(rasterInmemory))) # randomly assign values to each cell in the new raster that goes from 1 to the number of cells in the raster

names(rasterInmemory) <- "Our made up raster" # Changing names of layers

crs(rasterInmemory) <- crs(rasterOndisk) # setting equal the coordinate reference system

rasterStack <- stack(rasterInmemory,rasterOndisk) # stacking the two layers
plot(rasterStack) # plotting each lay


# working with CRS in R ---------------------------------------------------

st_proj_info(type = "proj") # displays available projections

crsData <- rgdal::make_EPSG() # display projectsions
View(crsData)

vectorPath <- system.file("vector/zion.gpkg", package = "spDataLarge") # creates a path to a vector file in the spDataLarge folder

zion <- st_read(vectorPath)
plot(zion[0]) # plots just the outline

st_crs(zion) # retrieves the crs for an object

newVector <- st_set_crs(zion, 4326) # set the crs for the object. Often used if the crs is missing but it is known

zion2 <- st_transform(zion, 4326) # reprojects the data

plot(zion2[0], col = "grey")

# reproject zion in NAD83 UTM zone 12N
zion3 <- st_transform(zion, 26912)

plot(zion2[0], col = "grey")
plot(zion3[0], col = "grey")


# units -------------------------------------------------------------------

world %>% 
  filter(name_long == "Luxembourg") -> luxembourg

st_area(luxembourg)  # calculate area of a polygon

units::set_units(st_area(luxembourg), km^2) #change units with set_units function

world.df <- st_drop_geometry(world) # drop geometries from the dataset



# a couple of challenges --------------------------------------------------

#1 Calculate the total population for each continent using dplyr
world %>% 
  dplyr::select(pop, continent) %>% 
  group_by(continent) %>% 
  summarise(ttlPop = sum(pop, na.rm = TRUE)) %>% 
  arrange(desc(ttlPop)) %>% 
  st_drop_geometry()
#2 What 3 continents have the greatest population 

# first option finds the total population, sorts it, and then slices 1:3
world %>% 
  dplyr::select(pop, continent) %>% 
  group_by(continent) %>% 
  summarise(ttlPop = sum(pop, na.rm = TRUE)) %>% 
  arrange(desc(ttlPop)) %>%
  st_drop_geometry() %>% 
  slice(1:3)
#  ?? st_drop_
#?sf::
#
# calculates total pop and then uses top_n to choose the top 3 from ttlPop

world %>% 
  dplyr::select(pop, continent) %>% 
  group_by(continent) %>% 
  summarise(ttlPop = sum(pop, na.rm = TRUE)) %>% 
  st_drop_geometry() %>% 
  top_n(3, wt = ttlPop)

# merge world data with coffee_data to a dataset called worldCoffee

data("coffee_data")

world %>% 
  dplyr::left_join(coffee_data, by = "name_long") -> worldCoffee

nrow(worldCoffee)
nrow(world)

plot(worldCoffee["coffee_production_2017"])

world %>% 
  inner_join(coffee_data, by = "name_long") -> worldCoffee_reduced

nrow(worldCoffee_reduced)
nrow(coffee_data)

setdiff(coffee_data$name_long, worldCoffee_reduced$name_long) # displays names that are different

world$name_long[grepl("congo", tolower(world$name_long))] #find location where congo is listed
stringr::str_subset(world$name_long, "Dem*.+Congo") # string subset to find dem and congo

# change the name of name_long if it is equal to "Congo, Dem. Rep. of" with that from the world data, otherwise leave it alone. 
coffee_data %>% 
  mutate(name_long = ifelse(name_long =="Congo, Dem. Rep. of", stringr::str_subset(world$name_long, "Dem*.+Congo"), name_long)) -> coffeeRenamed

coffeeRenamed$name_long
# using united_states data
## Week Challenge

# 1. Create a new object called us_states_name that contains only the NAME column from the us_states object. What is the class of the new object and what makes it geographic?

#2. Select columns from the us_states object which contain population data. Obtain the same result using a different command (bonus: try to find three ways of obtaining the same result). Hint: try to use helper functions, such as contains or starts_with from dplyr (see ?contains).

# 3. Find all states with the following characteristics (bonus find and plot them):
#   - Belong to the Midwest region.
#   - Belong to the West region, have an area below 250,000 km2 and in 2015 a population greater than 5,000,000 residents (hint: you may need to use the function units::set_units() or as.numeric()).
#   - Belong to the South region, had an area larger than 150,000 km2 or a total population in 2015 larger than 7,000,000 residents.
# 4. What was the total population in 2015 in the us_states dataset? What was the minimum and maximum total population in 2015?
# 5. How many states are there in each region?
# 6. What was the minimum and maximum total population in 2015 in each region? What was the total population in 2015 in each region?
# 7. Add variables from us_states_df to us_states, and create a new object called us_states_stats. What function did you use and why? Which variable is the key in both datasets? What is the class of the new object?
# 8. us_states_df has two more rows than us_states. How can you find them? (hint: try to use the dplyr::anti_join() function)
# 9. What was the population density in 2015 in each state? What was the population density in 2010 in each state?
# 10. How much has population density changed between 2010 and 2015 in each state? Calculate the change in percentages and map them.

# 11. Change the columns’ names in us_states to lowercase. (Hint: helper functions - tolower() and colnames() may help.)
# 12. Using us_states and us_states_df create a new object called us_states_sel. The new object should have only two variables - median_income_15 and geometry. Change the name of the median_income_15 column to Income.
# 13. Calculate the change in median income between 2010 and 2015 for each state. Bonus: What was the minimum, average and maximum median income in 2015 for each region? What is the region with the largest increase of the median income?