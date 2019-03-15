
# load libraries ----------------------------------------------------------
.libPaths("P:/RLibrary")
library(tidyverse)
library(sp)
# devtools::install_github("andrewzm/STRbook", force = TRUE)
library(STRbook)
# install.packages("spacetime")
library(spacetime)

# Load data
locs <- read_table(system.file("extdata","Stationinfo.dat", package = "STRbook"), col_names = c("id", "lat", "long")) # usign the package STRbook

locs <- read_table("data/Stationinfo.dat", col_names = c("id", "lat", "long")) 
head(locs)

Times <- read_table2("data/Times_1990.dat", col_names = c("julian", "year", "month","day"))
head(Times)

Tmax <- read_table2("data/Tmax_1990.dat", col_names = as.character(locs$id),
                    na = "-9999")
head(Tmax)

Tmin <- read_table2("data/Tmin_1990.dat", col_names = as.character(locs$id))

TDP <- read_table2("data/TDP_1990.dat", col_names = as.character(locs$id))

Precip <- read_table2("data/Precip_1990.dat", col_names = as.character(locs$id))


# data wrangling ----------------------------------------------------------
Tmax <- cbind(Times, Tmax) # bind the two datasets together by column
head(Tmax)

# Challenge
#1.  Wide to long format
#2. id to an integer
#3. add a column that is tmax
#4. filter out all NAs

Tmax %>% 
  gather(id, value, -julian, -year, -month, -day) %>% 
  mutate(id = as.integer(id),
         type = "tmax") %>% 
  filter(!is.na(value)) -> Tmax2

## TDP
TDP <- cbind(Times, TDP) # bind the two datasets together by column

TDP %>% 
  gather(id, value, -julian, -year, -month, -day) %>% 
  mutate(id = as.integer(id),
         type = "TDP") %>% 
  filter(value >= 0) -> TDP2

## Tmin
Tmin <- cbind(Times, Tmin) # bind the two datasets together by column

Tmin %>% 
  gather(id, value, -julian, -year, -month, -day) %>% 
  mutate(id = as.integer(id),
         type = "tmin") %>% 
  filter(value >= 0) -> Tmin2

## Precip
Precip <- cbind(Times, Precip) # bind the two datasets together by column

Precip %>% 
  gather(id, value, -julian, -year, -month, -day) %>% 
  mutate(id = as.integer(id),
         type = "prec") %>% 
  filter(value >= 0) -> Precip2

#Combine all the files together
NOAA_df_1990 <- rbind(Tmax2,Tmin2,TDP2,Precip2)

#Challenge 
#1.Combine with the location data
#2. Create a column called Date and is a date format
#3. Create a column called t that is julian - 728050

left_join(NOAA_df_1990, locs, by = "id") %>% 
  mutate(Date = paste(year,month,day, sep = "-"),
         Date = as.Date(Date),
         t = julian - 728050) -> NOAA_df_1990

head(NOAA_df_1990)


ggplot(data = NOAA_df_1990 %>% filter(type =="tmax", t %in% c(1,15,30))) +
  geom_point(aes(x = long, y = lat, colour = value), size = 2) + 
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group)) +
  facet_wrap(~Date, ncol = 1) + 
  coord_map(projection = "sinusoidal") + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_bw()

### 
data("BEA", package = "STRbook")
BEA            
data("MOcounties", package = "STRbook")

MOcounties2 <- left_join(MOcounties,BEA, by = "NAME10")
head(MOcounties2)

# Create a plot with the per capita income for each missouri county with a facet for 1970, 1980, 1990

