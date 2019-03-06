library(sf)
library(raster)
library(spData)
library(tidyverse)

# data features -----------------------------------------------------------

data(nz)
head(nz)
plot(nz)

plot(nz["Sex_ratio"])
# American comunity services package
#create an object called canterbury that iis just canter area
canterbury <- nz %>% filter(Name=="Canterbury")
canterbury_height <- nz_height[canterbury, ] # find the special points that are within canterbury
#data is points
plot(canterbury[0])
points(canterbury_height)
nz_height[canterbury, , op= st_disjoint]# between comas from spaces in columns
nz_height[canterbury,]
##
st_intersects(x= nz_height, y= canterbury)
nz_height %>% 
  filter(st_intersects(x = ., y = canterbury, sparse = FALSE))

# more spatial operators --------------------------------------------------

a_poly <- st_polygon(list(rbind(c(-1,-1),c(1,-1),c(1,1),c(-1,-1))))

a = st_sfc (a_poly)# so it trnsform to a dtaaframe
# create a line
> l_line <- st_linestring((x= matrix(c(-1,-1, 0.5, 1),ncol = 2)))
l <- st_sfc(l_line)
