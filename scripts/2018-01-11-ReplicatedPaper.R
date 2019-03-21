
# set library -------------------------------------------------------------
.libPaths("P:/Rlibrary") #designates where you install R packages

# install.packages("tidyverse") # install tidyverse if needed
# install.packages("gridExtra")

library(tidyverse)
library(gridExtra)


# load data ---------------------------------------------------------------

birdData <-read_csv("data/MASTER_AllCountData.csv") # read csv from data folder

glimpse(birdData) # look at the data 

unique(birdData$Notes)


# manipulate data ---------------------------------------------------------

# create a data set of just the true values
birdData %>% 
  filter(Notes == "This is the true number of individuals for the colony") %>% 
  rename(actCount = Count) %>% 
  select(Colony, actCount)-> actData

# birdData %>% 
#   filter(!is.na(Notes)) -> actData

head(actData)

# join the actual data to the original data with the 'true number' rows filtered out
birdData %>% 
  filter(is.na(Notes)) %>% 
  left_join(actData, by = "Colony") -> birdData2

# equal arrows (<- or ->) can be used to write data to object

# birddata2 <- birdData %>% 
#               filter(is.na(Notes)) %>% 
#               left_join(actData, by = "Colony") 

# Create columns of absolute error and deviance

birdData2 %>% 
  mutate(absError = abs(actCount - Count), # create two columns with new calculations
         diffCount = Count - actCount) %>% 
  select(-Notes) -> birdData3# remove notes column

unique(birdData3$Count_type)
unique(birdData3$Height_m)

lvl <- c("Ground", "30 UAV_manual", "30 Auto", "60 UAV_manual", "60 Auto", "90 UAV_manual", "90 Auto", "120 UAV_manual", "120 Auto")

birdData3 %>% 
  mutate(heightType = paste(Height_m,Count_type), # paste height and Count Type into a single column
         heightType = case_when(heightType == "n/a Ground" ~ "Ground", # replace the n/a Ground to Ground and keep everything else the same
                                TRUE ~ heightType),
         heightType = factor(heightType, levels = lvl)) -> birdData4 # reset the factor levels 

unique(birdData4$heightType)


# box plots of the deviance -----------------------------------------------

ggplot(data = birdData4) + 
  geom_boxplot(aes(x = heightType, y = absError, fill = Count_type)) +
  coord_flip() +
  theme_classic() +
  labs(x = "Height and Count type", y = "Absolute error") +
  theme(legend.position = "none") -> plotA

ggplot(data = birdData4) + 
  geom_boxplot(aes(x = heightType, y = diffCount, fill = Count_type)) +
  labs(y = "Difference from true count") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) -> plotB

grid.arrange(plotA,plotB, ncol = 2)
