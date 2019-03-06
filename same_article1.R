
# set library -------------------------------------------------------------

#.libPaths("")# designs where you install R packages in case if the packages are saved on a specific file

# libraries ---------------------------------------------------------------

library(tidyverse)


# input data --------------------------------------------------------------

birdata <- read_csv("data/1st_article_MASTER_AllCountData.csv", na = c("", "NA", "n/a"))# read csv also srtim ws
head(birdata)

# modify data -------------------------------------------------------------
unique(birdata$Height_m)

birdata_rev<-birdata %>% 
  mutate(Height_m =ifelse( Height_m== "n/a", NA, Height_m))  
  #filter(Count_type == "Auto")


# create adat set of just the true values ---------------------------------

actData <- birdata_rev %>% 
  filter(Notes == "This is the true number of individuals for the colony") %>%
  rename(actCount = Count) %>% 
  select(Colony, actCount)

head(actData)


# join the actual data to the original data with the ‘true number’ --------

birdata2 <- birdata_rev %>% 
  filter(is.na(Notes)) %>% 
  left_join(actData, by = "Colony")

head(birdata2)

birdata3 <- birdata2 %>% 
  filter(Count_type %in% c("Ground", "UAV_manual")| (Count_type == "Auto"& percent_Input== 10) )

#we don need to use droplevels because read_csv does not read as factpr

lvlz <- c("Ground", "30 UAV_manual", "30 Auto", "60 UAV_manual", "60 Auto", "90 UAV_manual", "90 Auto", "120 UAV_manual", "120 Auto")

birdata4 <- birdata3 %>%
  mutate(
    absError = abs(actCount - Count),
    # create two columns with new calculations
    tech = paste(Count_type, Height_m, sep = "_"),
    tech = case_when(tech == "Ground_NA" ~ "Ground", TRUE ~ tech),
    tech = factor(tech, levels = lvlz)
  ) %>% select(-Notes,-percent_Input)

# quasipoisson uses algoritms but for count data but not specific model
# with uses the data 

