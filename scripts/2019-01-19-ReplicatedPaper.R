
# load libraries ----------------------------------------------------------
.libPaths("P:/Rlibrary")

library(tidyverse)


# input data --------------------------------------------------------------

birdData <- read_csv("data/MASTER_AllCountData.csv", na = c("", "NA", "n/a")) # read csv data

birdData <- read_csv("data/MASTER_AllCountData.csv") # read csv data

head(birdData)


# modify data -------------------------------------------------------------
unique(birdData$Height_m) # look at the unique values 

birdData %>% 
  mutate(Height_m = ifelse(Height_m == "n/a",NA, Height_m)) -> birdData.rev
# create a data set of just the true values

birdData.rev %>% 
  filter(Notes == "This is the true number of individuals for the colony") %>% 
  rename(actCount = Count) %>% 
  select(Colony, actCount) -> actData

head(actData)

# join the actual data to the original data with the 'true number' rows filtered out
birdData.rev %>% 
  filter(is.na(Notes)) %>% 
  left_join(actData, by = "Colony") -> birdData2

head(birdData2)

birdData2 %>% 
  filter(Count_type %in% c("Ground","UAV_manual") | (Count_type == "Auto" & percent_Input == 10)) -> birdData3

# birdData3 %>% 
#   filter(Count_type == "Auto")

lvlz <- c("Ground", "UAV_manual_30", "UAV_manual_60",
          "UAV_manual_90", "UAV_manual_120",
          "Auto_30", "Auto_60", "Auto_90", "Auto_120")

birdData3 %>% 
  mutate(absDiff = abs(actCount - Count),
         tech = paste(Count_type, Height_m, sep = "_"),
         tech = case_when(tech == "Ground_NA" ~ "Ground",
                          TRUE ~ tech),
         tech = factor(tech, levels = lvlz)) %>% 
  dplyr::select(-Notes, -percent_Input) -> birdData4

birdData4

mod1 <- glm(absDiff ~ tech + as.factor(Colony), family = "quasipoisson", data = birdData4)
summary(mod1)
summary(aov(mod1))

mod2 <- MASS::glm.nb(absDiff ~ tech + as.factor(Colony),data = birdData4)
summary(mod2)
summary(aov(mod2))


mod3 <- glm(absDiff ~ tech * as.factor(Colony), family = "quasipoisson", data = birdData4)
summary(mod3)
summary(aov(mod3))

mod4 <- glm(absDiff ~  as.factor(Colony) * tech, family = "quasipoisson", data = birdData4)
car::Anova(mod4, type = 3)
car::Anova(mod3, type = 3)

m1r <- MASS::glmmPQL(absDiff ~ tech, random = ~1 |as.factor(Colony), family = "quasipoisson", data = birdData4)
summary(m1r)

TukeyHSD(aov(m1r))

library(multcomp)
summary(glht(m1r, linfct= mcp(tech = "Tukey")))
