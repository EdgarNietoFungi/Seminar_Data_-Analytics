
hi <- read.csv ("Data/1st_article_MASTER_AllCountData.csv")
actData <- hi %>% filter(Notes=="This is the true number of individuals for the colony") 
#hi %>% filter(Notes=="This is the true number of individuals for the colony") -> actData

actData <- hi %>% 
  filter(Notes=="This is the true number of individuals for the colony") %>%
  rename(ActCount= Count) %>% 
  select(Colony, ActCount) 
# taht is the Key
hi2 <- hi %>% 
  filter(Notes != "This is the true number of individuals for the colony") %>%
  left_join(actData, by= "Colony") 

# hi %>% 
#   filter(is.na(Notes)) %>%
#   left_join(actData, by= "Colony") -> hi2

# Create columns of absolute error and deviance

hi3 <- hi2 %>% 
  mutate(absError = abs(ActCount - Count), # create two columns with new calculations
         diffCount = Count - ActCount) %>% 
  select(-Notes)# remove notes column

unique(hi3$Count_type)
unique(hi3$Height_m)

lvl <- c("Ground", "30 UAV_manual", "30 Auto", "60 UAV_manual", "60 Auto", "90 UAV_manual", "90 Auto", "120 UAV_manual", "120 Auto")


# reset the factor levels 

hi4 <- hi3 %>% 
  mutate(heightType = paste(Height_m,Count_type), # paste height and Count Type into a single column
         heightType = case_when(heightType == "n/a Ground" ~ "Ground", # replace the n/a Ground to Ground and keep everything else the same
                                TRUE ~ heightType),
         heightType = factor(heightType, levels = lvl))# making them factor because they were characters before

unique(hi4$heightType)


# box plots of the deviance -----------------------------------------------


plotA <- 
ggplot(data = hi4) + 
  geom_boxplot(aes(x = heightType, y = absError, fill = Count_type)) +
  coord_flip()  #fliping the bars
  theme_classic() +
  labs(x = "Height and Count type", y = "Absolute error") +
  theme(legend.position = "none") 

# mine version
plotA <- 
  ggplot(data = hi4, aes(x = heightType, y = absError, fill = Count_type)) + 
  geom_boxplot() +
  coord_flip() +
  theme_classic() +
  labs(x = "Height and Count type", y = "Absolute error") +
  theme(legend.position = "none") 

plotB <- 
ggplot(data = hi4) + 
  geom_boxplot(aes(x = heightType, y = diffCount, fill = Count_type)) +
  labs(y = "Difference from true count") +
  coord_flip() #fliping the bars
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) 

#mine version
plotB <-
ggplot(data = hi4, aes(x = heightType, y = diffCount, fill = Count_type)) + 
  geom_boxplot() +
  labs(y = "Difference from true count") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) 
# with packagae gridExtra into 2 columns
grid.arrange(plotA,plotB, ncol = 2)

