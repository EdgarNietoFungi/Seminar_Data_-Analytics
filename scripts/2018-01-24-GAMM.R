
# load libraries ----------------------------------------------------------

.libPaths("P:/RLibrary") # set the path to the directory where you keep your R library.  

install.packages(c("mgcv", "vegan","nlme")) # install the mgvc, vegan, and nlme packages. mgvc allows you to do the gam modelling and vegan allows you to do multivariate analyses. nlme allows you to do mixed effect modelling and different autocorrelation structures 

library(mgcv) # load the libraries 
library(vegan)
library(nlme)
library(tidyverse)


# load the data -----------------------------------------------------------

R.Version() # display what version of R 
citation() # get citation for R
citation(package = "mgcv") # get citation for packages
toBibtex(citation(package = "mgcv")) # citations in Bibtex format
update.packages() # update already installed packages

data2010 <- read_csv("data/2010_Above.csv") # read in the 2010 above data
head(data2010) # look at the first 6 rows
tail(data2010) # look at the last 6 rows
data2011 <- read_csv("data/2011_Above.csv") # read in the 2011 above data
head(data2011) # look at the first 6 rows
tail(data2011) # look at the last 6 rows



# manipulate data ---------------------------------------------------------

## Manipulate 2010 data
data2010 %>% 
  mutate(logPatchsize = log10(Patch_Size), # log transform patchsize and prox
         logProx = log10(Prox),
         C1NotGrz = C1_Tot_Lfts - C1_Tot_Grz, #calculation of ungrazed leaves for each census
         C2NotGrz = C2_Tot_Lfts - C2_Tot_Grz,
         C1TotLftsPlt = C1_Tot_Lfts/5,
         C2TotLftsPlt = C2_Tot_Lfts/5,
         logC1TotLftsPlt = log(C1TotLftsPlt), #log transformation of total leaves for use as covariates in GAMM analysis
         logC2TotLftsPlt = log(C2TotLftsPlt)) -> data2010.rev

as.data.frame(data2010.rev) # displaying as a data.frame
View(data2010.rev) #opening the spreadsheet style viewer

## Manipulate 2011 data
data2011 %>% 
  mutate(logPatchsize = log10(Patch_Size), # log transform patchsize and prox
         logProx = log10(Prox),
         C1NotGrz = C1_Tot_Lfts - C1_Tot_Grz, #calculation of ungrazed leaves for each census
         C2NotGrz = C2_Tot_Lfts - C2_Tot_Grz,
         C3NotGrz = C3_Tot_Lfts - C3_Tot_Grz,
         C1TotLftsPlt = C1_Tot_Lfts/5,
         C2TotLftsPlt = C2_Tot_Lfts/5,
         C3TotLftsPlt = C3_Tot_Lfts/5,
         logC1TotLftsPlt = log(C1TotLftsPlt), #log transformation of total leaves for use as covariates in GAMM analysis
         logC2TotLftsPlt = log(C2TotLftsPlt),
         logC3TotLftsPlt = log(C3TotLftsPlt)) -> data2011.rev


# GAM analysis ------------------------------------------------------------
#GAMM analysis of 2010 First Census Aphids (Figure S3)
aphids2010mod1 <- gamm(C1_Tot_Aphids/5 ~ offset(logC1TotLftsPlt) + # offset is standardizing output by the total leaves on a plant, must be log transformed
                        s(Distance,k=5,bs="cr") + # smoothing terms, k represents "window of smoothing", bs is the type of spline in the smoothing
                        s(logPatchsize,bs="cr") +
                        s(logProx,bs="cr"),
                      random=list(Plot=~1), # random intercept for plot.  account for variation by plot but not explicitly calculate paramters for plot
                      family=negbin(1), # dependent variable follows negative binomial distribution
                      data=data2010.rev)

summary(aphids2010mod1$gam)
aphids2010mod1

# Challenge:  create a histogram of C1_Tot_Aphids and C1_Tot_Aphids/5 and natural log of C1_Tot_Aphids in ggplot2

data2010.rev %>% 
  select(Plot,C1_Tot_Aphids) %>% # limiting data to two columns
  mutate(C1_Tot_Aphids.5 = C1_Tot_Aphids/5, # data transformations
         C1_Tot_Aphids.log = log(C1_Tot_Aphids+1)) %>% 
  gather(type, value, C1_Tot_Aphids:C1_Tot_Aphids.log) %>% # create data in long form
  ggplot() + # pipe directly into ggplot
  geom_histogram(aes(x = value, fill = type), colour = "black") + # values for histogram are in the value column, set the fill colors in the histogram based on type column, edge color black
  facet_wrap(~type, ncol = 3, scales = "free_x") + # creating subplots based on type, in three columns and we want x to vary by subplot
  coord_cartesian(ylim = c(0,25), expand = FALSE) + # eliminating that damn 'pretty space'
  theme_classic() + # use classic theme
  theme(legend.position = "none") # delete the legend

exp(predict(aphids2010mod1$gam))

## create new data

newdata <- data.frame(Distance = min(data2010.rev$Distance):max(data2010.rev$Distance),
                      logPatchsize = mean(data2010.rev$logPatchsize),
                      logProx = mean(data2010.rev$logProx),
                      logC1TotLftsPlt = mean(data2010.rev$logC1TotLftsPlt))

data.frame(newdata,
      fit = predict(aphids2010mod1$gam, newdata = newdata, type = "response", se = TRUE)$fit,
      se.fit = predict(aphids2010mod1$gam, newdata = newdata, type = "response", se = TRUE)$se.fit)

#Other increments
seq(min(data2010.rev$Distance),max(data2010.rev$Distance), by = 10)
data2010.rev
