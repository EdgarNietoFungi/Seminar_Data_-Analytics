
# load libraries ----------------------------------------------------------

install.packages(c("mgcv", "vegan", "nlme"))#
#mgcv allows to gam modellling and vegan to multivariate analyses. nlem allows d o mixed effect modelling and different autocorrelation structures

library(tidyverse)
library(mgcv)
library(vegan)
library(nlme)
R.Version()# display version
citation() # how to cite it
citation(package = "mgcv")
citation(package = "mgcv")
toBibtex (citation(package = "mgcv"))
update.packages()# or use pacakages-- and update and also check 
#Also install R tools
#but the problem is update is the
#clear R workspace
#rm(list=ls())
#double is numeric with decimal places

data2010 <- read_csv("data/2010_above.csv")# read in the 2010 above data
head(data2010)
tail(data2010,10)
data2011 <- read_csv("data/2011_above.csv")# read in the 2010 above data
head(data2011,10)
tail(data2010,10)

# manipulatying dtaa ------------------------------------------------------

#manipulate
data2010.rev <-  data2010 %>% mutate(logPathSize = log10(Patch_Size),#log transform ..
                    logProx = log10(Prox),
                    C1NotGrz= C1_Tot_Lfts-C1_Tot_Grz,
                    C2NotGrz = C2_Tot_Lfts-C2_Tot_Grz,
                    C1TotLftsPlt= C1_Tot_Lfts/5,
                    C2TotLftsPlt= C2_Tot_Lfts/5,
                    logC1TotLftsPlt= log(C1TotLftsPlt),
                    logC2TotLftsPlt= log(C2TotLftsPlt))#C1_Not_Grz no uses as just for functions

#create a function for 2011

#meanwhile
data2011.rev <-  data2011 %>% mutate(logPathSize = log10(Patch_Size),#log transform ..
                                     logProx = log10(Prox),
                                     C1NotGrz= C1_Tot_Lfts-C1_Tot_Grz,
                                     C2NotGrz = C2_Tot_Lfts-C2_Tot_Grz,
                                     C3NotGrz = C3_Tot_Lfts-C3_Tot_Grz,
                                     C1TotLftsPlt= C1_Tot_Lfts/5,
                                     C2TotLftsPlt= C2_Tot_Lfts/5,
                                     C3TotLftsPlt= C3_Tot_Lfts/5,
                                     logC1TotLftsPlt= log(C1TotLftsPlt),
                                     logC2TotLftsPlt= log(C2TotLftsPlt),
                                     logC3TotLftsPlt= log(C3TotLftsPlt))#C1_Not_Grz no uses as just for functions


# GAMM analysis of 2010 Second Census Aphids (Figure S3) ------------------


