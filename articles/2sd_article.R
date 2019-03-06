###################################################################################################
## Forest fragments modulate the provision of multiple ecosystem services
## Matthew Mitchell, Elena Bennett & Andrew Gonzalez - McGill University, Montréal, Québec
## Journal of Applied Ecology
## February 1, 2014
## Generalized Additive Mixed Model Analysis
###################################################################################################

### ****N.B.: This analysis was performed using R Version 3.0.2 and 'mgcv' package 1.7-26.
###           Use of other versions of R and 'mgcv' may results in slightly different values****

#clear R workspace
rm(list=ls())

#load mgcv library
library(mgcv)

###########################################################
#GAMM analysis of 2010 and 2011 aboveground data
###########################################################

#import of separate year data
data.2010 <- read.csv("2010_Above.csv", header=TRUE)
data.2011 <- read.csv("2011_Above.csv", header=TRUE)
#two plots in 2011 were removed from analysis as they were intensively colonized by weed species

#manipulation of 2010 data
#log transform patch size and proximity index
data.2010$log.Patch_Size <- log10(data.2010$Patch_Size)
data.2010$log.Prox <- log10(data.2010$Prox)

#calculation of ungrazed leaves for each census
data.2010$C1_Not_Grz <- data.2010$C1_Tot_Lfts-data.2010$C1_Tot_Grz
data.2010$C2_Not_Grz <- data.2010$C2_Tot_Lfts-data.2010$C2_Tot_Grz

#log transformation of total leaves for use as covariates in GAMM analysis
data.2010$C1_Tot_Lfts_Plt <- data.2010$C1_Tot_Lfts/5
data.2010$C2_Tot_Lfts_Plt <- data.2010$C2_Tot_Lfts/5
#calculation of log leaves per plant (5 plants were censused each time)
data.2010$log.C1_Tot_Lfts_Plt <- log(data.2010$C1_Tot_Lfts_Plt)
data.2010$log.C2_Tot_Lfts_Plt <- log(data.2010$C2_Tot_Lfts_Plt)

#manipulation of 2011 data
#log transform patch size and proximity index
data.2011$log.Patch_Size <- log10(data.2011$Patch_Size)
data.2011$log.Prox <- log10(data.2011$Prox)

#calculation of ungrazed leaves for each census
data.2011$C1_Not_Grz <- data.2011$C1_Tot_Lfts-data.2011$C1_Tot_Grz
data.2011$C2_Not_Grz <- data.2011$C2_Tot_Lfts-data.2011$C2_Tot_Grz
data.2011$C3_Not_Grz <- data.2011$C3_Tot_Lfts-data.2011$C3_Tot_Grz

#log transformation of total leaves for use as covariates in GAMM analysis
data.2011$C1_Tot_Lfts_Plt <- data.2011$C1_Tot_Lfts/5
data.2011$C2_Tot_Lfts_Plt <- data.2011$C2_Tot_Lfts/5
data.2011$C3_Tot_Lfts_Plt <- data.2011$C3_Tot_Lfts/5
#calculation of log leaves per plant (5 plants were censused each time)
data.2011$log.C1_Tot_Lfts_Plt <- log(data.2011$C1_Tot_Lfts_Plt)
data.2011$log.C2_Tot_Lfts_Plt <- log(data.2011$C2_Tot_Lfts_Plt)
data.2011$log.C3_Tot_Lfts_Plt <- log(data.2011$C3_Tot_Lfts_Plt)

#GAMM analysis of 2010 First Census Aphids (Figure S3)
aphids.1.2010 <- gamm(C1_Tot_Aphids/5 ~ offset(log.C1_Tot_Lfts_Plt) +
                        s(Distance,k=5,bs="cr") +
                        s(log.Patch_Size,bs="cr") +
                        s(log.Prox,bs="cr"),
                      random=list(Plot=~1),
                      family=negbin(1),
                      data=data.2010)
summary(aphids.1.2010$gam)

#GAMM analysis of 2010 Second Census Aphids (Figure S3)
aphids.2.2010 <- gamm(C2_Tot_Aphids/5 ~ offset(log.C2_Tot_Lfts_Plt) +
                        s(Distance,k=5,bs="cr") +
                        s(log.Patch_Size,bs="cr") +
                        s(log.Prox,bs="cr"),
                      random=list(Plot=~1),
                      family=negbin(1),
                      data=data.2010)
summary(aphids.2.2010$gam)

#GAMM analysis of 2011 First Census Aphids (Figure S3)
aphids.1.2011 <- gamm(C1_Tot_Aphids/5 ~ offset(log.C1_Tot_Lfts_Plt) +
                        s(Distance,k=5,bs="cr") +
                        s(log.Patch_Size,bs="cr") +
                        s(log.Prox,bs="cr"),
                      random=list(Plot=~1),
                      family=negbin(1),
                      data=data.2011)
summary(aphids.1.2011$gam)

#GAMM analysis of 2011 Second Census Aphids (Figure S3)
aphids.2.2011 <- gamm(C2_Tot_Aphids/5 ~ offset(log.C2_Tot_Lfts_Plt) +
                        s(Distance,k=5,bs="cr") +
                        s(log.Patch_Size,bs="cr") +
                        s(log.Prox,bs="cr"),
                      random=list(Plot=~1),
                      family=negbin(1),
                      data=data.2011)
summary(aphids.2.2011$gam)

#GAMM analysis of 2011 Third Census Aphids (Figure S3)
aphids.3.2011 <- gamm(C3_Tot_Aphids/5 ~ offset(log.C3_Tot_Lfts_Plt) +
                        s(Distance,k=4,bs="cr") +
                        s(log.Patch_Size,bs="cr") +
                        s(log.Prox,bs="cr"),
                      random=list(Plot=~1),
                      family=negbin(1),
                      data=data.2011)
summary(aphids.3.2011$gam)

#GAMM analysis of 2010 Soybean Yield (Table S1)
soy.2010 <- gamm(Soybean ~ s(Distance,k=5,bs='cr') +
                         s(log.Patch_Size,bs='cr') +
                         s(log.Prox,bs='cr'),
                       random=list(Plot=~1),
                       family=gaussian,
                       data=data.2010)
summary(soy.2010$gam)

#GAMM analysis of 2011 Soybean Yield (Table S1)
soy.2011 <- gamm(Soybean ~ s(Distance,k=5,bs="cr") +
                         s(log.Patch_Size,bs="cr") +
                         s(log.Prox,bs="cr"),
                       random=list(Plot=~1),
                       family=gaussian,
                       data=data.2011)
summary(soy.2011$gam)

###########################################################
#2010 Aboveground Repeated Measures GAMM Analysis
###########################################################

#import 2010 repeated measures data file
rep.2010 <- read.csv("2010_Rep_Mes.csv", header=TRUE)

#create variables needed for GAMM analysis
rep.2010$log.Patch_Size <- log10(rep.2010$Patch_Size)
rep.2010$log.Prox <- log10(rep.2010$Prox)
rep.2010$Tot_Lfts_Pl <- rep.2010$Tot_Lfts/5
rep.2010$log.Tot_Lfts_Pl <- log(rep.2010$Tot_Lfts_Pl)
rep.2010$Not_Grz <- rep.2010$Tot_Lfts-rep.2010$Tot_Grz

#GAMM Repeated Measures analysis for 2010 Aphids (Table S1)
aphids.rep.2010 <- mgcv::gamm((Tot_Aphids/5) ~ offset(log.Tot_Lfts_Pl) +
                                s(Distance,k=5,bs="cr") +
                                s(log.Patch_Size,k=4,bs="cr") + 
                                s(log.Prox,bs="cr"),
                              random=list(Plot=~1, Census=~1),
                              correlation=corCompSymm(form=~Census|Distance|Plot),
                              family=negbin(1), 
                              data=rep.2010)
summary(aphids.rep.2010$gam)

#GAMM Repeated Measures analysis for 2010 Grazing (Table S1)
grz.rep.2010 <- mgcv::gamm(cbind(Tot_Grz,Not_Grz) ~ s(Distance,k=5,bs="cr") + 
                             s(log.Patch_Size,bs="cr") +
                             s(log.Prox,bs="cr"),
                           random=list(Plot=~1, Census=~1),
                           correlation=corCompSymm(form=~Census|Distance|Plot),
                           family=quasibinomial,
                           data=rep.2010)
summary(grz.rep.2010$gam)

###########################################################
##2011 Aboveground Repeated Measures GAMM Analysis
###########################################################

##Import 2011 repeated measures data file
rep.2011 <- read.csv("2011_Rep_Mes.csv", header=TRUE)

#create variables needed for GAMM analysis
rep.2011$log.Patch_Size <- log10(rep.2011$Patch_Size)
rep.2011$log.Prox <- log10(rep.2011$Prox)
rep.2011$Not_Grz <- rep.2011$Tot_Lfts-rep.2011$Tot_Grz
rep.2011$Tot_Lfts_Plt <- rep.2011$Tot_Lfts/5
rep.2011$log.Tot_Lfts_Plt <- log(rep.2011$Tot_Lfts_Plt)

#GAMM Repeated Measures analysis for 2011 Aphids (Table S1)
aphids.rep.2011 <- mgcv::gamm((Tot_Aphids/5) ~ offset(log.Tot_Lfts_Plt) +
                                s(Distance,k=5,bs="cr") +
                                s(log.Patch_Size,k=4,bs="cr") +
                                s(log.Prox,k=4,bs="cr"),
                              random=list(Plot=~1, Census=~1),
                              correlation=corCompSymm(form=~Census|Distance|Plot),
                              family=negbin(1),
                              data=rep.2011)
summary(aphids.rep.2011$gam)

#GAMM Repeated Measures analysis for 2011 Grazing (Table S1)
grz.rep.2011 <- mgcv::gamm(cbind(Tot_Grz,Not_Grz) ~ s(Distance,k=5,bs="cr") + 
                             s(log.Patch_Size,bs="cr") +
                             s(log.Prox,bs="cr"),
                           random=list(Plot=~1, Census=~1),
                           correlation=corCompSymm(form=~Census|Distance|Plot),
                           family=quasibinomial,
                           data=rep.2011)
summary(grz.rep.2011$gam)

###########################################################
##2010 Belowground GAMM Analysis
###########################################################

#Import 2010 belowground repeated measures data file
below.2010 <- read.csv("2010_Below.csv", header=TRUE)

#Create variables needed for GAMM analysis
below.2010$Cotton_Prop <- below.2010$Cotton_Loss/100
below.2010$P_Sorption_Prop <- below.2010$P_Sorption/100
below.2010$C_Prop <- below.2010$C/100
below.2010$N_Prop <- below.2010$N/100
below.2010$log.Patch_Size <- log10(below.2010$Patch_Size)
below.2010$log.Prox <- log10(below.2010$Prox)
below.2010$Litter_Holes_Prop <- below.2010$Litter_Loss_Holes/100

#GAMM Analysis for 2010 Cotton Fabric Loss (Table S1)
Cotton.2010 <- mgcv::gamm(Cotton_Prop ~ s(Distance,k=4,bs="cr") +
                            s(log.Patch_Size,bs="cr") +
                            s(log.Prox,bs="cr"),
                          random=list(Plot=~1),
                          family=quasibinomial,
                          data=below.2010)
summary(Cotton.2010$gam)

#GAMM Analysis for 2010 P Sorption (Table S1)
P.2010 <- mgcv::gamm(P_Sorption_Prop ~ s(Distance,k=4,bs="cr") +
                       s(log.Patch_Size,bs="cr") +
                       s(log.Prox,bs="cr"),
                     random=list(Plot=~1),
                     family=quasibinomial,
                     data=below.2010)
summary(P.2010$gam)

#GAMM Analysis for 2010 Soil Carbon (Table S1)
C.2010 <- mgcv::gamm(C_Prop ~ s(Distance,k=4,bs="cr") +
                       s(log.Patch_Size,bs="cr") +
                       s(log.Prox,bs="cr"),
                     random=list(Plot=~1),
                     family=quasibinomial,
                     data=below.2010)
summary(C.2010$gam)

#GAMM Analysis for 2010 Soil Nitrogen (Table S1)
N.2010 <- gamm(N_Prop ~ s(Distance,k=4,bs="cr") +
                       s(log.Patch_Size,bs="cr") +
                       s(log.Prox,bs="cr"),
                     random=list(Plot=~1),
                     family=quasibinomial,
                     data=below.2010)
summary(N.2010$gam)

#GAMM Analysis for 2010 Litterbags (Table S1)
Litter.2010 <- mgcv::gamm(Litter_Holes_Prop ~ s(Distance,k=4,bs="cr") + 
                           s(log.Patch_Size,bs="cr") + 
                           s(log.Prox,bs="cr"),
                         random=list(Plot=~1),
                         family=quasibinomial,
                         data=below.2010)
summary(Litter.2010$gam)

###########################################################
##2011 Belowground GAMM Analysis
###########################################################

#Import 2011 Belowground data file
below.2011 <- read.csv("2011_Below.csv", header=TRUE)

#Creation of variables needed for GAMM analysis
below.2011$P_Sorption_Prop <- below.2011$P_Sorption_015/100
below.2011$Cotton_Prop <- below.2011$C1_Cotton_Loss/100
below.2011$C_Prop <- below.2011$C_015/100
below.2011$N_Prop <- below.2011$N_015/100
below.2011$log.Patch_Size <- log10(below.2011$Patch_Size)
below.2011$log.Prox <- log10(below.2011$Prox)
below.2011$Litter_Holes_Prop <- below.2011$Litter_Loss_Holes/100

#GAMM Analysis for P Sorption (Table S1)
P.2011 <- mgcv::gamm(P_Sorption_Prop ~ s(Distance,k=5) +
                          s(log.Patch_Size,bs="cr") +
                          s(log.Prox,bs="cr"),
                        random=list(Plot=~1),
                        family=quasibinomial,
                        data=below.2011)
summary(P.2011$gam)

#GAMM Analysis for First Census Cotton Loss (Table S1)
Cotton.2011 <- mgcv::gamm(Cotton_Prop ~ s(Distance,k=5,bs="cr") + 
                             s(log.Patch_Size,bs="cr") +
                             s(log.Prox,bs="cr"),
                           random=list(Plot=~1),
                           family=quasibinomial,
                           data=below.2011)
summary(Cotton.2011$gam)

#GAMM Analysis for Soil Carbon (Table S1)
C.2011 <- mgcv::gamm(C_Prop ~ s(Distance,k=5,bs="cr") + 
                           s(log.Patch_Size,bs="cr") +
                           s(log.Prox,bs="cr"),
                         random=list(Plot=~1),
                         family=quasibinomial,
                         data=below.2011)
summary(C.2011$gam)

#GAMM Analysis for Soil Nitrogen (Table S1)
N.2011 <- mgcv::gamm(N_Prop ~ s(Distance,k=4,bs="cr") +
                           s(log.Patch_Size,k=4,bs="cr") +
                           s(log.Prox,k=4,bs="cr"),
                         random=list(Plot=~1),
                         family=quasibinomial,
                         data=below.2011)
summary(N.2011$gam)

#GAMM Analysis for 2011 Litterbags (Table S1)
Litter.2011 <- mgcv::gamm(Litter_Holes_Prop ~ s(Distance,k=5,bs="cr") +
                           s(log.Patch_Size,bs="cr") + 
                           s(log.Prox,bs="cr"),
                         random=list(Plot=~1),
                         family=quasibinomial,
                         data=below.2011)
summary(Litter.2011$gam)
