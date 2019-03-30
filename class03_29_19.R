
library(tidyverse)
commmData <- read.csv2("data/dryad-Robuchon-et-al-JEcol.csv")   
str(commmData
    )
unique(commmData$taxon
)
unique(commmData$site_id
)
hi <- commmData %>% 
  select(taxon, site_id, abundance)%>%
  group_by(taxon, site_id) %>% 
summarise(maxN = max(abundance)) %>% 
  spread(taxon, maxN, fill= 0)# all rows (sites) have plenty of species


hi

rowSums(hi[,-1])
spp <- names((hi[,-1]))
ind <-colSums(hi[,-1])<10

nospp1 <- spp[ind] # display species tha have less than 10
commmData1 <-commmData %>%
   select(-one_of(nospp1))# d

library(vegan)



decostand(commmData1[,-1], method = "pa")


#
spp2 <- names((commmData1[,-1]))
ind2 <- (colSums(commmData1[,-1])<10)

nospp2 <- spp2[ind2] # display species tha have less than 10
commmData2 <-commmData1 %>%
  select(-one_of(nospp2))#
       


