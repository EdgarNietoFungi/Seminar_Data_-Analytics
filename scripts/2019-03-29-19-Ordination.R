
# load library ------------------------------------------------------------
.libPaths("P:/RLibrary")
library(tidyverse)
#install.packages('ggfortify')
library(ggfortify)
# install.packages('vegan')
library(vegan)
library(ggrepel)


# load data ---------------------------------------------------------------

commData <- read_csv2("data/dryad-Robuchon-et-al-JEcol.csv")

glimpse(commData)
unique(commData$taxon)
unique(commData$site_id)

## Challenge - get the data in a usable format.  Species across the columns, sites by the rows


commData %>% 
  select(taxon, site_id, abundance) %>% # select the needed columns
  mutate(taxon = gsub(" ","_", taxon)) %>% # remove spaces from species names
  group_by(taxon, site_id) %>% # group by taxon and site
  summarise(maxN = max(abundance)) %>% #find max abundance for each species
  spread(taxon, maxN, fill = 0) -> commMatrix # go to wide format, filling in 0 for missing values

#  Check the data for low species abundance or rare species
rowSums(commMatrix[,-1]) ## all rows (sites) have plenty of species

spp<-names(commMatrix[,-1]) # pull out column names

ind <- (colSums(commMatrix[,-1])<10) #find which are true

nospp1<- spp[ind] # display specis that have less than 10

commMatrix %>% 
  select(-one_of(nospp1)) -> commMatrix1 # drops rare species

dim(commMatrix1)

# drop infrequent

spp<-names(commMatrix1[,-1]) # pull out column names

ind <- (colSums(decostand(commMatrix1[,-1], method = "pa"))<=2) #find which are true

nospp2<- spp[ind] # display specis that only occur at two or less sites

commMatrix1 %>% 
  select(-one_of(nospp2)) -> commMatrix2 # drops infrequent species

dim(commMatrix2) # check to see if everyone is on the same page


# principal component analysis (PCA) --------------------------------------
commMatrix2 <- as.data.frame(commMatrix2)
rownames(commMatrix2) <- commMatrix2$site_id
commMatrix2 <- commMatrix2[,-1]

#change into correct format
?princomp

pcaMod <- prcomp(commMatrix2, scale = TRUE)
summary(pcaMod)

scores(pcaMod)

autoplot(pcaMod, loadings = TRUE, label = TRUE, loadings.label = TRUE)


# non-metric multidimensional scaling -------------------------------------

mdsMod<- metaMDS(commMatrix2, k = 2)
mdsMod2<- metaMDS(commMatrix2, k = 2, distance = "euclidean")
mdsMod
mdsMod2


plot(mdsMod, type = "t")

data.scores <- as.data.frame(scores(mdsMod))

data.scores$site <- paste("Site", row.names(data.scores))

species.scores <- as.data.frame(scores(mdsMod, "species"))

species.scores %>% rownames_to_column("Species") -> species.scores


ggplot() + 
  geom_text_repel(data = species.scores, aes(x = NMDS1, y = NMDS2, label = Species), alpha = 0.5, color = "red", size = 2) + 
  geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2), size = 1) +
  geom_text_repel(data = data.scores, aes(x = NMDS1, y = NMDS2, label = site)) +
  theme_classic()

