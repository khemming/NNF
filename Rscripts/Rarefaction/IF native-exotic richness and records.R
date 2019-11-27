# date created: 27/2
# updated:


# Rarefied richness ------------------------------------------------------------------------  
# What I want to do now is to run the top five invasive families through the rarefaction process, obtaining their native/exotic rarefied richness, at 10-50 records, at 100-km.
# Based on 'Rarefaction chapter model v5' in Rarefaction chapter 

# steps:
# list these when I know what they are


# Library ------------------------------------------------------------------
  library(dplyr)
  library(raster)
  library(rgdal)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")
  
# data -----------------------------------------------------------------------------
# species
  dat <- read.csv("AVH/2019 master data/AVH master data.csv")
  colnames(dat) <- c("scientific.name", 
                     "matched.scientific.name", 
                     "order",
                     "family", 
                     "genus", 
                     "species", 
                     "latitude",
                     "longitude", 
                     "IBRA.region", 
                     "state", 
                     "year",
                     "taxon.identification.issue", 
                     "inferred.duplicate.Record", 
                     "name.not.in.national.checklists", 
                     "status")
  
  dat.c <- dplyr::select(dat, species, family, genus, latitude, longitude, year, status)
  
# which are the most common exotic families by records? ----------------------------------
  dat.c$status <- as.factor(dat.c$status)
  ex.fam2 <- group_by(dat.c, family, status )%>%
              summarise(n = n()) %>%
              arrange(desc(n))
  head(ex.fam2)
  
# exotic records by family  
  dat.exotic <- filter(ex.fam2, status == 0) %>%
                mutate(exotic.rec = n) %>%
                dplyr::select(-n, -status)
  
# native records by family  
  dat.native <- filter(ex.fam2, status == 1) %>%
    mutate(native.rec = n) %>%
    dplyr::select(-n, -status)
              
# merge  
  dat.ne <- left_join(dat.exotic, dat.native, by = "family") %>%
            mutate(total.rec = native.rec + exotic.rec)
  head(dat.ne) 

# what is their species richness? ----------------------------------------------------
  dat.c$species <- as.factor(dat.c$species)

  nat.spp <- filter(dat.c, status == 1) %>%
              dplyr::select(family, species) %>%
              distinct()
  nat.spp2 <- group_by(nat.spp, family) %>%
    summarise(native.spp = n()) %>%
    arrange(desc(native.spp))
  head(nat.spp2)
  
  exo.spp <- filter(dat.c, status == 0) %>%
              dplyr::select(family, species)  %>%
              distinct()
  exo.spp2 <- group_by(exo.spp, family) %>%
              summarise(exotic.spp = n()) %>%
              arrange(desc(exotic.spp))
  head(exo.spp2)
  
# merge  
  spp.ne <- left_join(exo.spp2, nat.spp2, by = "family") %>%
    mutate(total.spp = native.spp + exotic.spp)  
 
# merge record and spp richness data frames --------------------
  invasive <- left_join(dat.ne, spp.ne, by = "family")
  head(invasive)
  
# save
  write.csv(invasive, "C:/Users/s436862/Dropbox/Invasive families/Results/CSV/Records & species richness of plant families of Australia.csv", row.names = F)
  