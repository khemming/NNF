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
  
  dat.c <- dplyr::select(dat, species, family, genus, latitude, longitude, year, status) %>%
    group_by(species)
  
# which are the most common exotic families? -----------------------------------------
  dat.c$status <- as.factor(dat.c$status)
  ex.fam2 <- group_by(dat.c, family, status )%>%
              summarise(n = n()) %>%
              arrange(desc(n))
  head(ex.fam2)
  
  dat.exotic <- filter(ex.fam2, status == 0) %>%
                mutate(exotic = n) %>%
                dplyr::select(-n, -status)
  
  
  dat.native <- filter(ex.fam2, status == 1) %>%
    mutate(native = n) %>%
    dplyr::select(-n, -status)
              
  
  dat.ne <- left_join(dat.exotic, dat.native, by = "family") %>%
            mutate(total = native + exotic)
  head(dat.ne) 
  
###################################################################################### 
# rarefaction ------------------------------------------------------------------------
######################################################################################
  
# Rarefaction set-up ------------------------------------------------------- 
  spp <- filter(dat.c, family == "Asteraceae")
  
# number of records by grid square
  xy <- cbind(spp$longitude, spp$latitude)
  spp$cell <- raster::extract(raster, xy)
  
# number of records for only cells with records in them
  n_rec <- table(spp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), 
                   n_rec = as.vector(n_rec))
  
  cell_rec_occ <- spp %>%
    group_by(cell) %>%
    summarise(n_rec = n())      
  
# number of records for all cells 
  n_tot <- rasterize(xy, raster, fun = function(x,...) length(x))
  v_tot <- getValues(n_tot)
  cell_rec_tot <- data.frame(cell = 1:length(v_tot), n_tot = v_tot)
  
# check the dataframes line up
  ch_rv1 <- full_join(cell_rec_occ, cell_rec_tot)      
  plot(log(n_tot) ~ log(n_rec), data = ch_rv1) # they agree
  
  
  
  
  
  
  
  
  
  