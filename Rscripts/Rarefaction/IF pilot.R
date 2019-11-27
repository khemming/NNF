

# date created: 22/2

# plot other invasive families in Australia ---------------------------------------------

# library 
  library(dplyr)
  library(raster)
  library(rgdal)

  rm(list = ls())

  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")

# data (I'm using for now ...)
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
                           "native")
  
  dat.c <- dplyr::select(dat, species, family, genus, latitude, longitude, year, native) %>%
                   group_by(species)
  
# Which exotic families are most prevalent? ---------------------------------------------------------
# are there lots of exotic records in the AVh data?
  exo <- filter(dat.c, native == 0)

# which are the most common exotic families?
  exo.fam <- group_by(exo, family) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  head(exo.fam)  
  
# check it out:
# Poaceae       46513
# Asteraceae    24975
# Fabaceae      22300
# Brassicaceae  7368
# Cyperaceae    5937
# Malvaceae     5665
  
# df of top three: family, total records, native record, exotic records  
  dat.fam2 <- group_by(dat, family, native) %>%
              summarise(n = n())

# plotting the top three ------------------------------------------------------------------------  
# steps:
# plot nat/exo records
# plot n/e species richness
# do these using Eng_mann()

# raster template
  templ <- raster("EVs/Rasters 100-km/arid.grd")
  plot(templ)
  
# shapefile
  aus <- readOGR("Australia/australia_shapefile.shp")
  plot(aus)
  
# top three invasive families  
  ast <- filter(dat.c, family == "Asteraceae")
  fab <- filter(dat.c, family == "Fabaceae")
  bras <- filter(dat.c, family == "Brassicaceae")
  
  
# Asteraceae -----------------------------------------------
  spp <- ast
  
# rasterise record coordinates
  xy <- cbind(spp$longitude, spp$latitude)
  sps <- as.numeric(factor(spp$species))
  
# number of records (n.rec)
  n.rec <- rasterize(xy, templ, fun = function(x,...) {length(na.omit(x)) })
# crop extra cells  
  n.rec.crop <- mask(n.rec, aus)
  plot(log(n.rec.crop))
  
# species richness (sr)
  sr <- rasterize(xy, templ, field = sps, fun = function(x,...) {length(unique(na.omit(x))) })  
  sr.crop <- mask(sr, aus)
  plot(sr.crop)

# Fabaceae ---------------------------------------------------------------------------------
  spp <- fab

# rasterise record coordinates
  xy <- cbind(spp$longitude, spp$latitude)
  sps <- as.numeric(factor(spp$species))
  
# number of records (n.rec)
  n.rec <- rasterize(xy, templ, fun = function(x,...) {length(na.omit(x)) })
# crop extra cells  
  n.rec.crop <- mask(n.rec, aus)
  plot(log(n.rec.crop))
  
# species richness (sr)
  sr <- rasterize(xy, templ, field = sps, fun = function(x,...) {length(unique(na.omit(x))) })  
  sr.crop <- mask(sr, aus)
  plot(sr.crop)

 
# Brassicaceae ---------------------------------------------------------------------------------
  spp <- bras
  
# rasterise record coordinates
  xy <- cbind(spp$longitude, spp$latitude)
  sps <- as.numeric(factor(spp$species))
  
# number of records (n.rec)
  n.rec <- rasterize(xy, templ, fun = function(x,...) {length(na.omit(x)) })
# crop extra cells  
  n.rec.crop <- mask(n.rec, aus)
  plot(log(n.rec.crop))
  
# species richness (sr)
  sr <- rasterize(xy, templ, field = sps, fun = function(x,...) {length(unique(na.omit(x))) })  
  sr.crop <- mask(sr, aus)
  plot(sr.crop)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
  
  
  
  
  
  
  
  
    