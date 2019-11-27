########################################################
# cleaning step 2: cleaning missing ALA data
########################################################

# aim ------------------------------------------------------------
# remove records missing: 
# species, year, location data, and duplicates etc.
# remove hybrids and varieties (cultivars)
  
# library --------------------------------------------------------
  library(tidyverse)
  library(data.table)

# data --------------------------------------------------------------------
# read in RDS files
  ast <- readRDS("Data files/ALA/Asteraceae raw records.RDS")
  bra <- readRDS("Data files/ALA/Brassicaceae raw records.RDS")
  cyp <- readRDS("Data files/ALA/Cyperaceae raw records.RDS")
  fab <- readRDS("Data files/ALA/Fabaceae raw records.RDS")
  mal <- readRDS("Data files/ALA/Malvaceae raw records.RDS")
  myr <- readRDS("Data files/ALA/Myrtaceae raw records.RDS")
  pro <- readRDS("Data files/ALA/Proteaceae raw records.RDS")
  sol <- readRDS("Data files/ALA/Solanaceae raw records.RDS")
  
# bind
# note: to do: redownload plant families without extra cols
  dat <- rbind(ast, bra,fab, myr, pro, sol)
  dat <- dat %>% select(-taxonomicStatus, -scientificNameWithoutAuthor)
  dat <- rbind(dat, mal, cyp)
  col.names <- c("year",
                 "island",
                 "country",
                 "state",
                 "latitude",
                 "longitude",
                 "coordinate.uncertainty",
                 "taxon.rank",
                 "family",
                 "genus",
                 "species",
                 "id",
                 "collector")
  
  colnames(dat) <- col.names
  
  
# cleaning steps ------------------------------------------------------  
# remove records missing relevant fields
# NA values
  dat2 <- dat %>% tidyr::drop_na("species", "genus", "latitude", "longitude", "year")
  
# genera and species 
  dat2a <- filter(dat2, grepl(" ", species))
  dat2b <- filter(dat2a, !genus == "")
  
# records which are described as forms, varieites & cultivars (i.e. horticultural taxa)   
  dat2c <- filter(dat2b, !grepl("form", taxon.rank))
  dat2d <- filter(dat2c, !grepl("variety", taxon.rank))
  dat2e <- filter(dat2d, !grepl("cultivar", taxon.rank))
 
# remove incorrect year (where year = 0)
  table(dat2e$year, exclude = F)
  dat2e <- filter(dat2d, !year == "0")
  
# remove records with coordiantes that are incorrect, uncertain (NA) and with large uncertainties (above 10 km radius)
  dat3 <- dat2e
  dat3a <- filter(dat3, coordinate.uncertainty <= 10000 & !coordinate.uncertainty <= 0)
  table(dat3a$coordinate.uncertainty)
  
# remove duplicates: rounded lat/longs to ~1-km (2dp)
  dat3a$latitude <- round(dat3a$latitude, digits = 2)
  dat3a$longitude <- round(dat3a$longitude, digits = 2)

# find unique (distinct) records 
  dat3b <- dat3a %>% distinct(species, year, latitude, longitude, .keep_all = TRUE) 
  
# retained species & records ------------------------------------
  spp.list <- levels(as.factor(dat3b$species))
  length(spp.list)
  spp.list.df <- data.frame(spp.list) # 8740

  table(dat3b$family)
  
# save
  saveRDS(dat3b, "Data files/ALA/cleaning step 3 - cleaned records.RDS")

