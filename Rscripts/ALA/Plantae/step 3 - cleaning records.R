########################################################
# cleaning step 2: cleaning missing ALA data
########################################################

# scope ------------------------------------------------------------
# remove records missing: 
# species, year, location data, and duplicates etc.
# remove hybrids and varieties (cultivars)
  
# library --------------------------------------------------------
  library(tidyverse)
  library(data.table)

  rm(list = ls())

# data --------------------------------------------------------------------
# read in RDS files
  dat <- readRDS("Data files/ALA/kingdom data/Plantae raw records.RDS")

# useable names
  col.names <- c("id",
                 "species",
                 "year",
                 "latitude",
                 "longitude",
                 "coordinate.uncertainty",
                 "taxon.rank",
                 "family",
                 "genus",
                 "collector")
  colnames(dat) <- col.names
  
# cleaning function ---------------------------------------------------
# notes ---------------------------------------------------------------
# too big to do in steps
# therefore, I will be iteratively checking mistakes by running it and checking with tables and csv outputs of final file
# ---------------------------------------------------------------------
  let_them_be_clensed <- function(dat){

# remove records missing relevant fields
# NA values
  na <- dat %>% tidyr::drop_na("species", "genus", "latitude", "longitude", "year") %>%
                droplevels()
  
# family, genera and species 
  miss1 <- filter(na, grepl(" ", species))
  miss2 <- filter(miss1, !species == "")
  miss3 <- filter(miss2, !genus == "")
  miss4 <- filter(miss3, !genus == "")
  
# records which are described as forms, varieites & cultivars (i.e. horticultural taxa)   
  tax <- miss4
  
  rm(list=setdiff(ls(),"tax"))
    
  tax1 <- filter(tax, !grepl("form", taxon.rank))
  tax2 <- filter(tax1, !grepl("variety", taxon.rank))
  tax3 <- filter(tax2, !grepl("cultivar", taxon.rank))
  tax4 <- filter(tax3, !grepl("genus", taxon.rank))
  tax5 <- filter(tax4, !grepl("unranked", taxon.rank))
  tax6 <- filter(tax5, !grepl("section botany", taxon.rank))
  
# remove incorrect year (where year = 0)
  year <- tax3
  table(year$year, exclude = F)
  
  year1 <- filter(year, !year == "0")
  
# remove records with coordiantes that are incorrect, uncertain (NA) and with large uncertainties (above 10 km radius)
  coord <- year1
  
  coord$coordinate.uncertainty <- as.numeric(coord$coordinate.uncertainty)
  coord1 <- filter(coord, coordinate.uncertainty <= 10000 & !coordinate.uncertainty <= 0)
  table(coord1$coordinate.uncertainty)
  
# remove duplicates: rounded lat/longs to ~1-km (2dp)
  dup <- coord1
  
  dup$latitude <- as.numeric(dup$latitude)
  dup$longitude <- as.numeric(dup$longitude)
  
  dup$latitude <- round(dup$latitude, digits = 2)
  dup$longitude <- round(dup$longitude, digits = 2)

# find unique (distinct) records 
  dup2 <- dup %>% distinct(species, year, latitude, longitude, .keep_all = TRUE) %>%
                      droplevels()
  
  
# retained species & records
  dat.final <- dup2
  
# write a whole bunch of csvs to check out descrepencies
# spp, year, genus, family, latitude, longitude, coordinate.uncertainty, taxon.rank,
  dat.final <- dup2
  
# write a whole bunch of csvs to check out descrepencies
# spp, year, genus, family, latitude, longitude, coordinate.uncertainty, taxon.rank,
  spp <- table(dat.final$species, exclude = NULL) 
  write.csv(spp, "Data files/ALA/kingdom data/spp.csv", row.names = F)
  
  year <- table(dat.final$year, exclude = NULL)
  write.csv(year, "Data files/ALA/kingdom data/year.csv", row.names = F)
  
  genus <- table(dat.final$genus, exclude = NULL)
  write.csv(genus, "Data files/ALA/kingdom data/genus.csv", row.names = F)
  
  coordinate.uncertainty <- table(dat.final$coordinate.uncertainty, exclude = NULL)
  write.csv(coordinate.uncertainty, "Data files/ALA/kingdom data/coordinate.uncertainty.csv", row.names = F)
  
  taxon.rank <- table(dat.final$taxon.rank, exclude = NULL)
  write.csv(taxon.rank, "Data files/ALA/kingdom data/taxon.rank.csv", row.names = F)
  
  family <- table(dat.final$family, exclude = NULL)
  write.csv(family, "Data files/ALA/kingdom data/family.csv", row.names = F)
  
  return(dat.final)
  }
# ---------------------------------------------------------------
  
# checking steps ------------------------------------------------
# run function
  dat2 <- let_them_be_clensed(dat)
  
# issues so far: 
# hybrids and weird names
  
  
  
  spp.list <- levels(as.factor(dat.final$species))
  length(spp.list)
  spp.list.df <- data.frame(spp.list) # 8740

  k <- table(dat.final$family)
  write.csv(k, "Data files/ALA/all plant records/k.csv")
# save
  saveRDS(dat.final, "Data files/ALA/cleaning step 3 - cleaned records.RDS")

