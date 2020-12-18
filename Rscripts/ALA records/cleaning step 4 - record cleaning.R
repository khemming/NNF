

# library --------------------------------------------------------
  library(raster)
  library(tidyverse)
  library(data.table)
  
  rm(list = ls())
  
# data --------------------------------------------------------------------
# read in RDS files
  dat <- readRDS("Data files/ALA/cleaning steps/step 2 - raw record file.RDS")
  
# useable names
  col_names <- c("id",
                 "species",
                 "year",
                 "latitude",
                 "longitude",
                 "coordinate_uncertainty",
                 "taxon_rank",
                 "family",
                 "genus",
                 "collector")
  colnames(dat) <- col_names
  
# Australia coordinates 
  aus <- raster("Data files/Australia/Australia 1156.grd")
  
# cleaning steps function ------------------------------------------------
  rec_clean <- function(dat){
    
  # filter records for Australian location
    xy <- dat %>% 
          dplyr::select(longitude, latitude)
          cell_id <- raster::extract(aus, xy)
    dat$aus_cell <- ifelse(!is.na(cell_id), "yes", "no")
    dat2 <- dat %>% 
            filter(aus_cell == "yes") %>%
            dplyr::select(-aus_cell)
    
  # remove records missing relevant fields:
    dat3 <- dat2 %>% 
            tidyr::drop_na("species", "genus") %>%
            filter(grepl(" ", species)) %>% 
            filter(!genus == "") 
          
  # remove horticultural taxa  
    table(dat3$taxon_rank, exclude = NULL) # subspecies will be treated as species
    dat4 <- dat3 %>% 
            filter(taxon_rank == "species" | taxon_rank == "subspecies")
    dat4$taxon_rank <- if_else(dat4$taxon_rank == "subspecies", "species", dat4$taxon_rank)
  # check  
    table(dat4$taxon_rank, exclude = NULL)
    
  # remove incorrect year (NA or 0)
    table(dat4$year, exclude = NULL)
    dat5 <- dat4 %>% 
            tidyr::drop_na("year") %>%
            filter(year != "0")
    table(dat5$year, exclude = NULL)
  
  # uncertain (NA) and with large uncertainties (>10 km radius) 
    dat6 <- dat5 %>% 
            filter(coordinate_uncertainty <= 10000 & !coordinate_uncertainty <= 0)
    table(dat6$coordinate_uncertainty, exclude = F)
    
  # remove duplicates: rounded lat/longs to ~1-km (2dp)
    dat6$latitude <- round(dat6$latitude, digits = 2)
    dat6$longitude <- round(dat6$longitude, digits = 2)
  # find unique (distinct) records 
    dat7 <- dat6 %>% 
            distinct(species, year, latitude, longitude, .keep_all = TRUE) %>%
            droplevels()
    
  return(dat7)
  
  } # function end
  
# run function  ---------------------------------------------------
  dat7 <- rec_clean(dat)
  
# retained species & records 
  spp.list <- levels(as.factor(dat7$species))
  length(spp.list)
  spp.list.df <- data.frame(spp.list)
  
  table(dat7$family, exclude = NULL)
  
# save
  saveRDS(dat7, "Data files/ALA/cleaning steps/step 4 - filtered records.RDS")
# -----------------------------------------------------------------
  
  
  
  