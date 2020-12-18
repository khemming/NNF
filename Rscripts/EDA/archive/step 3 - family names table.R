#####################################################
# families and other taxa names data frame
#####################################################
  
# scope ---------------------------------------------
# call family names into loops efficiently, columns of:
# family name
# native raster name
# non-native raster name
  
# library -------------------------------------------
  library(tidyverse)
  library(raster)
  
  rm(list = ls())

# data ----------------------------------------------
# rasters
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/scaled richness")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  spp <- as.data.frame(c.stack, na.rm = F)
  glimpse(spp)
  
  setwd("C:/Users/s436862/Dropbox/Non-native-families")

# list of familiy/taxa names 
  family_list <- read.csv("Results/csv/summary statistics/family selection list.csv", stringsAsFactors = F) 
  family_list_rep <- rep(family_list$family, each = 2)
  
# data frame --------------------------------------------------------
  stat_rep <- as.numeric(as.character(nrow(family_list))) 
  
  dat <- data.frame(family_long = as.character(family_list_rep),
                    raster_name = as.character(names),
                    status = as.character(rep(c("native", "non-native"), as.numeric(as.character(nrow(family_list))))),
                    prefix = as.character(substr(family_list_rep, 1, 3)), stringsAsFactors = F)
  glimpse(dat)
 
  write.csv(dat, "Results/csv/summary statistics/selected family names table.csv", row.names = F)

# --------------------------------------------------------------