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
  family_list <- read.csv("Results/csv/summary statistics/family list master.csv") %>% 
              dplyr::select(family)
  family_list_long <- rep(fam.list, each = 2)
  
# data frame --------------------------------------------------------
  dat <- data.frame(family = family.list,
                    raster.name = names,
                    status = rep(c("native", "non-native"), 17),
                    prefix = substr(family.list, 1, 3))
  
  dat$family <- as.character(dat$family)
  dat$raster.name <- as.character(dat$raster.name)
  dat$status <- as.character(dat$status)
  dat$prefix <- as.character(dat$prefix)
  
  glimpse(dat)
 
  write.csv(dat, "Results/csv/summary statistics/family names master.csv", row.names = F)

# --------------------------------------------------------------