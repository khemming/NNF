

# library ---------------------------------------------------------------------
  library(raster)
  library(tidyverse)
 
  rm(list = ls()) 

# data -----------------------------------------------------------------------
# environmental data
# retained from varaible selection process
  pv_vs <- c("cell_id", "cell_category", "proportion_cover", "lat", "long", "amt", "arid", "ts", "pwarmq", "pcoldq", "th", "hii")
  
  pv <- read.csv("Results/csv/families and predictors/predictor variables 2538.csv", header = T) %>%
    dplyr::select_at(pv_vs)
  
# rasters
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/scaled")
  current.list <- list.files(pattern = "_observed.grd")
  raster_names <- gsub(pattern = "_observed.grd$", "", current.list)
  raster_stack <- stack(current.list)
  names(raster_stack) <- raster_names
  list2env(setNames(unstack(raster_stack), names(raster_stack)), .GlobalEnv)
  fam <- as.data.frame(raster_stack, na.rm = F)
  glimpse(fam)
  
  setwd("C:/Users/s436862/Dropbox/NNF")
  
# bind
  fam_pv <- cbind(fam, pv)
  fam_pv2 <- fam_pv %>% filter(cell_category == "land", # clears most NA cells
                               !is.na(hii))             # clears a few cells with no data
  
  fam2 <- fam_pv2 %>% select_at(1:ncol(fam))
  
  write.csv(fam_pv2, "Results/csv/families and predictors/families predictors 1156.csv", row.names = F)
  write.csv(fam2, "Results/csv/families and predictors/families 1156.csv", row.names = F)

# ------------------------------------------------------------------------------
