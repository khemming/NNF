

# library ------------------------------------------------
  library(raster)
  library(tidyverse)
  
  rm(list = ls())
  
# data ----------------------------------------------------------------   
# family names no status
  names22 <- read.csv("Results/csv/families and predictors/study families.csv", stringsAsFactors = F)[1:22, 1]
  glimpse(names22)
  
# observed nonnative
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/scaled")
  observed_rasters <- list.files(pattern = "_nonnative_observed.grd")
  observed_names <- gsub(pattern = ".grd$", "", observed_rasters)
  observed_stack <- stack(observed_rasters)
  names(observed_stack) <- observed_names
  
# observed native
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/scaled")
  obs_nat_rasters <- list.files(pattern = "_native_observed.grd")
  obs_nat_names <- gsub(pattern = ".grd$", "", obs_nat_rasters)
  obs_nat_stack <- stack(obs_nat_rasters)
  names(obs_nat_stack) <- obs_nat_names
  
# predicted native
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/predicted")
  predicted_rasters <- list.files(pattern = "_native_predicted.grd")
  predicted_names <- gsub(pattern = ".grd$", "", predicted_rasters)
  predicted_stack <- stack(predicted_rasters)
  names(predicted_stack) <- predicted_names
  
  setwd("C:/Users/s436862/Dropbox/NNF")
  
# potential relative richness ---------------------------------------------------    
# predicted native minus observed nonnative   
  for (i in 1:length(names22)){
   observed_raster <- observed_stack[[i]]
   predicted_raster <- predicted_stack[[i]]
  
 # calculate difference
   observed_raster[is.na(observed_raster[])] <- 0 
   poten_a <- predicted_raster - observed_raster
   poten_b <- calc(poten_a, fun = function(x) {x[x<0] <- 0; return(x)})
   
   save_name <- paste0("Results/rasters/potential/", names22[i], "_potential.grd")
   writeRaster(poten_b, filename = save_name, overwrite = T)
   
 }
  
# observed native minus observed nonnative  
  for (i in 1:length(names22)){
    observed_raster <- observed_stack[[i]]
    obs_nat_raster <- obs_nat_stack[[i]]
    
    # calculate difference
    observed_raster[is.na(observed_raster[])] <- 0 
    poten_a <- obs_nat_raster - observed_raster
    poten_b <- calc(poten_a, fun = function(x) {x[x<0] <- 0; return(x)})
    
    save_name <- paste0("Results/rasters/potential by observed native/", names22[i], "_obs_nat_potential.grd")
    writeRaster(poten_b, filename = save_name, overwrite = T)
    
  }
# -------------------------------------------------------------------
 
  
  