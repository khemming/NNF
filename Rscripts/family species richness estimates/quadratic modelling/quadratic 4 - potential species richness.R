

# library ------------------------------------------------
  library(raster)
  library(tidyverse)
  
  rm(list = ls())
  
# data ----------------------------------------------------------------   
# family names no status
  names22 <- read.csv("Results/csv/summary statistics/study families.csv", stringsAsFactors = F)[1:22, 1]
  glimpse(names22)
  
# observed nonnative
  setwd("C:/Users/Hemming/Dropbox/NNF/Results/rasters/scaled")
  observed_rasters <- list.files(pattern = "_nonnative_observed.grd")
  observed_names <- gsub(pattern = ".grd$", "", observed_rasters)
  observed_stack <- stack(observed_rasters)
  names(observed_stack) <- observed_names
  
# predicted native
  setwd("C:/Users/Hemming/Dropbox/NNF/Results/rasters/quadratic rasters")
  predicted_rasters <- list.files(pattern = "_native_predicted.grd")
  predicted_names <- gsub(pattern = ".grd$", "", predicted_rasters)
  predicted_stack <- stack(predicted_rasters)
  names(predicted_stack) <- predicted_names
  
  setwd("C:/Users/Hemming/Dropbox/NNF")
  
# potential relative richness ---------------------------------------------------    
  for (i in 1:length(names22)){
   observed_raster <- observed_stack[[i]]
   predicted_raster <- predicted_stack[[i]]
  
 # calculate difference
   observed_raster[is.na(observed_raster[])] <- 0 
   poten_a <- predicted_raster - observed_raster
   poten_b <- calc(poten_a, fun = function(x) {x[x<0] <- 0; return(x)})
   
   save_name <- paste0("Results/rasters/quadratic rasters/", names[i], "_potential.grd")
   writeRaster(poten_b, filename = save_name, overwrite = T)
   
 }
  
# -------------------------------------------------------------------
 
  
  