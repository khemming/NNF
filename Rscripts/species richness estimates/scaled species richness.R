#######################################################################
# observed proportional species richness
#######################################################################

# scope --------------------------------------------------------------
# re-do iNEXT species richness to relative proportions

# library -------------------------------------------------------------
  library(raster)
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------
# rasters
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/iNEXT")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/Non-native-families")

# proportional species richness -----------------------------------------------    
# function
# requires: raster -- loaded from above
#           raster.name -- identity for file location, so maybe put in if its observed or predicted, and in quotes
  prop_sr <- function(raster, raster.name) {
  
    prop <- raster/cellStats(raster, stat = 'max', na.rm = T)
  
    rasterfile <- paste0("Results/rasters/scaled richness/", raster.name, ".grd")
  
    writeRaster(prop, rasterfile, overwrite = T)
    
    return(plot(prop))
  
  }
  
  
# loop for all groups ----------------------------------------------------------
  for (i in 1:length(names)) {
    
    prop_sr(c.stack[[i]], names[i])
    
  }
  
# ------------------------------------------------------------------------------
 
