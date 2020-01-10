#######################################################################
# potential scaled species richness
#######################################################################

# scope --------------------------------------------------------------
# take away observed non-native richness from predicted native richness 

# library -------------------------------------------------------------
  library(raster)
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------
# richness
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/scaled richness")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/scaled richness")
  
# potential relative richness ---------------------------------------------------    
# function
# requires: predicted raster -- native predicted
#           observed raster -- non-native observed
#           raster.name -- identity for file location, in quotes
  rr.pot <- function(pred.raster, obs.raster, save.name) {
    obs.raster[is.na(obs.raster[])] <- 0 
    rr.pot1 <- pred.raster - obs.raster
    rr.pot2 <- calc(rr.pot1, fun = function(x) {x[x<0] <- 0; return(x)})
    
    rasterfile <- paste0(save.name, ".grd")
  
  writeRaster(rr.pot2, filename = rasterfile, overwrite = T)
  
  return(plot(rr.pot2))
  
  }
  
# function requires:
#                   predicted native raster
#                   observed non-native raster
#                   save file name (no extension)
# Asteraceae 
  rr.pot(predicted.Asteraceae.native, 
         Asteraceae.non.native, 
         "potential.Asteraceae.non.native")
  
# Brassicaceae 
  rr.pot(predicted.Brassicaceae.native, 
         Brassicaceae.non.native, 
         "potential.Brassicaceae.non.native")
  
# Cyperaceae 
  rr.pot(predicted.Cyperaceae.native, 
         Cyperaceae.non.native, 
         "potential.Cyperaceae.non.native")
  
# Fabaceae 
  rr.pot(predicted.Fabaceae.native, 
         Fabaceae.non.native, 
         "potential.Fabaceae.non.native")
  
# Malvaceae 
  rr.pot(predicted.Malvaceae.native, 
         Malvaceae.non.native, 
         "potential.Malvaceae.non.native")
  
# Myrtaceae 
  rr.pot(predicted.Myrtaceae.native, 
         Myrtaceae.non.native, 
         "potential.Myrtaceae.non.native")
  
# Plantae 
  rr.pot(predicted.Plantae.native, 
         Plantae.non.native, 
         "potential.Plantae.non.native")
  
# Proteaceae 
  # rr.pot(predicted.Asteraceae.native, 
  #        Asteraceae.non.native, 
  #        "potential.Asteraceae.non.native")

# Solanaceae 
  rr.pot(predicted.Solanaceae.native, 
         Solanaceae.non.native, 
         "potential.Solanaceae.non.native")


# -------------------------------------------------------------------
 
  
  