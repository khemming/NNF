

# library ------------------------------------------------
  library(raster)
  library(tidyverse)
  
  rm(list = ls())

# data ----------------------------------------------------------------   
# model data
  

# raster (100 km scale)
  aus <- raster("Data files/Australia/Australia 1156.grd")
  
# family names by status
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/iNEXT")
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = ".grd$", "", current.list)
  setwd("C:/Users/s436862/Dropbox/NNF")

# continental-scale cells to predict on to
  pv <- read.csv("Results/csv/families and predictors/families predictors 1156.csv")

# cell ID
  cell_id <- pv %>% dplyr::select(cell_id)  
  
# predicting species richness ------------------------------------------------
  pred_sr <- matrix(nrow = nrow(pv),
                    ncol = length(names))
  
  for (i in 1:length(names)) {
    
    pred_sr[, i] <- predict(model_list[[i]], newdata = pv)
    
  }

# check
  head(pred_sr)
  
# rasterise predicted values --------------------------------------------------
# turn 1003 cells to 2538 in correct order
# generate list of occupied cells
  pd <- data.frame(pred_sr)
  
# make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(aus)), ncol = length(names))
  class(pred_sr) <- "numeric"
  
# add the occupied cells
  x[cell_id$cell_id, ] <- pred_sr
  
# produce rasters -------------------------------------------------------
# function
  pred_rasterise <- function(raster_df, raster_template, raster_names_df){
    
    save <- paste0("Results/rasters/predicted/", raster_names_df, "_predicted.grd")
    
    xa <- setValues(raster_template, raster_df)
    xb <- calc(xa, fun = function(x) {x[x<0] <- 0; return(x)})
    xc <- calc(xb, fun = function(x) {x[x>1] <- 1; return(x)})
    writeRaster(xc, save, overwrite = T)
    
  }
  
# loop
  for (i in 1:length(names)){
    
    pred_rasterise(x[, i],
                   aus,
                   names[i])
    
  }
  
# ------------------------------------------------------------