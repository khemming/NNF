############################################################
# Australia raster at 1 and 100 km scales
############################################################

# scope ----------------------------------------------------
# have the rasters and data frames that account fo roceanic and land cells
# one of the environmental variables, PEWC, doesn't have values for all cells, and I have decided to remove theese from the Poaceae chapter study
# to this end, I am making two versions of the 100 km rasters and data frames:
# the first is 'complete', which has all 1133 cells
# the second has PEWC-corrected cells, which can subset all land-cells based on what PEWC hsa to offer, which is 1102 cells, I believe

# library --------------------------------------------------
  library(raster)
  library(rgdal)
  library(tidyverse)
  
  rm(list = ls())

# data -----------------------------------------------------  
  aus <- raster("Data files/Australia/Australia 1 km.grd")
  aus.shp <- readOGR("Data files/Australia/australia_shapefile.shp")

# 100 km --------------------------------------------------------------------
# the 'complete' 100 km raster template
  plot(aus)
  aus.100 <- aggregate(aus, fact = 100, fun = mean)
  plot(aus.100)
  plot(aus.shp, add = T)
  
  aus.100.template <- setValues(aus.100, 1:ncell(aus.100))
  plot(aus.100.template)
  aus.100.masked <- mask(aus.100.template, aus.100)
  plot(aus.100.masked)
  
  writeRaster(aus.100.masked, "Data files/Australia/Australia 100 km complete.grd", overwrite = T)
  
# data frame -----------------------------------------------------------------
# proportion of cell covered by shapefile
  prop.cover <- rasterize(aus.shp, aus.100, getCover = T)
  prop.cover <- data.frame(getValues(prop.cover))
  prop.cover <- prop.cover/100
  colnames(prop.cover) <- "prop.cover"
  
# cell_id
  cell.id <- 1:length(aus.100)
  
# all (non-PEWC) cell category: ocean or land
  cell.category.all <- ifelse(prop.cover > 0, "land", "ocean")
  colnames(cell.category.all) <- "cell.category.complete"
  
# pewc (PEWC) cell category
  pewc <- read.csv("Results/EVs/CSV/100 km EFs scaled.csv") %>%
    dplyr::select(pewc)
  cell.category.pewc <- ifelse(is.na(pewc) == T, "ocean", "land")
  colnames(cell.category.pewc) <- "cell.category.pewc"
  
  cells <- cbind(cell.id, prop.cover, cell.category.all, cell.category.pewc)
  head(cells)  
  
# save  
  write.csv(cells, "Data files/Australia/Australia 100 km complete.csv", row.names = F)
  
# Australia 100 km PEWC-corrected ---------------------------------------------
  pewc.ras <- raster("Results/EVs/Rasters/100 km/pewc.grd")
  plot(pewc.ras)
  aus.100.template <- setValues(aus.100, 1:ncell(aus.100))
  plot(aus.100.template)
  aus.100.masked <- mask(aus.100.template, aus.100)
  plot(aus.100.masked)
  pewc.aus <- mask(aus.100.masked, pewc.ras)
  plot(pewc.aus)
  writeRaster(pewc.aus, "Data files/Australia/Australia 100 km PEWC.grd", overwrite = T)
  
# ----------------------------------------------------------------------