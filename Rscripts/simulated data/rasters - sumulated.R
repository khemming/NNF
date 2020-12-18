


# library --------------------------------------------------------------
  library(raster)
  library(rgdal)
  library(tidyverse)

  rm(list = ls())

# data -----------------------------------------------------------------
# creating raster data 
# large scale Australia 
  shp <- readOGR("Data files/Australia/Australia shapefile.shp")
  aus <- raster("Data files/Australia/Australia 1 km.grd")
  aus_200 <- aggregate(aus, fact = 200, fun = mean)
  plot(aus_200)
  plot(shp, add = T)
  
# well-predicted native -----------------------------------------  
# simulating a north-south species richness gradient across Australia
# use this for both native and nonnative distributions
  nat1 <- setValues(aus_200, 1:ncell(aus_200))
  plot(nat1)
  nat2 <- mask(nat1, aus_200)
  plot(nat2)
  
# use longitude to assign cell values
  xy <- data.frame(xyFromCell(nat2, 1:length(nat2)))
  head(xy)
  xy$z <- round(rev(xy$y), 2)
  nat3 <- setValues(nat2, xy$z)
  nat4 <- mask(nat3, aus_200)
  nat5 <- nat4/min(xy$z, na.rm = T)
  plot(nat5)
  
# add noise
  sq <- seq(-0.05, 0.05, 0.01)
  nat6 <- sample(sq, length(getValues(nat5)), replace = T)
  nat7 <- nat5 - nat6
  nat8 <- nat7/cellStats(nat7, stat = "max") # scale back to 0 - 1
  plot(nat8)
  nat9 <- nat8
  
# well predicted nonnative ------------------------------------------
  nonnat1 <- sample(sq, length(getValues(nat5)), replace = T)
  nonnat2 <- nat5 - nonnat1
  nonnat3 <- nonnat2/cellStats(nonnat2, stat = "max") # scale back to 0 - 1
  plot(nonnat3)
 
# minor spread potential ------------------------------------------------------------------
  nat8[is.na(nat8[])] <- 0 
  sp1 <- nat8 - nonnat3
  sp2 <- calc(sp1, fun = function(x) {x[x<0] <- 0; return(x)})
  sp3 <- mask(sp2, aus_200)
  plot(sp3)
 
# incomplete nonnative distribution -----------------------------------------------------
# two steps: first, make a "northern" distribution; second, make it sneak down the East coast
# data 
  nonnat4 <- data.frame(getValues(nonnat3))
  nonnat4$cell_id <- 1:length(nonnat3)
  colnames(nonnat4) <- c("value", "cell_id")
  head(nonnat4)
 
# plot Aus cells with/without values in window  
# set Console window to display 27 cols  [622 bottom left hand corner]--------------------------------------------------
  window1 <- setValues(aus_200, 1:length(aus_200))
  window2 <- mask(window1, aus_200)
  getValues(window2)
  
# north    
  nrth <- calc(nonnat3, fun = function(x) {x[x<0.68] <- NA; return(x)})
  plot(nrth)
  nrth_v <- data.frame(getValues(nrth))
  nrth_v$cell_id <- 1:length(nrth_v)
  colnames(nrth_v) <- c("value", "cell_id")
  head(nrth_v)
  
  north_ras <- mask(window1, nrth)
  nv <- getValues(north_ras)
  
# east
  east <- c(230, 235:236, 238, 240, 
            262, 264, 266:268,
            287, 289, 293:296, 
            319:320, 322:323, 
            348:350, 
            373, 375:377, 
            401:404,
            429:431,
            456:457,
            482:483,
            510)
  
# combine 
  nn_incomp <- nonnat3
  nn_incomp[ c(nv,
               east)] <- NA
 
  nn_incomp2 <- mask(nonnat3, nn_incomp, inverse = F)
  nn_incomp4 <- mask(nn_incomp2, aus_200)
  plot(nn_incomp4)
  
# dispersal limiation
  dl <- data.frame(cell_val = getValues(nn_incomp4),
                   cell_id  = 1:length(nn_incomp4))
  head(dl)
  set.seed(565)
  j <- sample(1:length(dl$cell_val), 400, replace = F)
  dl$cell_val2 <- dl$cell_val
  dl$cell_val2[j] <- dl$cell_val2[j]*0.8
  nonnat5 <- setValues(aus_200, dl$cell_val2)
  plot(nonnat5)
  nonnat6 <- nonnat5
 
# substantial spread potential -------------------------------------------
  nonnat5[is.na(nonnat5[])] <- 0 
  sp4 <- nat8 - nonnat5
  sp5 <- calc(sp4, fun = function(x) {x[x<0] <- 0; return(x)})
  sp6 <- mask(sp5, aus_200)
  plot(sp6)

# non-correlated nonnative ----------------------------------------------
  nc_nn <- nn_incomp4
  nc_nn_v <- getValues(nc_nn)
  
  nc_seq <- rnorm(length(getValues(nc_nn)), 0.5, 0.2)
  nc_sam <- sample(nc_seq, length(getValues(nc_nn)), replace = T)
  
  nc_nn_v2 <- setValues(nc_nn, nc_seq)
  nc_nn_v3 <- mask(nc_nn_v2, nc_nn)
  plot(nc_nn_v3)
  
# save rasters -----------------------------------------------
  writeRaster(nat9, "Results/simulated results/native complete.grd", overwrite = T)
  writeRaster(nonnat3, "Results/simulated results/nonnative complete.grd", overwrite = T)
  writeRaster(sp3, "Results/simulated results/minor invasion potential.grd", overwrite = T)
  writeRaster(nonnat6, "Results/simulated results/nonnative incomplete.grd", overwrite = T)
  writeRaster(sp6, "Results/simulated results/major invasion potential.grd", overwrite = T)
  writeRaster(nc_nn_v3, "Results/simulated results/nonnative no correlation.grd", overwrite = T)
  
# --------------------------------------------------------------- 
  