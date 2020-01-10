##########################################################
# predicted native richness
##########################################################

# scope --------------------------------------------------
# predict richness using relationships to environmental variables

# library ------------------------------------------------
  library(tidyverse)
  library(raster)
  rm(list = ls())

# data ----------------------------------------------------------------   
# model data
  load("Data files/Rdata/model_coefficients.RData")

# raster (100-km scale)
  aus <- raster("Data files/Australia/Australia 100 km v.all.grd")

# continental-scale cells to predict on to
  pred_ev <- evs %>% filter(cell.category.all == "land") %>%
                     dplyr::select(vs_factors)

    head(pred.ev)
    nrow(pred.ev)

# predicting distributions ------------------------------------------------

    
    
# Asteraceae
  Ast.nat.pred <- predict(m1, newdata = pred.ev)
  
# Brassicaceae
  Bra.nat.pred <- predict(m3, newdata = pred.ev)
  
# Cyperaceae
  Cyp.nat.pred <- predict(m5, newdata = pred.ev)
  
# Fabaceae
  Fab.nat.pred <- predict(m7, newdata = pred.ev)
  
# Malvaceae
  Mal.nat.pred <- predict(m9, newdata = pred.ev)
  
# Myrtaceae
  Myr.nat.pred <- predict(m11, newdata = pred.ev)
 
# Plantae
  Pla.nat.pred <- predict(m13, newdata = pred.ev)
  
# Proteaceae
  Pro.nat.pred <- predict(m15, newdata = pred.ev)

# Solanaceae
  Sol.nat.pred <- predict(m16, newdata = pred.ev)
  
# rasterise predicted values --------------------------------------------------
# turn 1003 cells to 2538 in correct order ------------------------------------
# cell ID
  cell.id <- evs %>%
    filter(cell.category.v1 == "land") %>%
    dplyr::select(cell.id)
  
# bind predicted distributions and add cell id
  pred.distr <- cbind(cell.id, 
                      Ast.nat.pred,
                      Bra.nat.pred,
                      Cyp.nat.pred, 
                      Fab.nat.pred, 
                      Mal.nat.pred, 
                      Myr.nat.pred, 
                      Pla.nat.pred, 
                      Pro.nat.pred,   
                      Sol.nat.pred)
  
  pred.distr <- data.frame(pred.distr)
  
# generate list of occupied cells
  cell.list <- pred.distr$cell.id
  
# make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(aus)), ncol = 9) # pred.distr - 1
  
  pred.distr.m <- as.matrix(pred.distr)
  pred.distr.mm <- pred.distr.m[, 2:10]
  class(pred.distr.mm) <- "numeric"
  
# add the occupied cells
  x[cell.list, ] <- pred.distr.mm
  
# producing rasters -------------------------------------------------------
# Asteraceae
  p1a <- setValues(aus, x[, 1])
  p1 <- calc(p1a, fun = function(x) {x[x<0] <- 0; return(x)})
  writeRaster(p1, "Results/rasters/scaled richness/predicted.Asteraceae.native.grd", overwrite = T)
  plot(p1)
  
# Brassicaceae
  p1a <- setValues(aus, x[, 2])
  p1 <- calc(p1a, fun = function(x) {x[x<0] <- 0; return(x)})
  writeRaster(p1, "Results/rasters/scaled richness/predicted.Brassicaceae.native.grd", overwrite = T)
  plot(p1)
  
# Cyperaceae
  p1a <- setValues(aus, x[, 3])
  p1 <- calc(p1a, fun = function(x) {x[x<0] <- 0; return(x)})
  writeRaster(p1, "Results/rasters/scaled richness/predicted.Cyperaceae.native.grd", overwrite = T)
  plot(p1)
  
# Fabaceae
  p1a <- setValues(aus, x[, 4])
  p1 <- calc(p1a, fun = function(x) {x[x<0] <- 0; return(x)})
  writeRaster(p1, "Results/rasters/scaled richness/predicted.Fabaceae.native.grd", overwrite = T)
  plot(p1)
  
# Malvaceae
  p1a <- setValues(aus, x[, 5])
  p1 <- calc(p1a, fun = function(x) {x[x<0] <- 0; return(x)})
  writeRaster(p1, "Results/rasters/scaled richness/predicted.Malvaceae.native.grd", overwrite = T)
  plot(p1)
  
# Myrtaceae
  p1a <- setValues(aus, x[, 6])
  p1 <- calc(p1a, fun = function(x) {x[x<0] <- 0; return(x)})
  writeRaster(p1, "Results/rasters/scaled richness/predicted.Myrtaceae.native.grd", overwrite = T)
  plot(p1)
  
# Plantae
  p1a <- setValues(aus, x[, 7])
  p1 <- calc(p1a, fun = function(x) {x[x<0] <- 0; return(x)})
  writeRaster(p1, "Results/rasters/scaled richness/predicted.Plantae.native.grd", overwrite = T)
  plot(p1)
  
# Proteaceae
  p1a <- setValues(aus, x[, 8])
  p1 <- calc(p1a, fun = function(x) {x[x<0] <- 0; return(x)})
  writeRaster(p1, "Results/rasters/scaled richness/predicted.Proteaceae.native.grd", overwrite = T)
  plot(p1)
  
# Solanaceae
  p1a <- setValues(aus, x[, 9])
  p1 <- calc(p1a, fun = function(x) {x[x<0] <- 0; return(x)})
  writeRaster(p1, "Results/rasters/scaled richness/predicted.Solanaceae.native.grd", overwrite = T)
  plot(p1)
  
# ------------------------------------------------------------