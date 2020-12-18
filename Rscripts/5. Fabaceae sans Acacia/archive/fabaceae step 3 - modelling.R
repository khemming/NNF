

# library ---------------------------------------------------------------------
  library(raster)
  library(MuMIn)
  library(ape)
  library(nlme)  
  library(sp)
  library(tidyverse)
 
  rm(list = ls()) 

# data -----------------------------------------------------------------------
# rasters to data frame
  fna <- raster("C:/Users/s436862/Dropbox/NNF/Results/Fabaceae sans Acacia/rasters/scaled/FNA_Native.grd")

  setwd("C:/Users/s436862/Dropbox/NNF")
  
  spp <- as.data.frame(fna) %>%
         mutate(cell_id = 1:length(fna))
  
# predictor variables
  pv <- read.csv("Results/csv/families and predictors/families predictors 1156.csv", header = T)
  
# merge
  fpv <- left_join(pv, spp, by = "cell_id")

# raster
  aus <- raster("Data files/Australia/Australia 1156.grd")
  
# cell ID
  cell_id <- pv %>% dplyr::select(cell_id)  
  
# identify spatial auto-correlation --------------------------------------------------------------
# store all results for supplementary materials
  moran_l <- list()
  gls_l <- list()
  model_list <- list()
  ci_list <- list()
  cor_m <- matrix(nrow = length(2))
  
# identify spatial autocorrelation function (returns p-value)
  moran_fun <- function(fam_col, col_no) {
    xy <- fpv %>% filter(!is.na(fam_col)) %>%
      dplyr::select(all_of(col_no), long, lat)
    coords = cbind(xy$long, xy$lat)
    w = fields:::rdist(coords)
    m_i <- Moran.I(x = xy[, 1], w = w, na.rm = T)
    return(m_i)
  }
  
# run 
  moran_l[[1]] <- moran_fun(fpv[, 1], 1)
  moran_l[[2]] <- moran_fun(fpv[, 2], 2)
  
  moran_l # native has spatial a/c but nonnative does not
  names(moran_l) <- names   
  
# model selection --------------------------------------------------------------------------------
# test different methods for modelling spatial autocorrelation, and choose best fit
  model_sel_fun <- function(fam_col) {
  # model methods to account for spatial autocorrelation
    model_e <- gls(fam_col ~ hii + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                   data = fpv, correlation = corExp(form = ~long + lat, nugget=T) , na.action = na.omit, method = "ML")
    model_g <- gls(fam_col ~ hii + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                   data = fpv, correlation = corGaus(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_s <- gls(fam_col ~ hii + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                   data = fpv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_r <- gls(fam_col ~ hii + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                   data = fpv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_lm <- lm(fam_col ~ hii + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                   data = fpv, na.action = na.omit)
  # compare models using AICc
    model_sel <- model.sel(model_e, model_g , model_s, model_r, model_lm)
    return(model_sel)
  } 
  
# run
  fam_col <- fpv$FNA_native
  gls_l[[1]] <- model_sel_fun(fam_col)
  cor_m[1] <- gls_l[[1]]$correlation[1] # best correlation structure
  
  fam_col <- fpv$FNA_nonnative
  gls_l[[2]] <- model_sel_fun(fam_col)
  cor_m[2] <- gls_l[[2]]$correlation[1] # best correlation structure
  
# check  
  cor_m

# store model output
  gls_m <- matrix(nrow = 10, ncol = 18)
  gls_m[1:5, ] <- as.matrix(gls_l[[1]])
  gls_m[6:10, ] <- as.matrix(gls_l[[2]])
  gls_m
  
# run best model
# native: Spher
  model_list[[1]] <- gls(FNA_native_observed ~ hii + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                                    data = fpv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  ci_list[[1]] <- data.frame(intervals(model_list[[1]], 0.95, which = "coef")$coef)

# nonnative: Spher
  model_list[[2]] <- gls(FNA_nonnative_observed ~ hii + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                                    data = fpv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  ci_list[[2]] <- data.frame(intervals(model_list[[2]], 0.95, which = "coef")$coef)
  
# save
  write.csv(moran_l, "Results/Fabaceae sans Acacia/csv/Morans I.csv", row.names = T)
  write.csv(gls_m, "Results/Fabaceae sans Acacia/csv/GLS model structures.csv", row.names = T)

# predicted richness -----------------------------------------------------  
  psr <- matrix(nrow = nrow(cell_id), ncol = 2)
  psr[, 1] <- predict(model_list[[1]], newdata = pv)
  psr[, 2] <- predict(model_list[[2]], newdata = pv)
    
# make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(aus)), ncol = 2)
  class(psr) <- "numeric"
  
# add the occupied cells
  x[cell_id$cell_id, ] <- psr
  
# save  
  pred_rasterise <- function(raster_df, raster_template, raster_names_df){
    
    save <- paste0("Results/Fabaceae sans Acacia/rasters/predicted/", raster_names_df, "_predicted.grd")
    
    xa <- setValues(raster_template, raster_df)
    xb <- calc(xa, fun = function(x) {x[x<0] <- 0; return(x)})
    xc <- calc(xb, fun = function(x) {x[x>1] <- 1; return(x)})
    writeRaster(xc, save, overwrite = T)
    
  }
  
# run  
  pred_rasterise(x[, 1], aus, names(FNA_native_observed))
  pred_rasterise(x[, 2], aus, names(FNA_native_observed))
    
# potential richness ----------------------------------------------------------  
# predicted native minus observed nonnative 
  observed_raster <- setValues(aus, x[, 1])
  plot(observed_raster)
  
  predicted_raster <- setValues(aus, x[, 2])
  plot(predicted_raster)
  # start here
  
  
# ------------------------------------------------------------------------------
