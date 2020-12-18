

# library -------------------------------------------------------------------------
  library(raster)
  library(rgdal)
  library(iNEXT)
  library(MuMIn)
  library(ape)
  library(nlme)  
  library(sp)
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------------------
# australia 
  raster <- raster("Data files/Australia/Australia 1156.grd")
  
# Fabaceae with no Acacia (fna)  
  dat <- readRDS("Data files/ALA/master data/Fabaceae no acacia.RDS")
  
# predictor variables
  pv <- read.csv("Results/csv/families and predictors/families predictors 1156.csv", header = T)
  
# cell ID
  cell_id <- pv %>% dplyr::select(cell_id)  

# iNEXT function ------------------------------------------------------------------
# notes ---------------------------------------------------------------------------
# interpolates (rarefaction) and extrapolates (Chao1) species richness to defined sampling coverage (we use 0.8)
# specify minimum number of records, which is used as the size to rarify to in the size rarefaction (default = 15)
# specifiy coverage to rarify to (default = 0.8)
# ---------------------------------------------------------------------------------
  inext <- function(dat, min.rec = 15, coverage = 0.8) {
  
# assign each point in the dataframe to raster cell
  xy <- cbind(dat$longitude, dat$latitude)
  dat$cell <- raster::extract(raster, xy)
  
# number of records per cell
  nr <- dat %>%
    group_by(cell) %>%
    summarise(n.rec = n()) %>%
    filter(!is.na(cell))
  
  dat2 <- full_join(dat, nr, by = "cell")
  
# filter by min.rec and extract number of records of each species in each cell
  cr <- dat2 %>%
        ungroup() %>%
        filter(n.rec >= min.rec) %>%
        mutate(species = factor(species)) %>%
        group_by(species, cell) %>%
        summarise(n = n()) 
  
# get a list of the occupied cell numbers
  cell.list <- as.numeric(as.character(levels(factor(cr$cell))))
  
# store coverage output
  out_cov <- numeric()
# store size output
  out_size <- numeric()
# check for warning
  out_warn <- numeric()
  
# do the rarefaction cell by cell  
  for(i in 1:length(cell.list)) {
    td <- data.frame(spp = cr$n[cr$cell == cell.list[i]])
    td <- td[!is.na(td$spp), ]
    
  # coverage rarefaction using iNEXT function
  # check for warning
    out_warn[i] <- 0
    temp1 <- tryCatch(estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL), warning = function(w) {out_warn[i] <<- 1})
    
  # if there was a warning run again
    if(out_warn[i] == 1) temp1 <- estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL)
    out_cov[i] <- temp1[, 4]
    
  # size rarefaction using iNEXT function
    temp2 <- estimateD(td, datatype = "abundance", base = "size", level = min.rec, conf = NULL)
    out_size[i] <- temp2[, 4]
  }
  
# put the rarefaction estimates into the raster
# need to include the missing cell values as well as the occupied cells
  cell_cov <- rep(NA, length(getValues(raster)))
  cell_size <- rep(NA, length(getValues(raster)))
  cell_cov_warn <- rep(NA, length(getValues(raster)))
  
# add the occupied cells
  cell_cov[cell.list] <- out_cov
  cell_size[cell.list] <- out_size
  
# coverage estimates with warning cells set to NA
  cell_cov_warn[cell.list] <- out_warn
  out_cov_warn <- ifelse(out_warn == 1, NA, out_cov)
  cell_cov_warn[cell.list] <- out_cov_warn
  
# generate the raster object for estimated richness  
  rast_cov <- setValues(raster, cell_cov)
  rast_size <- setValues(raster, cell_size)
  
# coverage raster with warning cells set to NA
  rast_cov_warn <- setValues(raster, cell_cov_warn)
  
# number of records per cell
  nrec <- rep(NA, length(getValues(raster)))
  nrec[nr$cell] <- nr$n.rec
  
  nrec[nr$cell] <- nr$n.rec
  
# raw species richness
  spp_per_cell <- as.numeric(factor(dat2$species))
  n_spp <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(unique(na.omit(x))) })
  m_spp <- mask(n_spp, raster)
  plot(m_spp)
  
# return the values for each cell and the rasters
  return(list(cell_cov, rast_cov, cell_size, rast_size, nrec, cell_cov_warn, rast_cov_warn, m_spp))
  
  }
# -----------------------------------------------------------------------------------

# run & save multiple families function ---------------------------------------------------------
# notes -----------------------------------------------------------------------------
# there are lists for each family x status
# these 8 lists as follows: [[1]] df sr calculated by coverage - w warning cells
                          # [[2]] raster sr calculated by coverage - w warning cells
                          # [[3]] df sr calculated by size (i.e. 15-rec rarefaction)           
                          # [[4]] raster sr calculated by size (i.e. 15-rec rarefaction)
                          # [[5]] df number of records per cell
                          # [[6]] df coverage w warning cells removed
                          # [[7]] raster coverage w warning cells removed
                          # [[8]] raster raw species richness
  
# I am going to save [[7]] into raster
# the others will go into dedicated CSVs
# ------------------------------------------------------------------------------------    

# run for Fabaceae sans Acacia (fna)
  inext_fna <- function(family_name, stat) {
    dat_fam <- dat %>%
      filter(family == family_name,
             status == stat) %>%
      mutate(species = factor(species))
    
    x <- inext(dat_fam, min.rec = 15, coverage = 0.8)  
    
    par(mfrow = c(2, 2))
    
    # plot records
    nrec <- setValues(raster, x[[5]])
    plot(nrec)
    # plot raw sr
    plot(x[[8]]) 
    # plot iNEXT  warining cells retained
    plot(x[[2]])   
    # plot iNEXT  warining cells removed
    plot(x[[7]])   
    
    # save raster
    rasterfile <- paste0("Results/Fabaceae sans Acacia/rasters/iNEXT/", family_name, "_", stat, ".grd")
    writeRaster(x[[7]],
                filename = rasterfile,
                overwrite = T)
    
    # save data frame  
    records <- x[[5]]
    raw.richness <- getValues(x[[8]])
    rarefaction <- x[[3]]
    inext.w.warnings <- x[[1]]
    inext <- getValues(x[[7]]) 
    
    df <- cbind(records, 
                raw.richness,
                rarefaction,
                inext.w.warnings,
                inext)
    head(df)
    csvfile <- paste0("Results/Fabaceae sans Acacia/csv/", family_name, "_", stat, ".csv")
    write.csv(df, csvfile)
    
  }
  
  inext_fna("FNA", "native") # ifnore warnings
  
# scaled species richness -------------------------------------------------------------
  fna <- raster("Results/Fabaceae sans Acacia/rasters/iNEXT/FNA_native.grd")
  
  prop_sr <- function(raster, raster.name) {
    
    lraster <- log(raster)
    
    prop <- lraster/cellStats(lraster, stat = 'max', na.rm = T)
    
    rasterfile <- paste0("Results/Fabaceae sans Acacia/rasters/scaled/", raster.name, "_observed.grd")
    
    writeRaster(prop, rasterfile, overwrite = T)
    
    return(plot(prop))
    
  }
  
  prop_sr(fna, "FNA_native")
  
# modelling native richness ----------------------------------------------------------
  aus <- raster
  fna <- raster("Results/Fabaceae sans Acacia/rasters/scaled/FNA_native_observed.grd")
  spp <- as.data.frame(fna) %>%
    mutate(cell_id = 1:length(fna))
  names(spp) <- c("FNA_native", "cell_id")

# merge
  fpv <- left_join(pv, spp, by = "cell_id")

# identify spatial auto-correlation --------------------------------------------------------------
# identify spatial autocorrelation function (returns p-value)
  xy <- fpv %>% 
        filter(!is.na(FNA_native)) %>%
        dplyr::select(FNA_native, long, lat)
  coords <- cbind(xy$long, xy$lat)
  w <- fields:::rdist(coords)
  m_i <- Moran.I(x = xy[, 1], w = w, na.rm = T)
  morans <- data.frame(m_i)
  write.csv(morans, "Results/Fabaceae sans Acacia/csv/Morans_i.csv", row.names = F)

# modelling ---------------------------------------------------------------
# linear terms 
  l_formula <- formula(FNA_native ~ hii +
                         th +
                         pcoldq +
                         pwarmq +
                         ts + 
                         arid + 
                         amt  + 
                         proportion_cover)
# test different methods for modelling spatial autocorrelation, and choose best fit
  model_e <- gls(l_formula, data = fpv, correlation = corExp(form = ~long + lat, nugget=T) , na.action = na.omit, method = "ML")
  model_g <- gls(l_formula, data = fpv, correlation = corGaus(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  model_s <- gls(l_formula, data = fpv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  model_r <- gls(l_formula, data = fpv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  model_lm <- lm(l_formula, data = fpv, na.action = na.omit)
  
# compare models using AICc
  model_sel <- model.sel(model_e, model_g , model_s, model_r, model_lm)
  model_sel # best model is model 
  model_sel_df <- data.frame(model_sel)
# best linear terms model  
  lm_ms <- model_e
  
# quadratic terms
  hii2 <- fpv$hii^2
  th2 <- fpv$th^2
  pcoldq2 <- fpv$pcoldq^2
  pwarmq2 <- fpv$pwarmq^2
  ts2 <- fpv$ts^2
  arid2 <- fpv$arid^2
  amt2 <- fpv$amt^2
  
  q_formula <- formula(FNA_native ~ hii + hii2 +
                         th + th2 +
                         pcoldq + pcoldq2 +
                         pwarmq + pwarmq2 +
                         ts + ts2 +
                         arid + arid2 + 
                         amt + amt2 + 
                         proportion_cover)
  
  
# test different methods for modelling spatial autocorrelation, and choose best fit
  model_e <- gls(q_formula, data = fpv, correlation = corExp(form = ~long + lat, nugget=T) , na.action = na.omit, method = "ML")
  model_g <- gls(q_formula, data = fpv, correlation = corGaus(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  model_s <- gls(q_formula, data = fpv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  model_r <- gls(q_formula, data = fpv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  model_lm <- lm(q_formula, data = fpv, na.action = na.omit)

# compare models using AICc
  model_sel <- model.sel(model_e, model_g , model_s, model_r, model_lm)
  model_sel
# best quadratic terms model  
  quad_ms <- model_e
  
# grand model selection 
  model_sel <- model.sel(lm_ms, quad_ms)
  model_sel_df <- model_sel
  write.csv(model_sel_df, "Results/Fabaceae sans Acacia/csv/linear quadratic model selection AICc.csv", row.names = T)
  
  ci <- data.frame(intervals(quad_ms, 0.95, which = "coef")$coef)
  ci
  write.csv(ci, "Results/Fabaceae sans Acacia/csv/model estimates.csv", row.names = T)
  
# predicted richness -------------------------------------------------------------------
# best model was quadratic model section
  psr <- predict(quad_ms, newdata = pv)
  psr_lm <- predict(lm_ms, newdata = pv)
# make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(aus)), ncol = 2)
  class(psr) <- "numeric"
  class(psr_lm) <- "numeric"
# add the occupied cells
  x[cell_id$cell_id, 1] <- psr
  x[cell_id$cell_id, 2] <- psr_lm
  hist(x)

# save  
  pred_rasterise <- function(raster_df, raster_template, raster_names_df){
    
    save <- paste0("Results/Fabaceae sans Acacia/rasters/predicted/", raster_names_df, "_predicted.grd")
    
    xa <- setValues(raster_template, raster_df)
    xb <- calc(xa, fun = function(x) {x[x<0] <- 0; return(x)})
    xc <- calc(xb, fun = function(x) {x[x>1] <- 1; return(x)})
    writeRaster(xc, save, overwrite = T)
    
  }
  
# run  
  pred_rasterise(x[, 1], aus, "FNA_native")
  pred_rasterise(x[, 2], aus, "FNA_native_lm")
# potential richness ----------------------------------------------------------  
# predicted native minus observed nonnative 
  
# observed nonnative 
  observed_raster <- raster("C:/Users/s436862/Dropbox/NNF/Results/rasters/scaled/Fabaceae_nonnative_observed.grd")
  plot(observed_raster)
  
# predicted native  
  predicted_raster <- raster("Results/Fabaceae sans Acacia/rasters/predicted/FNA_native_predicted.grd")
  plot(predicted_raster)
  
# calculate difference
  observed_raster[is.na(observed_raster[])] <- 0 
  poten_a <- predicted_raster - observed_raster
  poten_b <- calc(poten_a, fun = function(x) {x[x<0] <- 0; return(x)})
  
  save_name <- paste0("Results/Fabaceae sans Acacia/rasters/potential/FNA_nonnative__potential.grd")
  writeRaster(poten_b, filename = save_name, overwrite = T)  

# observed native minus observed nonnative  
  obs_nat_raster <- raster("Results/Fabaceae sans Acacia/rasters/scaled/FNA_native_observed.grd")
    
# calculate difference
  observed_raster[is.na(observed_raster[])] <- 0 
  poten_a <- obs_nat_raster - observed_raster
  poten_b <- calc(poten_a, fun = function(x) {x[x<0] <- 0; return(x)})
    
  save_name <- paste0("Results/Fabaceae sans Acacia/rasters/potential/FNA_obs_nat_potential.grd")
  writeRaster(poten_b, filename = save_name, overwrite = T)

# r2 ----------------------------------------------------------------------
  adj_r2 <- function(obs_raster, pred_raster){
    y <- getValues(obs_raster)
    yhat <- getValues(pred_raster)
    r_squared = 1 - sum((y - yhat)^2, na.rm = T) / sum((y - mean(y, na.rm = T))^2, na.rm = T)
    r_squared
    
    n <- length(na.omit(y))
    p <- 9
    adj_r_squared = 1 - (1 - r_squared) * ((n - 1)/(n - p - 1))
    
    return(adj_r_squared) 
  } 
  r2 <- data.frame(family = "FNA",
               quad_nat = NA,
               lin_nat = NA)
  r2$quad_nat[1] <- adj_r2(fna, predicted_raster)
  
  predicted_raster_lm <- raster("Results/Fabaceae sans Acacia/rasters/predicted/FNA_native_lm_predicted.grd")
  r2$lin_nat[1] <- adj_r2(fna, predicted_raster_lm)
  r2
 
  write.csv(r2, "Results/Fabaceae sans Acacia/csv/FNA r2.csv", row.names = F)

# --------------------------------------------------------------------------      
  