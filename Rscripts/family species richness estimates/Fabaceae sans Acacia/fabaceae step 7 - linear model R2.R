# library ------------------------------------------------------------------
  library(raster)
  
  rm(list = ls())
  
# data ---------------------------------------------------------------------   
# family names
  names22 <- read.csv("Results/csv/family summary statistics/study families.csv", stringsAsFactors = F)[1:22, 1]
  glimpse(names22)
  
# observed native
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/scaled")
  ob_nat_rasters <- list.files(pattern = "_native_observed.grd")
  ob_nat_names <- gsub(pattern = ".grd$", "", ob_nat_rasters)
  ob_nat <- stack(ob_nat_rasters)
  names(ob_nat) <- ob_nat_names
  
# observed nonnative
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/scaled")
  ob_nnat_rasters <- list.files(pattern = "_nonnative_observed.grd")
  ob_nnat_names <- gsub(pattern = ".grd$", "", ob_nnat_rasters)
  ob_nnat <- stack(ob_nnat_rasters)
  names(ob_nnat) <- ob_nnat_names
  
# predicted native
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/predicted")
  pred_nat_rasters <- list.files(pattern = "_native_predicted.grd")
  pred_nat_names <- gsub(pattern = ".grd$", "", pred_nat_rasters)
  pred_nat <- stack(pred_nat_rasters)
  names(pred_nat) <- pred_nat_names
  
# predicted nonnative
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/predicted")
  pred_nnat_rasters <- list.files(pattern = "_nonnative_predicted.grd")
  pred_nnat_names <- gsub(pattern = ".grd$", "", pred_nnat_rasters)
  pred_nnat <- stack(pred_nnat_rasters)
  names(pred_nnat) <- pred_nnat_names
  setwd("C:/Users/s436862/Dropbox/NNF")

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
  } # finish

# also comparing native species as the predicted raster for nonnatives    
# store results
  r2 <- data.frame(family = names22,
                   lin_nat = NA,
                   lin_nn_nn = NA,
                   lin_nn_nat = NA)
  
  
  for (i in 1:length(names22)){
    r2$lin_nat[i] <- adj_r2(ob_nat[[i]], pred_nat[[i]])
    r2$lin_nn_nn[i] <- adj_r2(ob_nnat[[i]], pred_nnat[[i]])
    r2$lin_nn_nat[i] <- adj_r2(ob_nnat[[i]], pred_nat[[i]])
  }
  r2
  
  write.csv(r2, "Results/csv/correlates/linear r2.csv", row.names = F)

# --------------------------------------------------------------------------  