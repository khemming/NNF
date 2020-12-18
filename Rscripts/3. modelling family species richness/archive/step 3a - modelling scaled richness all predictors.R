########################################################
# model building
########################################################

# scope -----------------------------------------------
# model relationships between native and non-native taxa against all environmental and anthropogenic variables, assess how improved R2s are (and therefore whether or not we're missing responses of these groups to Australia)

# library ----------------------------------------------
  library(tidyverse)
  library(raster)
  library(broom)
 
  rm(list = ls())

# data --------------------------------------------------
# rasters
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/scaled")

  current.list <- list.files(pattern = "observed.grd")
  raster_names <- gsub(pattern = ".observed.grd$", "", current.list)
  raster_stack <- stack(current.list)
  names(raster_stack) <- raster_names
  list2env(setNames(unstack(raster_stack), names(raster_stack)), .GlobalEnv)
  
  spp <- as.data.frame(raster_stack, na.rm = F)
  glimpse(spp)
  
  setwd("C:/Users/s436862/Dropbox/Non-native-families")

# family names
  family_name <- read.csv("Results/csv/summary statistics/master family summary table.csv", stringsAsFactors = F) %>%
      filter(study_family == "yes")

# environmental and anthropogenic variables 
  evs <- read.csv("Results/csv/predictor variables/predictor variables scaled.csv", header = T) %>%
  dplyr::select(-twarmm, -pwetm, -pdrym, -tcoldm) # these covary too much with quarter data to include)
  glimpse(evs)
  
# bind  
  spp_ev <- bind_cols(spp, evs) %>%
                     filter(cell_category_all == "land") %>%
                     dplyr::select(-cell_id, -cell_category_all)
                                   
  head(spp_ev)
  
# models --- ----------------------------------------------------------  
# model_summary (array) requirements ------------------------------------------
# row_names (48):
#               24 predictors coefficients, repeated for native and non-native
# columns (6):
#               variable (names of parameters)
#               estimate (mean estimate of parameter)
#               lower.ci (lower confidence interval)
#               upper.ci (lower confidence interval)
#               plot.names (proper parameter names; I fill in next script)
# matrice.names (19): 
#               family and taxa names
#               native and non-native will be paired in plots (so 17 not 34 matrices)
# array terms ---------------------------------------------------------
# column row and matrice names
  col_names <- c("variable", 
                 "status", 
                 "estimate", 
                 "lower_ci", 
                 "upper_ci", 
                 "plot_names")
  ev_names <- names(rep(evs[4:27], 2))
  matrice_names <- as.matrix(family_name$family)
  
  model_summary <- array(data = NA,
                         dim = c(length(ev_names), length(col_names), length(matrice_names)),
                         dimnames = list(ev_names, col_names, matrice_names))
  
  model_summary[1:48,"status", ] <- rep(c("native", "non-native"), each = 24)
  model_summary[1:48, "variable", ] <- ev_names

# check
  model_summary[,,"Asparagaceae"] # looks good  

# r2
  r2 <- data.frame(matrix(nrow = length(raster_names),
                                 ncol = 2))
  names(r2) <- c("family", "r2")
  r2$family <- raster_names
  glimpse(r2)
  
# models (as list)
  models <- vector(mode = "list", length = length(raster_names))
  names(models) <- raster_names
  
# align raster names with family/taxa names
  matrix_iteration <- rep(family_name$family, each = 2)
  
# loop start -------------------------------------------------------
# native loop
  for (i in seq(1, length(raster_names), 2)) {
    
  # model
    x <- lm(spp_ev[, i] ~ prop_cover + 
                          amt +    ap +    arid +   elev +   hii +    
                          iso +    mdr +   pawc +   pcoldq + pdryq +  
                          pewc +   ps +    pwarmq + pwetq +  
                          rz +     sp +    st +     tar +    tcoldq + 
                          tdryq + th +     ts +     twarmq + twetq,  
                          data = spp_ev)
  # coefficients
    x_coef <- summary(x)$coefficients[3:26, 1]
    model_summary[1:24, "estimate", matrix_iteration[i]] <- x_coef
  # confidence intervals
    x_ci <- confint(x)[3:26, 1]
    model_summary[1:24, c("lower_ci","upper_ci"), matrix_iteration[i]] <- x_ci
  # r2
    r2[i, 2] <- summary(x)$adj.r.squared
  # models
    models[[i]] <- x
  }
  
# non-native loop
  for (i in seq(2, length(raster_names), 2)) {
    
    x <- lm(spp_ev[, i] ~ prop_cover + 
                          amt +    ap +    arid +   elev +   hii +    
                          iso +    mdr +   pawc +   pcoldq + pdryq +  
                          pewc +   ps +    pwarmq + pwetq +  
                          rz +     sp +    st +     tar +    tcoldq + 
                          tdryq + th +     ts +     twarmq + twetq,  
                          data = spp_ev)
  # coefficients
    x_coef <- summary(x)$coefficients[3:26, 1]
    model_summary[25:48, "estimate", matrix_iteration[i]] <- x_coef
  # confidence intervals
    x_ci <- confint(x)[3:26, 1]
    model_summary[25:48, c("lower_ci","upper_ci"), matrix_iteration[i]] <- x_ci
  # r2
    r2[i, 2] <- summary(x)$adj.r.squared
  # models
    models[[i]] <- x
  }
  
# checks
  model_summary[,,"Asparagaceae"]
  head(r2)
  models$Amaranthaceae.native
  
  
# check residuals ------------------------------------------------------
  par(mfrow = c(2, 2))
  plot(models[[1]]$residuals)
  plot(models[[2]]$residuals)
  plot(models[[3]]$residuals)
  plot(models[[4]]$residuals)
  
  plot(models[[5]]$residuals)
  plot(models[[6]]$residuals)
  plot(models[[7]]$residuals)
  plot(models[[8]]$residuals)
  
  plot(models[[9]]$residuals)
  plot(models[[10]]$residuals)
  plot(models[[11]]$residuals)
  plot(models[[12]]$residuals)
  
  plot(models[[13]]$residuals)
  plot(models[[14]]$residuals)
  plot(models[[15]]$residuals)
  plot(models[[16]]$residuals)
  
  plot(models[[17]]$residuals)
  plot(models[[18]]$residuals)
  plot(models[[19]]$residuals)
  plot(models[[20]]$residuals)
  
  plot(models[[21]]$residuals)
  plot(models[[22]]$residuals)
  plot(models[[23]]$residuals)
  plot(models[[24]]$residuals)
  
# histogram
  hist(models[[1]]$residuals)
  hist(models[[2]]$residuals)
  hist(models[[3]]$residuals)
  hist(models[[4]]$residuals)
  
  hist(models[[5]]$residuals)
  hist(models[[6]]$residuals)
  hist(models[[7]]$residuals)
  hist(models[[8]]$residuals)
  
  hist(models[[9]]$residuals)
  hist(models[[10]]$residuals)
  hist(models[[11]]$residuals)
  hist(models[[12]]$residuals)
  
  hist(models[[13]]$residuals)
  hist(models[[14]]$residuals)
  hist(models[[15]]$residuals)
  hist(models[[16]]$residuals)
  
  hist(models[[17]]$residuals)
  hist(models[[18]]$residuals)
  hist(models[[19]]$residuals)
  hist(models[[20]]$residuals)
  
  hist(models[[21]]$residuals)
  hist(models[[22]]$residuals)
  hist(models[[23]]$residuals)
  hist(models[[24]]$residuals)
  
  
# correlation matrix -------------------------------------------
# nat 
  nat_spp <- spp %>% dplyr::select(seq(1, length(raster_names), 2))
# non-nat
  non_nat_spp <- spp %>% dplyr::select(seq(2, length(raster_names), 2))
  
  corr_mat <- matrix(nrow = nrow(family_name),
                     ncol = 2)
  corr_mat[, 1] <- family_name$family
  
  for (i in 1:nrow(family_name)){
    
    corr_mat[i, 2] <- cor(nat_spp[i], non_nat_spp[i], use = "complete.obs", method = "spearman")
    
  }  
  
# save ---------------------------------------------------------  
# Rdata
  save.image("Data files/Rdata/model_data_all_predictor_variables.RData")

# r2 data frame
  write.csv(r2, "Results/csv/summary statistics/r2 all_predictor_vars.csv", row.names = F)
  
# ---------------------------------------------------------------------  
  