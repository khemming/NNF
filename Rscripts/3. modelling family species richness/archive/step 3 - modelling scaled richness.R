

# scope -----------------------------------------------
# model relationships between native and nonnative taxa against environmental and anthropogenic variables
# save model data in dedicated data frames (e.g. residual df, r2 df)
# have these in formats useful for plotting

# library ----------------------------------------------
  library(tidyverse)
  library(raster)
  
  rm(list = ls())

# data --------------------------------------------------
# rasters
  setwd("C:/Users/Hemming/Dropbox/NNF/Results/rasters/scaled")
  current.list <- list.files(pattern = "_observed.grd")
  raster_names <- gsub(pattern = "_observed.grd$", "", current.list)
  raster_stack <- stack(current.list)
  names(raster_stack) <- raster_names
  list2env(setNames(unstack(raster_stack), names(raster_stack)), .GlobalEnv)
  spp <- as.data.frame(raster_stack, na.rm = F)
  glimpse(spp)
  
  setwd("C:/Users/Hemming/Dropbox/NNF")

# family names
  family_name <- read.csv("Results/csv/summary statistics/study families.csv", stringsAsFactors = F) %>%
                 filter(study_family == "yes") %>%
         dplyr::select(family)
  
# environmental and anthropogenic variables 
  evs <- read.csv("Results/csv/predictor variables/predictor variables scaled.csv", header = T)
 
# bind  
  spp_ev <- bind_cols(spp, evs) %>%
            filter(cell_category_all == "land")
  
# spatial data frame for modelling  
  coordinates(spp_pv) <- ~ lat + long  
  head(spp_ev)
  
# models --- ----------------------------------------------------------  
# model_summary (array) requirements ------------------------------------------
# row_names (14):
#               coefficients, repeated for native and nonnative
#               note we are dropping prop_cover
# columns (6):
#               variable (names of parameters)
#               estimate (mean estimate of parameter)
#               lower.ci (lower confidence interval)
#               upper.ci (lower confidence interval)
#               plot.names (proper parameter names; I fill in next script)
# matrice.names (19): 
#               family and taxa names
#               native and nonnative will be paired in plots (so 17 not 34 matrices)
# array terms ---------------------------------------------------------
# model summary
  col_names <- c("variable", 
                 "status", 
                 "estimate", 
                 "lower_ci", 
                 "upper_ci", 
                 "plot_names")
  
  ev_names <- rep(vs_factors[2:8], 2) # removing proportion of land cover covariate
  matrice_names <- as.matrix(family_name$family)
  model_summary <- array(data = NA,
                         dim = c(14, 6, nrow(family_name)),
                         dimnames = list(ev_names, col_names, matrice_names))
  model_summary[1:14,"status",] <- rep(c("native", "nonnative"), each = 7)
  model_summary[1:14, "variable", ] <- ev_names

# check
  model_summary[,,"Asparagaceae"] # looks good  

# r2
  r2 <- data.frame(matrix(nrow = length(raster_names),
                                 ncol = 2))
  names(r2) <- c("family", "r2")
  r2$family <- raster_names
  
# models (as list)
  models <- vector(mode = "list", length = length(raster_names))
  names(models) <- raster_names
  
# align raster names with family/taxa names
  matrix_iteration <- rep(family_name$family, each = 2)
  
# loop start -------------------------------------------------------
# native loop
  for (i in seq(1, length(raster_names), 2)) {
    
  # model
    x <- lm(spp_ev[, i] ~ proportion_cover + amt + arid + ts + pcoldq + pwarmq + th + hii, data = spp_ev)
  # coefficients
    x_coef <- summary(x)$coefficients[3:9, 1]
    model_summary[1:7, "estimate", matrix_iteration[i]] <- x_coef
  # confidence intervals
    x_ci <- confint(x)[3:9, ]
    model_summary[1:7, c("lower_ci","upper_ci"), matrix_iteration[i]] <- x_ci
  # models
    models[[i]] <- x
  }
  
# nonnative loop
  for (i in seq(2, length(raster_names), 2)) {
    
    x <- lm(spp_ev[, i] ~ proportion_cover + amt + arid + ts + pcoldq + pwarmq + th + hii, data = spp_ev)
  # coefficients
    x_coef <- summary(x)$coefficients[3:9, 1]
    model_summary[8:14, "estimate", matrix_iteration[i]] <- x_coef
  # confidence intervals
    x_ci <- confint(x)[3:9, ]
    model_summary[8:14, c("lower_ci","upper_ci"), matrix_iteration[i]] <- x_ci
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
  
  plot(models[[25]]$residuals)
  plot(models[[26]]$residuals)
  plot(models[[27]]$residuals)
  plot(models[[28]]$residuals)
  
  plot(models[[29]]$residuals)
  plot(models[[30]]$residuals)
  plot(models[[31]]$residuals)
  plot(models[[32]]$residuals)
  
  plot(models[[33]]$residuals)
  plot(models[[34]]$residuals)
  
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
  
  hist(models[[25]]$residuals)
  hist(models[[26]]$residuals)
  hist(models[[27]]$residuals)
  hist(models[[28]]$residuals)
  
  hist(models[[29]]$residuals)
  hist(models[[30]]$residuals)
  hist(models[[31]]$residuals)
  hist(models[[32]]$residuals)
  
  hist(models[[33]]$residuals)
  hist(models[[34]]$residuals)
  
  

  
# save ---------------------------------------------------------  
# Rdata
  save.image("Data files/Rdata/model_data.RData")

# r2 data frame
  write.csv(r2, "Results/csv/summary statistics/r2.csv", row.names = F)
  

  
# ---------------------------------------------------------------------  
  