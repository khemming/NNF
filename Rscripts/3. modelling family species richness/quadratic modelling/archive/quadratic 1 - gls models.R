

# scope -----------------------------------------------
# model all families with non-linear model terms
# library ----------------------------------------------
  library(raster)
  library(MuMIn)
  library(ape)
  library(nlme)  
  library(sp)
  library(tidyverse)

  rm(list = ls())

# data --------------------------------------------------
# family names by status
  setwd("C:/Users/Hemming/Dropbox/NNF/Results/rasters/scaled")
  current.list <- list.files(pattern = "_observed.grd")
  names <- gsub(pattern = "\\_observed.grd$", "", current.list)
  setwd("C:/Users/Hemming/Dropbox/NNF")

# family richness and environmental and anthropogenic variables 
  fpv <- read.csv("Results/csv/families and predictors/families predictors 1131.csv", header = T)
 
# identify spatial auto-correlation --------------------------------------------------------------
# store all results for supplementary materials
  moran_l <- list()
  gls_l <- list()
  model_list <- list()
  ci_list <- list()
  cor_m <- matrix(nrow = length(names))
  
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
  for (i in 1:length(names)){
    
    moran_l[[i]] <- moran_fun(fpv[, i], i)
    
  }
  
  moran_l # all families have spatial-autocorrelation
  names(moran_l) <- names  
  
# unlist if need
  
# set quadratic model structure -------------------------------------------------------------------
  hii2 <- fpv$hii^2
  th2 <- fpv$th^2
  pcoldq2 <- fpv$pcoldq^2
  pwarmq2 <- fpv$pwarmq^2
  ts2 <- fpv$ts^2
  arid2 <- fpv$arid^2
  amt2 <- fpv$amt^2
    
  q_formula <- formula(fam ~ hii + hii2 +
                             th + th2 +
                             pcoldq + pcoldq2 +
                             pwarmq + pwarmq2 +
                             ts + ts2 +
                             arid + arid2 + 
                             amt + amt2 + 
                             proportion_cover)
  
# model selection --------------------------------------------------------------------------------
# test different methods for modelling spatial autocorrelation, and choose best fit
  model_sel_fun <- function(fam) {
# model methods to account for spatial autocorrelation
    model_e <- gls(q_formula, data = fpv, correlation = corExp(form = ~long + lat, nugget=T) , na.action = na.omit, method = "ML")
    model_g <- gls(q_formula, data = fpv, correlation = corGaus(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_s <- gls(q_formula, data = fpv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_r <- gls(q_formula, data = fpv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
# compare models using AICc
    model_sel <- model.sel(model_e, model_g , model_s, model_r)
    return(model_sel)
  } 
  
# run
  for (i in 1:length(names)){
    fam <- fpv[, i]              
    gls_l[[i]] <- model_sel_fun(fam)
    cor_m[i] <- gls_l[[i]]$correlation[1] # best correlation structure
  }
  
  cor_m
  
# test for random group
  gls_l[[2]]
  
# run lowest-AIC models ------------------------------------------------------------------
# revalue correlation structure output from matrix    
  model_df <- data.frame(cor_m) %>%
              mutate(family = names,
                     joiner = if_else(cor_m  == "corExp(~long+lat,T)", "e",
                                      if_else(cor_m  == "corSpher(~long+lat,T)", "s",
                                              if_else(cor_m == "corRatio(~long+lat,T)", "r", "g"))))
  
  cor_str <- data.frame(run_method = c("corExp(form = ~long + lat, nugget=T)",
                                       "corSpher(form = ~long + lat, nugget=T)",
                                       "corRatio(form = ~long + lat, nugget=T)",
                                       "corGaus(form = ~long + lat, nugget=T)"),
                        joiner = c("e", "s", "r", "g"))
  cor_str2 <- left_join(model_df, cor_str, by = "joiner")

# run for each family because correlation structure cannot be coerced into formula
# families 1 - 10 -------------------------------------------------------------  
# Exp
  x <- 1
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 2
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 3
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 4
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Ratio
  x <- 5
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corRatio(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 6
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 7
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 8
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 9
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 10
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# families 11 - 20 -------------------------------------------------------------  
# Exp
  x <- 11
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 12
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Ratio
  x <- 13
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corRatio(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 14
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 15
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 16
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Ratio
  x <- 17
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corRatio(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 18
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 19
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 20
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# families 21 - 30 -------------------------------------------------------------  
# Exp
  x <- 21
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 22
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 23
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 24
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 25
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 26
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Ratio
  x <- 27
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corRatio(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 28
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 29
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 30
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# families 31 - 40 -------------------------------------------------------------  
# Exp
  x <- 31
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 32
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Ratio
  x <- 33
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corRatio(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 34
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 35
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 36
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Ratio
  x <- 37
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corRatio(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Exp
  x <- 38
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)

# Exp
  x <- 39
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Gaus
  x <- 40
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# families 41 - 44 -------------------------------------------------------------    
# Ratio
  x <- 41
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corRatio(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Gaus
  x <- 42
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Exp
  x <- 43
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Gaus
  x <- 44
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)

# -------------------------------------------------------------  
  names(model_list) <- names
  names(ci_list) <- names
  names(moran_l) <- names
  names(gls_l) <- names
    
# save ---------------------------------------------------------  
# Rdata
  save.image("Data files/Rdata/quadratic_model_data.RData")

# ---------------------------------------------------------------------  