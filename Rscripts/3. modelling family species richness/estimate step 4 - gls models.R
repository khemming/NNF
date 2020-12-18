


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
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/scaled")
  current.list <- list.files(pattern = "_observed.grd")
  names <- gsub(pattern = "\\_observed.grd$", "", current.list)
  setwd("C:/Users/s436862/Dropbox/NNF")

# family richness and environmental and anthropogenic variables 
  fpv <- read.csv("Results/csv/families and predictors/families predictors 1156.csv", header = T)
 
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
  
  moran_l
  names(moran_l) <- names  
  
# Moran's I data frame for saving: 4 x 6
  m_mat <- round(matrix(unlist(moran_l), byrow = T, nrow=length(names)), 4) # four columns
  row.names(m_mat) <- names
  colnames(m_mat) <- c("observed","expected", "sd", "p.value")
  m_df <- data.frame(m_mat) %>%
          mutate(fam_status = row.names(m_mat),
                 family = rep(names22, each = 2),
                 status = rep(c("native", "nonnative"), 22)) %>%
          group_by(status, family)
  
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
  for (i in 1:length(names)){
    fam_col <- fpv[, i]              
    gls_l[[i]] <- model_sel_fun(fam_col)
    cor_m[i] <- gls_l[[i]]$correlation[1] # best correlation structure
  }
  
  cor_m
  
# check
  gls_l[[2]]
  
# save all gls model data -----------------------------------------------
# in data frame converted from list

# unlist into this matrix    
  gls_mat <- matrix(nrow = 5, ncol = 18 * length(names)) # ncol = 18 * no. of spp
  
# unlisted gls is is the wrong structure (5 x 18 * spp; we want 5 * spp x 18)
# this code changes it that
# divide 796 into leading (l) and trailing (t) sequences of 18 (n)
  n <- 18
  l <- seq(1, 792, n)
  tail(l)
  t <- seq(n, 792, 18)
  tail(t)
  
# rows its going in to  
  r <- seq(5, 220, 5)
  s <- seq(1, 216, 5)
  
  gls_mat[,] <- unlist(gls_l, recursive = T)
  gls_mat2 <- matrix(nrow = length(names) * 5, ncol = 18, byrow = F)
  
  for (i in 1:length(names)){
    
  gls_mat2[r[i]:s[i],] <- gls_mat[,l[i]:t[i]]
  
    }
  
  colnames(gls_mat2) <- colnames(gls_l[[1]])
 
# run lowest-AIC models ------------------------------------------------------------------
# set correct correlation strctures
  m_formula <- formula(fam ~ hii + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover)
# revalue correlation structure output from matrix    
  model_df <- data.frame(cor_m) %>%
              mutate(family = names,
                     joiner = if_else(cor_m == "", "lm", 
                              if_else(cor_m == "corExp(~long+lat,T)", "e",
                              if_else(cor_m == "corSpher(~long+lat,T)", "s",
                              if_else(cor_m == "corRatio(~long+lat,T)", "r", "g")))))
  
  cor_str <- data.frame(run_method = c("corExp(form = ~long + lat, nugget=T)",
                                       "corSpher(form = ~long + lat, nugget=T)",
                                       "corRatio(form = ~long + lat, nugget=T)",
                                       "corGaus(form = ~long + lat, nugget=T)"),
                        joiner = c("e", "s", "r", "g"))
  cor_str2 <- left_join(model_df, cor_str, by = "joiner")

  lm_m <- matrix(nrow = 9, ncol = 3)

# run for each family because correlation strcuture cannot be coerced into a formula
# families 1 - 10 -------------------------------------------------------------  
# Exp
  x <- 1
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 2
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 3
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 4
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 5
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 6
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 7
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 8
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 9
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 10
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# families 11 - 20 -------------------------------------------------------------  
# Exp
  x <- 11
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 12
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 13
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 14
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 15
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# lm
  x <- 16
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- lm(fam ~ hii + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                               data = fpv, na.action = na.omit)
  lm_m[,2] <- coef(model_list[[x]])
  lm_m[, c(1, 3)] <- confint(model_list[[x]])
  ci_list[[x]] <- data.frame(lm_m)
  colnames(ci_list[[x]]) <- c("lower", "est.", "upper")
  
# Exp
  x <- 17
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 18
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 19
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 20
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# families 21 - 30 -------------------------------------------------------------  
# Exp
  x <- 21
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# lm
  x <- 22
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- lm(fam ~ hii + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                        data = fpv, na.action = na.omit)
  lm_m[,2] <- coef(model_list[[x]])
  lm_m[, c(1, 3)] <- confint(model_list[[x]])
  ci_list[[x]] <- data.frame(lm_m)
  colnames(ci_list[[x]]) <- c("lower", "est.", "upper")
  
# Spher
  x <- 23
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 24
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 25
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 26
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 27
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# lm
  x <- 28
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- lm(fam ~ hii + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                        data = fpv, na.action = na.omit)
  lm_m[,2] <- coef(model_list[[x]])
  lm_m[, c(1, 3)] <- confint(model_list[[x]])
  ci_list[[x]] <- data.frame(lm_m)
  colnames(ci_list[[x]]) <- c("lower", "est.", "upper")
  
# Exp
  x <- 29
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 30
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# families 31 - 40 -------------------------------------------------------------  
# Exp
  x <- 31
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 32
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 33
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 34
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 35
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 36
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Exp
  x <- 37
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Exp
  x <- 38
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Exp
  x <- 39
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Spher
  x <- 40
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# families 41 - 44 -------------------------------------------------------------    
# Ratio
  x <- 41
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corRatio(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# lm
  x <- 42
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- lm(fam ~ hii + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                        data = fpv, na.action = na.omit)
  lm_m[,2] <- coef(model_list[[x]])
  lm_m[, c(1, 3)] <- confint(model_list[[x]])
  ci_list[[x]] <- data.frame(lm_m)
  colnames(ci_list[[x]]) <- c("lower", "est.", "upper")
  
# Exp
  x <- 43
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Exp
  x <- 44
  fam <- fpv[, x]
  cor_str2[x, "run_method"]
  model_list[[x]] <- gls(m_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)

# -------------------------------------------------------------  
  names(model_list) <- names
  names(ci_list) <- names
  names(moran_l) <- names
  names(gls_l) <- names
 
# save ---------------------------------------------------------  
  write.csv(m_df, "Results/csv/modelling/Morans I.csv", row.names = F)
  write.csv(gls_mat2, "Results/csv/modelling/GLS model structures.csv", row.names = T)
  
  save.image("Data files/Rdata/linear_model_data.RData")

# ---------------------------------------------------------------------  