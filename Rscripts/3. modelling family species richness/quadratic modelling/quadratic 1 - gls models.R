

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
 
# store all results for supplementary materials
  model_list <- list()
  ci_list <- list()
  
# set quadratic model structure -------------------------------------------------------------------
  hii2 <- fpv$hii^2
  th2 <- fpv$th^2
  pcoldq2 <- fpv$pcoldq^2
  pwarmq2 <- fpv$pwarmq^2
  ts2 <- fpv$ts^2
  arid2 <- fpv$arid^2
  amt2 <- fpv$amt^2
    
  q_formula <- formula(fam ~ hii2     + hii +
                             th2      + th +
                             pcoldq2  + pcoldq +
                             pwarmq2  + pwarmq +
                             ts2      + ts +
                             arid2    + arid + 
                             amt2     + amt + 
                             proportion_cover)
  
  lm_m <- matrix(nrow = 16, ncol = 3)
  
# run for each family with the correlation structure that the linear model worked best for
# families 1 - 10 -------------------------------------------------------------  
# Exp
  x <- 1
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 2
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 3
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 4
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 5
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 6
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 7
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 8
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 9
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 10
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# families 11 - 20 -------------------------------------------------------------  
# Exp
  x <- 11
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 12
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 13
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Gaus
  x <- 14
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corGaus(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 15
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# lm
  x <- 16
  fam <- fpv[, x]
  
  model_list[[x]] <- lm(q_formula, 
                        data = fpv, na.action = na.omit)
  lm_m[,2] <- coef(model_list[[x]])
  lm_m[, c(1, 3)] <- confint(model_list[[x]])
  ci_list[[x]] <- data.frame(lm_m)
  colnames(ci_list[[x]]) <- c("lower", "est.", "upper")
  
# Exp
  x <- 17
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 18
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 19
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 20
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# families 21 - 30 -------------------------------------------------------------  
# Exp
  x <- 21
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# lm
  x <- 22
  fam <- fpv[, x]
  
  model_list[[x]] <- lm(q_formula, 
                        data = fpv, na.action = na.omit)
  lm_m[,2] <- coef(model_list[[x]])
  lm_m[, c(1, 3)] <- confint(model_list[[x]])
  ci_list[[x]] <- data.frame(lm_m)
  colnames(ci_list[[x]]) <- c("lower", "est.", "upper")
  
# Spher
  x <- 23
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 24
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 25
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 26
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 27
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# lm
  x <- 28
  fam <- fpv[, x]
  
  model_list[[x]] <- lm(q_formula, 
                        data = fpv, na.action = na.omit)
  lm_m[,2] <- coef(model_list[[x]])
  lm_m[, c(1, 3)] <- confint(model_list[[x]])
  ci_list[[x]] <- data.frame(lm_m)
  colnames(ci_list[[x]]) <- c("lower", "est.", "upper")
  
# Exp
  x <- 29
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 30
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# families 31 - 40 -------------------------------------------------------------  
# Exp
  x <- 31
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 32
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 33
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 34
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Spher
  x <- 35
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
# Exp
  x <- 36
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Exp
  x <- 37
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Exp
  x <- 38
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Exp
  x <- 39
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Spher
  x <- 40
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corSpher(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# families 41 - 44 -------------------------------------------------------------    
# Ratio
  x <- 41
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corRatio(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# lm
  x <- 42
  fam <- fpv[, x]
  
  model_list[[x]] <- lm(q_formula, 
                        data = fpv, na.action = na.omit)
  lm_m[,2] <- coef(model_list[[x]])
  lm_m[, c(1, 3)] <- confint(model_list[[x]])
  ci_list[[x]] <- data.frame(lm_m)
  colnames(ci_list[[x]]) <- c("lower", "est.", "upper")
  
# Exp
  x <- 43
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)
  
# Exp
  x <- 44
  fam <- fpv[, x]
  
  model_list[[x]] <- gls(q_formula, correlation = corExp(form = ~long + lat, nugget=T), 
                         na.action = na.omit, method = "ML", data = fpv)
  ci_list[[x]] <- data.frame(intervals(model_list[[x]], 0.95, which = "coef")$coef)

# -------------------------------------------------------------  
  names(model_list) <- names
  names(ci_list) <- names
  
# save ---------------------------------------------------------  
# Rdata
  save.image("Data files/Rdata/quadratic_model_data.RData")

# ---------------------------------------------------------------------  