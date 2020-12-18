

# library ------------------------------------------------
  library(MuMIn)
  library(nlme)  
  library(tidyverse)
 
  rm(list = ls())

# data ----------------------------------------------------------------   
# linear model data
  load(("Data files/Rdata/linear_model_data.RData"))
  lin_models <- model_list
  lin_ci <- ci_list
  lin_moran <- moran_l 
  lin_gls <- gls_l
  
# quadratic model data
  load("Data files/Rdata/quadratic_model_data.RData")
  quad_models <- model_list
  quad_ci <- ci_list
  quad_moran <- moran_l 
  quad_gls <- gls_l

# selecting only native models for linear-quadratic model comparison
  nat_m <- seq(1, length(names), 2)
  qm <- quad_models[nat_m]
  lm <- lin_models[nat_m]

# R2s 
  lr2 <- read.csv("Results/csv/modelling/linear r2.csv") 
  head(lr2)
  qr2 <- read.csv("Results/csv/modelling/quadratic r2.csv")  
  head(qr2)  
  
# short family names  
  names22 <- read.csv("Results/csv/families and predictors/study families.csv", stringsAsFactors = F)[1:22, 1]
  
# linear vs. quadratic model selection -----------------------------------------------
# compare and store best model for each native family
  model_ls <- list()
  best_model <- matrix(nrow = length(names),
                         ncol = 1)

  r1 <- seq(1, length(names), 2)
  r2 <- seq(2, length(names), 2)
  lq2 <- matrix(nrow = length(names),
                ncol = 6)
  
  for (i in 1:length(names)){
    
    model_ls[[i]] <- as.matrix(model.sel(qm[[i]], lm[[i]]))  # store all models
    m_wgt <- data.frame(model.sel(qm[[i]], lm[[i]]))
    lq2[r1[i]:r2[i], 2:6] <- as.matrix(m_wgt[, c("logLik", "df", "AICc", "delta", "weight")])
    lq2[r1[i]:r2[i], 1] <- row.names(model.sel(qm[[i]], lm[[i]]))
    } # ignore warning
  
  names(model_ls) <- names22
  model_ls[[2]]
  model_ls[[1]]
  
  lq3 <- data.frame(rep(names22, each = 2), lq2)
  colnames(lq3) <- c( "family", "model_type", "logLik", "df", "AICc", "delta", "weight")
  head(lq3)
  
# quandratic model support if quadratic is better (delta == 0) or linear model has delta <10  
  ms_mat <- lq3 %>% 
            mutate(delta = as.numeric(delta)) %>%
            filter((model_type == "qm[[i]]" & delta == 0) | 
                   (model_type == "lm[[i]]" & between(delta, 0, 10))) %>%
            filter(family != "Juncaceae" | model_type == "lm[[i]]",         # quadratic delta < 10
                   family != "Cyperaceae" | model_type == "lm[[i]]") %>%    # quadratic delta < 10
            mutate(ms = if_else(model_type == "qm[[i]]", "quadratic", "linear"))
  
  ms_mat

# model selection R2 -------------------------------------------  
  ms_r2 <- ms_mat %>% 
           dplyr::select(family, ms) %>% 
           left_join(., lr2, by = "family") %>%
           left_join(., qr2, by = "family")
  ms_r2_meta <- ms_r2
  
  ms_r2$ms_nat <- ifelse(ms_r2$ms == "quadratic", ms_r2$qua_nat, ms_r2$lin_nat)
  ms_r2$ms_nonnat <- ifelse(ms_r2$ms == "quadratic", ms_r2$qua_nn_nn, ms_r2$lin_nn_nn)
  
# tidy  
  ms_r2.1 <- ms_r2 %>% 
             dplyr::select(family, ms_nat, ms_nonnat) %>%
             rename(nat_r2 = ms_nat,
                    nonnat_r2 = ms_nonnat)
  ms_r2.2 <- ms_r2.1 %>% mutate(nat_r2 = sprintf("%.3f", nat_r2, 3),
                                nonnat_r2 = sprintf("%.3f", nonnat_r2, 3))  
  ms_r2.2

# save ----------------------------------------------------------
  write.csv(lq3, "Results/csv/modelling/linear vs quadratic model selection.csv", row.names = F)
  write.csv(ms_r2_meta, "Results/csv/modelling/linear vs quadratic r2.csv", row.names = F)
  saveRDS(model_ls, "Data files/Rdata/linear vs quadratic model selection.RDS")
  write.csv(ms_r2.2, "Results/csv/correlates/model selection r2.csv", row.names = F) 
  
# ---------------------------------------------------------------  