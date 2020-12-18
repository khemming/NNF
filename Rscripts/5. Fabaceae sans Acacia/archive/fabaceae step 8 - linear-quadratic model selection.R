

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
  lr2 <- read.csv("Results/csv/correlates/linear r2.csv") 
  head(lr2)
  qr2 <- read.csv("Results/csv/correlates/quadratic r2.csv")  
  head(qr2)  
  
# short family names  
  names22 <- read.csv("Results/csv/family summary statistics/study families.csv", stringsAsFactors = F)[1:22, 1]
  
# model selection -----------------------------------------------
# compare best linear vs. quadratic model for each native family species richness
# store all and best linear vs. quadratic model selection results
  lq_models <- list()
  best_model <- matrix(nrow = length(names),
                         ncol = 1)
# store row 1 and 2  
  r1 <- seq(1, length(names), 2)
  r2 <- seq(2, length(names), 2)
  lq2 <- matrix(nrow = length(names),
                ncol = 5)
  
  for (i in 1:length(names)){
    
    lq_models[[i]] <- as.matrix(model.sel(qm[[i]], lm[[i]]))  # store all models
    m_wgt <- data.frame(model.sel(qm[[i]], lm[[i]]))
    lq2[r1[i]:r2[i], 2:5] <- as.matrix(m_wgt[, c("logLik", "AICc", "delta", "weight")])
    lq2[r1[i]:r2[i], 1] <- row.names(model.sel(qm[[i]], lm[[i]]))
    }
  
  names(lq_models) <- names22
  lq_models[[2]]
  lq_models[[1]]
  
  lq3 <- data.frame(rep(names22, each = 2), lq2)
  colnames(lq3) <- c( "family", "model_type", "logLik", "AICc", "delta", "weight")
  head(lq3)
  
  lq4 <- lq3 %>% slice(r2) 
  lq4$model_supported <- ifelse(lq4$model_type == "qm[[i]]" & lq4$delta >= 10, "quadratic", "linear")
  head(lq4)
  
# model selection R2 -------------------------------------------  
  ms_r2 <- lq4 %>% 
           select(family, model_supported) %>% 
           left_join(., lr2, by = "family") %>%
           left_join(., qr2, by = "family")
  
  ms_r2$ms_nat <- ifelse(ms_r2$model_supported == "quadratic", ms_r2$qua_nat, ms_r2$lin_nat)
  ms_r2$ms_nonnat <- ifelse(ms_r2$model_supported == "quadratic", ms_r2$qua_nn_nn, ms_r2$lin_nn_nn)
  
  ms_r2.1 <- ms_r2 %>% 
             select(family, ms_nat, ms_nonnat) %>%
             rename(nat_r2 = ms_nat,
                    nonnat_r2 = ms_nonnat)
  ms_r2.2 <- ms_r2.1 %>% mutate(nat_r2 = sprintf("%.3f", nat_r2, 3),
                                nonnat_r2 = sprintf("%.3f", nonnat_r2, 3))  

# save ----------------------------------------------------------
  write.csv(lq3, "Results/csv/supplementary materials/linear vs quadratic model selection.csv", row.names = F)   
  saveRDS(lq_models, "Data files/Rdata/linear vs quadratic model selection.RDS")
  
  write.csv(ms_r2.2, "Results/csv/correlates/model selection r2.csv", row.names = F) 
  
# ---------------------------------------------------------------  