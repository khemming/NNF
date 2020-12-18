

# library -----------------------------------------------------
  library(raster)
  library(tidyverse)
  
  rm(list = ls())
  
# data --------------------------------------------------------
# number of records and species  
  summary <-  read.csv("Results/csv/family summary statistics/study families.csv", stringsAsFactors = F)[1:22, ]  
  
# linear and quadratic r2
  lr2 <- read.csv("Results/csv/NNFC correlates/linear r2.csv")
  qr2 <- read.csv("Results/csv/NNFC correlates/quadratic r2.csv")
  r2 <- left_join(lr2, qr2, by = "family")

# native-nonntive correlation
  nnfc <- read.csv("Results/csv/NNFC correlates/model selection r2.csv")
  
# cells occupied
  co <- read.csv("Results/csv/NNFC correlates/cell occupation.csv")
  
# arrival time
  fo <- read.csv("Results/csv/family summary statistics/arrival times.csv")
  
# add all together
  dat <- bind_cols(nnfc, r2, summary, co, fo) %>%
           dplyr::select(-family1, -family2, -family3, -family4)
  head(dat)

# how do r2 go with nnfc? ------------------------------------
  plot(correlation ~ ms_nat, dat)
  cor(dat$correlation, dat$ms_nat, method = "pearson", use = "complete.obs")
#$ not well
  
# model 1 ------------------------------------------------------
# native-nonnat6ive family correlation       =                                       =
# model selection R2                         +
# diffrence in proportion of cells occupied  +
# difference in first occurrence             +
# world-wide species                         +   <- have not got this info yet.
# number of nonnative records
  m1_fc <- data.frame(nnfc = dat$correlation)
  
  m1_dat <- dat %>% 
            mutate(r2 = dat$ms_nat,
                   co = dat$prop_15,
                   fo = dat$nonnat_fo,
                   nn_rec = log(dat$nonnative_rec)) %>%
       dplyr::select(family, correlation, r2, co, fo, nn_rec)
    
  
  cor1 <- round(cor(m1_dat[, 2:6], method = "pearson", use = "complete.obs"), 2)
  cor1  # nn_rec and co correlate strongly 
  
# model  
  m1 <- lm(correlation ~ r2 + co + fo + nn_rec, data = m1_dat)
  sum1 <- summary(m1)
  sum1                                                                  

# summary as table
  m1_coef <- data.frame(sum1$coefficients)
  m1_coef[] <- lapply(m1_coef, sprintf, fmt = "%.3f")
  m1_coef
  m1_df <- data.frame(parameter = rownames(m1_coef),
                      m1_coef)
  m1_df
  
  write.csv(m1_df, "Results/csv/NNFC correlates/model 1 summary table.csv", row.names = F)
  
# checks
  plot(correlation ~ r2, m1_dat)
  plot(correlation ~ nn_rec, m1_dat)
  plot(correlation ~ fo, m1_dat)
  
# test 2 ------------------------------------------------------
# changing FO to nonnative absolute FO
# NNFC                                       =
# native vs R2                               +
# native-nonntive co 15 rec proportion       +
# nonnative fo                               + 
# difference in native-nonnative species     +
# number of nonnative records
  test <- data.frame(nnfc = nnfc$correlation)
  
  test2 <- test %>% mutate(r2 = r2$lin_nat,
                           co = co$prop_15,
                           fo = fo$nonnat_fo,
                           spp = summary$native_spp - summary$nonnative_spp,
                           nn_rec = summary$nonnative_rec)
  
  cor2 <- round(cor(test1[2:6], method = "pearson", use = "complete.obs"), 2)
  cor2  # nn_rec and co correlate strongly 
  
  
# model  
  m2 <- lm(nnfc ~ r2 + co + fo + spp + nn_rec, data = test2)
  summary(m2)  
  
# -------------------------------------------------------------