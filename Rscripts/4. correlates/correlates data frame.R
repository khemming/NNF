

# library -----------------------------------------------------------
  library(tidyverse)
  
  rm(list = ls())
  
# data --------------------------------------------------------------
  names22 <- read.csv("Results/csv/families and predictors/study families.csv")[1:22, 1]
  fam <- read.csv("Results/csv/families and predictors/families 1156.csv")
# R2  
  r2 <- read.csv("Results/csv/correlates/model selection R2.csv")
# worldwide species  
  ww_spp <- read.csv("Results/csv/correlates/worldwide spp gen pp.csv")
# number of cells occupied  
  cell_occ <- read.csv("Results/csv/correlates/cell occupation.csv")
# family mean date of first nonnative record
  fo <- read.csv("Results/csv/correlates/first occurrence.csv")
# number of nonnative records
  rec <- read.csv("Results/csv/families and predictors/study families.csv", stringsAsFactors = F) %>%
    filter(study_family == "yes") %>%
    mutate(nn_rec = log(nonnative_rec)) %>%
    dplyr::select(family, nn_rec) 
 
# correlates ---------------------------------------------------------   
# step (1) native-nonnative correlation
# native familires: select every second column starting from 1
  nat <- fam %>% dplyr::select(seq(1, ncol(fam), 2))
# nonnative: start from 2
  nonnat <- fam %>% dplyr::select(seq(2, ncol(fam), 2))
  
# correlate  
  dat1 <- matrix(nrow = length(names22),
                     ncol = 2)
  dat1[, 1] <- names22
  
  for (i in 1:length(names22)){
    
    dat1[i, 2] <- cor(nat[i], nonnat[i], use = "complete.obs", method = "spearman")
    
  }  
  
# tidy
  dat1 <- data.frame(dat1)
  colnames(dat1) <- c("family", "correlation")
  dat1$correlation <- as.numeric(dat1$correlation)
  dat1$correlation <- sprintf("%.3f", dat1$correlation, 3)
  
# step (2) native and nonnative R2
  dat2 <- left_join(dat1, r2, by = "family")
 
# step (3) worldwide species (log-scaled
  dat3 <- dat2 %>% mutate(ww_spp = log(ww_spp$ww_spp))
          
# step (4) nonnative records (log-scaled
  dat4 <- left_join(dat3, rec, by = "family")
  
# step (5) arrival time
  dat5 <- left_join(dat4, fo, by = "family") 
  
# step (6) difference in native-nonnative extent
  dat6 <- left_join(dat5, cell_occ, my = "family")
  
# save
  write.csv(dat6, "Results/csv/correlates/correlates.csv", row.names = F)
  
# ----------------------------------------------------------------