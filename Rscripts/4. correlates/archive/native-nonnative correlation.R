

# library -----------------------------------------------------------
  library(tidyverse)

  rm(list = ls())

# data --------------------------------------------------------------
# families
  fam <- read.csv("Results/csv/families and predictors/families 1156.csv")

# 22 families
  names22 <- read.csv("Results/csv/families and predictors/study families.csv", stringsAsFactors = F)[1:22, 1]
  glimpse(names22)
  
  
# correlation matrix ------------------------------------------------
# native familires: select every second column starting from 1
  nat <- fam %>% dplyr::select(seq(1, ncol(fam), 2))
# nonnative: start from 2
  nonnat <- fam %>% dplyr::select(seq(2, ncol(fam), 2))
  
# correlation matrix  
  corr_mat <- matrix(nrow = length(names22),
                     ncol = 2)
  corr_mat[, 1] <- names22
  
  for (i in 1:length(names22)){
    
    corr_mat[i, 2] <- cor(nat[i], nonnat[i], use = "complete.obs", method = "spearman")
    
  }  

# tidy and save  
  corr <- data.frame(corr_mat)
  colnames(corr) <- c("family", "correlation")
  corr$correlation <- as.numeric(corr$correlation)
  corr$correlation <- sprintf("%.3f", corr$correlation, 3)
  write.csv(corr, "Results/csv/correlates/NNF correlation.csv", row.names = F)

# -----------------------------------------------------------------
