
# library ------------------------------------------------------
  library(tidyverse)
# data ---------------------------------------------------------
# correlates
  dat <- read.csv("Results/csv/correlates/correlates.csv") %>%
         dplyr::select(family, correlation, nat_r2, ww_spp, nn_rec, nonnat_fo, prop_15)


# model ------------------------------------------------------
# native-nonnative correlation with:
# nat_r2 = native R2                                      
# ww_spp = worldwide species (log)
# nn_rec = nonnative records (log)
# nonnat_fo = mean family first nonnative occurrence  
# prop_15 = diffrence in proportion of cells occupied (with at elast fifteen records)  

  dat_cor <- round(cor(dat[, 2:7], method = "pearson", use = "complete.obs"), 2)
  dat_cor 

# model  
  m1 <- lm(correlation ~ nat_r2 + ww_spp + nn_rec + nonnat_fo + prop_15, data = dat)
  summary(m1)

# save table ---------------------------------------------------
# summary as table
  sum <- summary(m1)
  m1_coef <- data.frame(sum$coefficients)
  m1_coef[] <- lapply(m1_coef, sprintf, fmt = "%.3f")
  m1_coef
  m1_df <- data.frame(parameter = rownames(m1_coef),
                      m1_coef) %>%
           rename(se = Std..Error,
                  p_value = Pr...t..)
  m1_df
  
  write.csv(m1_df, "Results/csv/correlates/model summary.csv", row.names = F)
  