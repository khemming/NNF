#####################################################
# step 8 - families that meet the selection criteria
#####################################################

# scope ---------------------------------------------
# which families meet the minimum required data to be analysed?

# library -------------------------------------------
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------
# summary table based off Plantae downloaded records
  summary <- read.csv("Results/csv/summary statistics/species and records summary master.csv")
  glimpse(summary)

# filter data ---------------------------------------  
# filters:
# >=100 native spp.  
# >=24 nn spp. 
# >= 40k native records
  fams <- summary %>% filter(native.spp >= 100,
                             non.native.spp >= 24,
                             native.rec >= 40000) %>%
                      dplyr::select(family)
# save
  write.csv(fams, "Results/csv/summary statistics/family selection list.csv", row.names = F)

# -----------------------------------------------------