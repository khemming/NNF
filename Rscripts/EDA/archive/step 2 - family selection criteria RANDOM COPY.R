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

  # list of familiy/taxa names 
  family_list <- read.csv("Results/csv/summary statistics/family selection list.csv", stringsAsFactors = F) 
  family_list_rep <- rep(family_list$family, each = 2)
  
# data frame --------------------------------------------------------
  stat_rep <- as.numeric(as.character(nrow(family_list))) 
  
  dat <- data.frame(family_long = as.character(family_list_rep),
                    raster_name = as.character(names),
                    status = as.character(rep(c("native", "non.native"), as.numeric(as.character(nrow(family_list))))),
                    prefix = as.character(substr(family_list_rep, 1, 3)), stringsAsFactors = F)
  glimpse(dat)
  
  write.csv(dat, "Results/csv/summary statistics/selected family names table.csv", row.names = F)
  
  # --------------------------------------------------------------