###############################################################
# date of first occurrence of non-native species
###############################################################

# library -----------------------------------------------------
  library(tidyverse)
  rm(list = ls())

# data --------------------------------------------------------
# record data
  dat <- readRDS("Data files/ALA/master data/master data.RDS") %>%
          filter(status == "non_native")

# required families
  names <- read.csv("Results/csv/summary statistics/master family summary table.csv", stringsAsFactors = F) %>%
    filter(study_family == "yes")
  
# mean first record of occurrence ----------------------------
# famlies
  for (i in 1:nrow(names)){
    
    fo_fam <- dat %>% ungroup() %>%
               filter(family == names$family[i]) %>%
               group_by(species, year) %>%
               arrange(.by_group = T) %>%
               ungroup() %>%
               distinct(species, .keep_all = T)
    
    names$first_occ[i] <- ceiling(mean(fo_fam$year))
  
  }
  
# C3/C4
  C3_fo <- dat %>% ungroup() %>%
            filter(pp == "C3") %>%
            group_by(species, year) %>%
            arrange(.by_group = T) %>%
            ungroup() %>%
            distinct(species, .keep_all = T)
  names[names$family == "C3", "first_occ"] <- ceiling(mean(C3_fo$year))
  
  C4_fo <- dat %>% ungroup() %>%
            filter(pp == "C4") %>%
            group_by(species, year) %>%
            arrange(.by_group = T) %>%
            ungroup() %>%
            distinct(species, .keep_all = T)
  names[names$family == "C4", "first_occ"] <- ceiling(mean(C4_fo$year))
    
  names_final <- names %>% dplyr::select(family, first_occ)
  
# save 
  write.csv(names_final, "Results/csv/summary statistics/date of first occurrence.csv", row.names = F)

# -------------------------------------------------------------
  
  
  
  
  
  