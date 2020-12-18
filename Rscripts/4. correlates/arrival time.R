

# library -----------------------------------------------------
  library(raster)
  library(tidyverse)
  
  rm(list = ls())
  
# data --------------------------------------------------------
# record data
  dat <- readRDS("Data files/ALA/master data/master records.RDS")
  
  fams <-  read.csv("Results/csv/families and predictors/study families.csv", stringsAsFactors = F)[1:22, 1]
  glimpse(fams)
  
# mean first record of occurrence ----------------------------
  fo <- data.frame(family = fams, 
                   nat_fo = NA,
                   nonnat_fo = NA,
                   diff_fo = NA)
  fo

# famlies
  for (i in 1:length(fams)){
    
  # native first occurrence  
    nat <- dat %>% 
           filter(family == fams[i], status == "native") %>%
           group_by(species, year) %>%
           arrange(.by_group = T) %>%
           ungroup() %>%
           distinct(species, .keep_all = T)
    
    fo$nat_fo[i] <- 2020 - ceiling(mean(nat$year))
    
  # nonnative first occurrence  
    nnat <- dat %>% 
      filter(family == fams[i], status == "nonnative") %>%
      group_by(species, year) %>%
      arrange(.by_group = T) %>%
      ungroup() %>%
      distinct(species, .keep_all = T)
    
    fo$nonnat_fo[i] <- 2020 - ceiling(mean(nnat$year))
    
  # difference
    fo$diff_fo[i] <- fo$nat_fo[i] - fo$nonnat_fo[i]
    
  }
  
  fo
  
# save 
  write.csv(fo, "Results/csv/correlates/first occurrence.csv", row.names = F)
  
# -------------------------------------------------------------