############################################################################
# Preliminary summary of reords and species richness for all families
############################################################################

# scope --------------------------------------------------------------------
# which families meet the minimum species richness and record criteria to be included in the study? Let's get an idea.
# preliminary statistics because using Plantae records (which are a different data set to how I pull out each families' records from ALA4R package)

# library -------------------------------------------------------------------------
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------------------
# preliminary records from Plantae data file
  dat <- readRDS("Data files/ALA/master data/Plantae master data.RDS") %>%
         dplyr::select(family, species, status)

# beta records and raw species richness --------------------------------------------- # derived from Plantae data  
# records
  rec.fam <- dat %>% dplyr::select(-status) %>%
                      group_by(family) %>%
                      summarise(records = n()) %>%
                      mutate(status = "total")
  
  rec.status <- dat %>% group_by(family, status) %>%
                        summarise(records = n()) %>%
                        bind_rows(., rec.fam) %>% 
                        spread(status, records) %>%
                        replace_na(list(native = 0, "non-native" = 0, total = 0)) 
  rec.status
  
# species
  spp.fam <- dat %>% distinct(family, species) %>%
                     group_by(family) %>%
                     summarise(spp = n()) %>%
                     mutate(status = "total")
                     
  spp.status <- dat %>% distinct(family, species, .keep_all = TRUE) %>%
                        group_by(family, status) %>%
                        summarise(spp = n()) %>%
                        bind_rows(., spp.fam) %>%
                        spread(status, spp) %>%
                        replace_na(list(native = 0, non.native = 0, total = 0)) 
  spp.status
  
# combine
  colnames(spp.status) <- c("family", "native.spp", "nn.spp", "total.spp")
  colnames(rec.status) <- c("family", "native.rec", "nn.rec", "total.rec")
  
  dat.all <- left_join(spp.status, rec.status, by = "family")
  
  write.csv(dat.all, "Results/csv/summary statistics/species and records beta.csv", row.names = F)
  
# ----------------------------------------------------
  
  
  
  
  
  
    
  