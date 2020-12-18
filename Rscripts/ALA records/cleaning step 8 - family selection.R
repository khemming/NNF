

# library -------------------------------------------------------------------------
  library(raster)  
  library(tidyverse)
  library(janitor)

  rm(list = ls())

# data ----------------------------------------------------------------------------
  dat <- readRDS("Data files/ALA/cleaning steps/step 6 - pp family records.RDS")
  
# family rerords and richness --------------------------------------------------------
# records and richness for each family and each of Poaceae's two photosynthetic pathways
# records by status
  fam_r <- dat %>% 
           dplyr::select(-status) %>%
           group_by(family) %>%
           summarise(records = n()) %>%
           mutate(status = "total")
  
  fam_rs <- dat %>% 
            group_by(family, status) %>%
            summarise(records = n()) %>%
            bind_rows(., fam_r) %>% 
            spread(status, records) %>%
            rename(native_rec = native,
                   nonnative_rec = nonnative,
                   total_rec = total)
  
# species by status
  fam_spp <- dat %>% 
             distinct(family, species) %>%
             group_by(family) %>%
             summarise(spp = n()) %>%
             mutate(status = "total")
  
  fam_ss <- dat %>% 
            distinct(family, species, .keep_all = TRUE) %>%
            group_by(family, status) %>%
            summarise(spp = n()) %>%
            bind_rows(., fam_spp) %>%
            spread(status, spp) %>%
            rename(native_spp = native,
                   nonnative_spp = nonnative,
                   total_spp = total)
  
  dat2 <- left_join(fam_ss, fam_rs, by = "family")
  

# filter study families based off records and richness -----------------------------
# selection criteria:
# >=100 native spp.  
# >=24 nn spp. 
# >= 40k native records
  table1 <- dat2 %>% 
          mutate(study_family = ifelse(native_spp >= 50 & 
                                       nonnative_spp >= 20 & 
                                       native_rec >= 20000, "yes", "no")) %>%
    adorn_totals()
  table(table1$study_family)
  tail(table1)

# save 
  write.csv(table1, "Results/csv/family summary statistics/all families.csv", row.names = F)
  
# study families only
# added prefix column
  table2 <- table1 %>% 
    filter(study_family == "yes") %>%
    adorn_totals()
  table2
  
# prefix column
  table3 <- table2 %>% mutate(prefix = substr(family, 1, 3))
  
  table3$prefix[c(18, 19)] <- c("C3", "C4") 
  table3
  
  
  write.csv(table3, "Results/csv/family summary statistics/study families.csv", row.names = F)   
  
# include study family column
  incl_fam <- table1 %>% 
              ungroup() %>%
       dplyr::select(family, study_family)
  dat_master <- left_join(dat, incl_fam, by = "family") %>%
           droplevels()
  head(dat_master)

# save master data  
  saveRDS(dat_master, "Data files/ALA/master data/master records.RDS")
  
# --------------------------------------------------------------------------------
  
