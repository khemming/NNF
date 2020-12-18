

# library -------------------------------------------------------------------------
  library(tidyverse)
  library(raster)
  library(janitor)

  
  rm(list = ls())

# data ----------------------------------------------------------------------------
  dat <- readRDS("Data files/ALA/cleaning steps/step 6 - named, filtered and study family records.RDS")
  
# total records and raw species richness ------------------------------------------------
# total 
  tots <- data.frame(matrix(nrow = 1, ncol = 4))
  colnames(tots) <- c("families", "genera", "species", "records")
  tots$families <- nrow(distinct(dat, family)) 
  tots$genera <- nrow(distinct(dat, genus))
  tots$species <- nrow(distinct(dat, species))
  tots$records <- nrow(dat) 

  write.csv(tots, "Results/csv/summary statistics/record and richness total of all families.csv", row.names = F)  
  
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
  
  fam_summary <- left_join(fam_ss, fam_rs, by = "family")
  

# filter study families based off records and richness -----------------------------
# selection criteria:
# >=100 native spp.  
# >=24 nn spp. 
# >= 40k native records
  dat_all <- fam_summary %>% mutate(study_family = ifelse(native_spp >= 90 & 
                                                   nonnative_spp >= 20 & 
                                                   native_rec >= 40000, "yes", "no"))
  table(dat_all$study_family)

# prefix column (three-letter name of family) & raster name (name + status) 
  dat_all <- dat_all %>% mutate(prefix = substr(family, 1, 3))
    
# save 
  write.csv(dat_all, "Results/csv/summary statistics/study families, status, records and richness.csv", row.names = F)
  
# table form  
  table_1 <- dat_all %>% 
    filter(study_family == "yes") %>%
    adorn_totals()
  table_1
  
  write.csv(table_1, "Results/csv/summary statistics/study families.csv", row.names = F)   
  
# master data -------------------------------------------------------------------
# include study family column
  incl_fam <- dat_all %>% 
              ungroup() %>%
       dplyr::select(family, study_family)
  m_dat <- left_join(dat, incl_fam, by = "family")
  
  saveRDS(m_dat, "Data files/ALA/master data/master records.RDS")
  
# --------------------------------------------------------------------------------
  
