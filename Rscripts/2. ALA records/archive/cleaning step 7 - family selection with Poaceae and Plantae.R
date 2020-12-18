

# scope --------------------------------------------------------------------
# select families with enough records/species 

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
# records and richness for each family, plantae kingdom, and each of Poaceae's two photosynthetic pathways
# family 
  summary_family <- function(dat){
    
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
      
      fam_summary <<- left_join(fam_ss, fam_rs, by = "family")
      
      return(fam_summary)
  } # end fun
  
# run family function
  summary_family(dat)
  
# kingdom 
  summary_kingdom <- function(dat, kingdom){
  # record summary by total then by status
    king_r <- dat %>% 
              summarise(records = n()) %>%
              mutate(family = "Plantae",
                     status = "total") %>%
       dplyr::select(family, status, records)
    
      king_rs <- dat %>% 
                 group_by(status) %>%
                 summarise(records = n()) %>%
                 mutate(family = "Plantae") %>%
          dplyr::select(family, status, records) 
    
    king_rec <- bind_rows(king_r, king_rs) %>%
                spread(status, records) %>%
                rename(native_rec = native,
                       nonnative_rec = nonnative,
                       total_rec = total)
    
  # species summary by total then by status
    king_s <- dat %>% 
              distinct(species) %>%
              summarise(species = n()) %>%
              mutate(family = "Plantae",
                     status = "total") %>%
       dplyr::select(family, status, species)
    
    king_ss <- dat %>% 
               distinct(species, .keep_all = T) %>%
               group_by(status) %>%
               summarise(species = n()) %>%
               mutate(family = "Plantae") %>%
        dplyr::select(family, status, species) 
    
    king_spp <- bind_rows(king_s, king_ss) %>%
                spread(status, species) %>%
                rename(native_spp = native,
                       nonnative_spp = nonnative,
                       total_spp = total)
    
    king_summary <<- left_join(king_spp, king_rec, by = "family")
    
    return(king_summary)
    
  } # end fun
  
# run kingdom function
  summary_kingdom(dat, "Plantae")
  
# Poaceae photosynthetic pathways 
  summary_C3 <- function(dat, pp){
    # record summary by total then by status
      dat_pp <- dat %>% filter(family == "Poaceae",
                           pp == pp) %>%
                           mutate(family = pp)  
    
      pp_r <- dat_pp %>% summarise(records = n()) %>%
              mutate(family = pp,
                     status = "total") %>%
              dplyr::select(family, status, records)
      
      pp_rs <- dat_pp %>% group_by(status) %>%
               summarise(records = n()) %>%
               mutate(family = pp) %>%
               dplyr::select(family, status, records) 
      
      pp_rec <- bind_rows(pp_r, pp_rs) %>%
                spread(status, records) %>%
                rename(native_rec = native,
                       nonnative_rec = nonnative,
                       total_rec = total)
    
    # species summary by total then by status
      pp_s <- dat_pp %>% distinct(species) %>%
              summarise(species = n()) %>%
              mutate(family = pp,
                     status = "total") %>%
              dplyr::select(family, status, species)
      
      pp_ss <- dat_pp %>% distinct(species, .keep_all = T) %>%
               group_by(status) %>%
               summarise(species = n()) %>%
               mutate(family = pp) %>%
               dplyr::select(family, status, species) 
      
      pp_spp <- bind_rows(pp_s, pp_ss) %>%
                spread(status, species) %>%
                rename(native_spp = native,
                       nonnative_spp = nonnative,
                       total_spp = total)
    
    C3_summary <<- left_join(pp_spp, pp_rec, by = "family")
    
    return(C3_summary)
  }
  summary_C3(dat, "C3")
  
  summary_C4 <- function(dat, pp){
  # record summary by total then by status
    dat_pp <- dat %>% filter(family == "Poaceae",
                         pp == pp) %>%
      mutate(family = pp)  
    
    pp_r <- dat_pp %>% summarise(records = n()) %>%
      mutate(family = pp,
             status = "total") %>%
      dplyr::select(family, status, records)
    
    pp_rs <- dat_pp %>% group_by(status) %>%
      summarise(records = n()) %>%
      mutate(family = pp) %>%
      dplyr::select(family, status, records) 
    
    pp_rec <- bind_rows(pp_r, pp_rs) %>%
      spread(status, records) %>%
      rename(native_rec = native,
             nonnative_rec = nonnative,
             total_rec = total)
    
  # species summary by total then by status
    pp_s <- dat_pp %>% distinct(species) %>%
      summarise(species = n()) %>%
      mutate(family = pp,
             status = "total") %>%
      dplyr::select(family, status, species)
    
    pp_ss <- dat_pp %>% distinct(species, .keep_all = T) %>%
      group_by(status) %>%
      summarise(species = n()) %>%
      mutate(family = pp) %>%
      dplyr::select(family, status, species) 
    
    pp_spp <- bind_rows(pp_s, pp_ss) %>%
      spread(status, species) %>%
      rename(native_spp = native,
             nonnative_spp = nonnative,
             total_spp = total)
    
    C4_summary <<- left_join(pp_spp, pp_rec, by = "family")
    
    return(C4_summary)
  }
  summary_C4(dat, "C4")
  
# save
  dat_all <- bind_rows(fam_summary, king_summary, C3_summary, C4_summary) %>%
             arrange(family) %>%
             mutate_at(vars(-family), ~replace(., is.na(.), 0))
  
# filter study families based off records and richness -----------------------------
# selection criteria:
# >=100 native spp.  
# >=24 nn spp. 
# >= 40k native records
  dat_all <- dat_all %>% mutate(study_family = ifelse(native_spp >= 90 & 
                                               nonnative_spp >= 24 & 
                                               native_rec >= 40000, "yes", "no"))
  table(dat_all$study_family)

# prefix column (three-letter name of family) & raster name (name + status) 
  dat_all <- dat_all %>% mutate(prefix = substr(family, 1, 3))
    
# save 
  write.csv(dat_all, "Results/csv/summary statistics/study families, status, records and richness.csv")
  
# table form  
  table_1 <- dat_all %>% 
    filter(study_family == "yes") %>%
    adorn_totals()
  table_1
  
  write.csv(table_1, "Results/csv/summary statistics/record and richness of study families.csv", row.names = F)   
  
# master data -------------------------------------------------------------------
# include study family column
  incl_fam <- dat_all %>% 
              ungroup() %>%
       dplyr::select(family, study_family)
  m_dat <- left_join(dat, incl_fam, by = "family")
  
  write.csv(m_dat, "Data files/ALA/master data/master data.csv")
  
# --------------------------------------------------------------------------------
  
