############################################################################
# master summary of records and species richnesses for higher taxa
############################################################################

# library -------------------------------------------------------------------------
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------------------
  dat <- readRDS("Data files/ALA/master data/master data.RDS") %>%
         dplyr::select(family, species, status, pp)
  
# records and raw species richness ------------------------------------------------
# for each family, plantae kingdom, and each of Poaceae's two photosynthetic pathways
  summary_family <- function(dat){
    
    # records by status
      fam_r <- dat %>% dplyr::select(-status) %>%
                      group_by(family) %>%
                      summarise(records = n()) %>%
                      mutate(status = "total")
      
      fam_rs <- dat %>% group_by(family, status) %>%
                        summarise(records = n()) %>%
                        bind_rows(., fam_r) %>% 
                        spread(status, records) %>%
                        rename(native_rec = native,
                               non_native_rec = non_native,
                               total.rec = total)
      
    # species by status
      fam_spp <- dat %>% distinct(family, species) %>%
                         group_by(family) %>%
                         summarise(spp = n()) %>%
                         mutate(status = "total")
      
      fam_ss <- dat %>% distinct(family, species, .keep_all = TRUE) %>%
                        group_by(family, status) %>%
                        summarise(spp = n()) %>%
                        bind_rows(., fam_spp) %>%
                        spread(status, spp) %>%
                        rename(native_pp = native,
                               non_native_pp = non_native,
                               total.spp = total)
      
      fam_summary <<- left_join(fam_ss, fam_rs, by = "family")
      
      return(fam_summary)
  }
  summary_family(dat)
  
  summary_kingdom <- function(dat, kingdom){
  # record summary by total then by status
    king_r <- dat %>% summarise(records = n()) %>%
      mutate(family = "Plantae",
             status = "total") %>%
      dplyr::select(family, status, records)
    
    king_rs <- dat %>% group_by(status) %>%
      summarise(records = n()) %>%
      mutate(family = "Plantae") %>%
      dplyr::select(family, status, records) 
    
    king_rec <- bind_rows(king_r, king_rs) %>%
                spread(status, records) %>%
                rename(native_rec = native,
                       non_native_rec = non_native,
                       total.rec = total)
    
    # species summary by total then by status
    king_s <- dat %>% distinct(species) %>%
      summarise(species = n()) %>%
      mutate(family = "Plantae",
             status = "total") %>%
      dplyr::select(family, status, species)
    
    king_ss <- dat %>% distinct(species, .keep_all = T) %>%
      group_by(status) %>%
      summarise(species = n()) %>%
      mutate(family = "Plantae") %>%
      dplyr::select(family, status, species) 
    
    king_spp <- bind_rows(king_s, king_ss) %>%
                spread(status, species) %>%
                rename(native_pp = native,
                       non_native_pp = non_native,
                       total.spp = total)
    
    king_summary <<- left_join(king_spp, king_rec, by = "family")
    
    return(king_summary)
    
  } 
  summary_kingdom(dat, "Plantae")
  
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
                       non_native_rec = non_native,
                       total.rec = total)
    
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
                rename(native_pp = native,
                       non_native_pp = non_native,
                       total.spp = total)
    
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
             non_native_rec = non_native,
             total.rec = total)
    
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
      rename(native_pp = native,
             non_native_pp = non_native,
             total.spp = total)
    
    C4_summary <<- left_join(pp_spp, pp_rec, by = "family")
    
    return(C4_summary)
  }
  summary_C4(dat, "C4")
  
  dat_all <- bind_rows(fam_summary, king_summary, C3_summary, C4_summary) %>%
    arrange(family) %>%
    mutate_all(~replace(., is.na(.), 0))
  
# filter for study families --------------------------------------------------------
# included in selection criteria or not
# filters:
# >=100 native spp.  
# >=24 nn spp. 
# >= 40k native records
  dat_all %>% mutate(study_family = ifelse(native_pp >= 100 & 
                                           non_native_pp >= 24 & 
                                           native_rec >= 40000, "yes", "no"))
  
  fams <- summary %>% filter(native_pp >= 100,
                             non_native_pp >= 24,
                             native_rec >= 40000) %>%
    dplyr::select(family)
  
# adding relevant columns --------------------------------------------------------
# prefix column (three-letter name of family)  
  
# raster name (family name + status)

  
# save -----------------------------------------------------------------
  write.csv(dat_final, "Results/csv/summary statistics/master family summary table.csv", row.names = F)
  
# --------------------------------------------------------------------------------
  
