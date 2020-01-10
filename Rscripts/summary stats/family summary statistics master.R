############################################################################
# master summary of records and species richnesses for selected higher taxa
############################################################################

# scope --------------------------------------------------------------------
# for the selected taxa, what are their cleaned record numbers, species numbers (total and by status)
# this script adds Plantae and C3 and C4 Poaceae from previous (beta) script

# library -------------------------------------------------------------------------
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------------------
# preliminary records from Plantae data file
  pla <- readRDS("Data files/ALA/master data/Plantae master data.RDS") %>%
         dplyr::select(family, species, status)
  
  fam <- readRDS("Data files/ALA/master data/family master data.RDS") %>%
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
                        rename(native.rec = native,
                               non.native.rec = non.native,
                               total.rec = total)
      
    # species by status
      fam_spp <- fam %>% distinct(family, species) %>%
                         group_by(family) %>%
                         summarise(spp = n()) %>%
                         mutate(status = "total")
      
      fam_ss <- fam %>% distinct(family, species, .keep_all = TRUE) %>%
                        group_by(family, status) %>%
                        summarise(spp = n()) %>%
                        bind_rows(., fam_spp) %>%
                        spread(status, spp) %>%
                        rename(native.spp = native,
                               non.native.spp = non.native,
                               total.spp = total)
      
      fam_summary <<- left_join(fam_ss, fam_rs, by = "family")
      
      return(fam_summary)
  }
  summary_family(fam)
  
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
                rename(native.rec = native,
                       non.native.rec = non.native,
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
                rename(native.spp = native,
                       non.native.spp = non.native,
                       total.spp = total)
    
    king_summary <<- left_join(king_spp, king_rec, by = "family")
    
    return(king_summary)
    
  } 
  summary_kingdom(pla, "Plantae")
  
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
                rename(native.rec = native,
                       non.native.rec = non.native,
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
                rename(native.spp = native,
                       non.native.spp = non.native,
                       total.spp = total)
    
    C3_summary <<- left_join(pp_spp, pp_rec, by = "family")
    
    return(C3_summary)
  }
  summary_C3(fam, "C3")
  
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
      rename(native.rec = native,
             non.native.rec = non.native,
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
      rename(native.spp = native,
             non.native.spp = non.native,
             total.spp = total)
    
    C4_summary <<- left_join(pp_spp, pp_rec, by = "family")
    
    return(C4_summary)
  }
  summary_C4(fam, "C4")
  
# combine & save -----------------------------------------------------------------
  dat_all <- bind_rows(fam_summary, king_summary, C3_summary, C4_summary) %>%
             arrange(family)
  
  write.csv(dat_all, "Results/csv/summary statistics/species and records summary master.csv", row.names = F)
  
# ----------------------------------------------------
  
