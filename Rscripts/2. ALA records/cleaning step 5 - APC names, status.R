

# library ----------------------------------------------------------------
  library(tidyverse)
  library(data.table)

  rm(list = ls())

# data ---------------------------------------------------------------------
# APC names
  apc <- read.csv("Data files/ALA/cleaning steps/step 3 - APC names & status.csv", strip.white = T) %>%
         dplyr::select(name, naturalised) %>%
         distinct(name, .keep_all = T)    # remove duplicate APC names
  
# ALA records
  ala <- readRDS("Data files/ALA/cleaning steps/step 4 - filtered records.RDS")

# filter ALA species names with APC accepted names ------------------------
# match ALA records with APC species names & status 
  ala$species <- as.factor(ala$species)
  ala_names <- data.frame(species = as.character(levels(ala$species)), ala_accepted = 1) 
  
# apc status (native/nonnative), with naturalised being nonnative
  apc$name <- as.factor(apc$name) 
  apc_names <- data.frame(species = as.character(levels(apc$name)), 
                          apc_accepted = 1, 
                          apc_status = table(apc$name, apc$naturalised)[, 1]) 
  
# match ALA to APC names
  allnames <- merge(ala_names, apc_names, by = "species", all = T)
  allnames <- mutate(allnames, ala_accepted = ifelse(is.na(ala_accepted) == T, 0, 1),
                     apc_accepted = ifelse(is.na(apc_accepted) == T, 0, 1),
                     total = ala_accepted + apc_accepted)
  matching_names <- filter(allnames, total == 2)
  
# filter ALA records for accepted names
  ala$species <- as.character(ala$species)
  apc_names$species <- as.character(apc_names$species)
  ala_apc_names <- filter(ala, ala$species %in% matching_names$species)
  
# assign native and nonnative status ---------------------------------------------
  apc_named_ala_records <- left_join(ala_apc_names, apc_names, by = "species")
  
# checks
  table(apc_named_ala_records$apc_status, exclude = F)
  table(apc_named_ala_records$apc_accepted, exclude = F)
  
# asign useful status names
  apc_named_ala_records2 <- apc_named_ala_records %>% rename(status = apc_status) %>%
                            mutate(status = ifelse(status == 1, "native", "nonnative"))
  table(apc_named_ala_records2$status, exclude = NULL)

# save ---------------------------------------------------------------
  apc_final <- apc_named_ala_records2 %>% dplyr::select(-apc_accepted) 
  
  saveRDS(apc_final, "Data files/ALA/cleaning steps/step 5 - accepted status records.RDS")
  
# -----------------------------------------------------------------------------
  
