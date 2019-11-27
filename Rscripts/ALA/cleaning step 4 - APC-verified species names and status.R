########################################################
# cleaning step 4: APC-verified species names and status
########################################################

# scope ------------------------------------------------------------------
# update species names and origin (native/non-native) based on APC species list

# library ----------------------------------------------------------------
  library(tidyverse)
  library(data.table)

  rm(list = ls())

# data ---------------------------------------------------------------------
  ala <- readRDS("Data files/ALA/cleaning step 3 - cleaned records.RDS")

# APC names
  apc <- read.csv("Data files/ALA/cleaning step 2 - ALA names & status.csv", strip.white = T) %>%
    dplyr::select(name, naturalised)
  
# filter ALA species names with APC accepted names ------------------------
# match ALA records with APC species names & status 
  ala$species <- as.factor(ala$species)
  ala.names <- data.frame(species = as.character(levels(ala$species)), ala.accepted = 1) 
  
# remove duplicate APC names
  apc.dup <- distinct(apc, name, .keep_all = T)
  
# apc status (native/non-native), with naturalised being non-native
  apc$name <- as.factor(apc$name) 
  apc.names <- data.frame(species = as.character(levels(apc.dup$name)), 
                          apc.accepted = 1, 
                          apc.status = table(apc.dup$name, apc.dup$naturalised)[, 1]) 
  
# match ALA to APC names
  allnames <- merge(ala.names, apc.names, by = "species", all = T)
  allnames <- mutate(allnames, ala.accepted = ifelse(is.na(ala.accepted) == T, 0, 1),
                     apc.accepted = ifelse(is.na(apc.accepted) == T, 0, 1),
                     total = ala.accepted + apc.accepted)
  matching.names <- filter(allnames, total == 2)
  
# filter ALA records for accepted names
  ala$species <- as.character(ala$species)
  apc.names$species <- as.character(apc.names$species)
  ala.apc.names <- filter(ala, ala$species %in% matching.names$species)
  
# assign native and non-native status ---------------------------------------------
  apc.named.ala.records <- left_join(ala.apc.names, apc.names, by = "species")
  
# checks
  table(apc.named.ala.records$apc.status, exclude = F)
  table(apc.named.ala.records$apc.accepted, exclude = F)
  
# asign meaningful names
  apc.named.ala.records2 <- apc.named.ala.records %>% rename(status = apc.status) %>%
                            mutate(status = ifelse(status == 1, "native", "non-native"))
  table(apc.named.ala.records2$status, exclude = NULL)

# save ---------------------------------------------------------------
  apc.final <- apc.named.ala.records2 %>% dplyr::select(-apc.accepted) 
  
  saveRDS(apc.final, "Data files/ALA/master invasive families data.RDS")
  
# -----------------------------------------------------------------------------
  
