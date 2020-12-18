


# library --------------------------------------------------------
  library(raster)
  library(tidyverse)
  
  rm(list = ls())

# notes on assigning pathways ------------------------------------
# Photosynthetic pathways were assigned first using Osborne et al. (2014) and then
# Watson et al. (1992) via their website found here: https://www.delta-intkey.com/grass/index.htm
# for Watson, I went through and added them individually in the cases where Osborne lacked genera information
# for genera or Panicum species which still lacked pp data, I note and exclude these studies
# ----------------------------------------------------------------

# data -----------------------------------------------------------
# Poaceae data
  poa <- readRDS("Data files/ALA/cleaning steps/step 5 - accepted status.RDS") %>% 
         filter(family == "Poaceae") %>% 
         droplevels()
  poa$genus <- as.character(poa$genus)
  poa$species <- as.character(poa$species)
  
# Australia coordinates 
  aus <- raster("Data files/Australia/Australia 1156.grd")
  
# all other records (to join Poaceae to make master data at end)
  families <- readRDS("Data files/ALA/cleaning steps/step 5 - accepted status.RDS") %>% 
    filter(family != "Poaceae") %>%
    droplevels()
  
# Osborne et al. (2014)'s pp data by genus and by species for Panicum 
  os <- read.csv("Data files/Poaceae C3-C4 pathways/c3-c4.csv", fileEncoding = "UTF-8-BOM", header = T)
  colnames(os) <- c("pp", "genus")
  os$genus <- as.character(os$genus)
  table(os$pp) # note there are 'C3 and C4's and 'unknown' categories
  
  os_panicum <- read.csv("Data files/Poaceae C3-C4 pathways/Panicum.csv", fileEncoding = "UTF-8-BOM", header = T) %>%
    dplyr::select(Species, Pathway)
  colnames(os_panicum) <- c("species", "pani_pp")
  head(os_panicum) 
  
# assign Panicum by species --------------------------------------------
# Megathyrsus maxmimus is a synonym for Panicum maximum
  poa[poa$genus == "Megathyrsus", "genus"] <- "Panicum"
  poa[poa$species == "Megathyrsus maximus", "species"] <- "Panicum maximum"  
  
# isolate panicum
  poa_pan <- poa %>% 
             filter(genus == "Panicum") %>%
             dplyr::select(species, genus) %>%
             distinct(species, .keep_all = T) %>%
             droplevels()
  head(poa_pan)
  
# join ala data with Osborne data  
  pan_pp <- left_join(poa_pan, os_panicum, by = "species")
  head(pan_pp)
  
# species with no pathway   
  pan_pp$pani_pp[is.na(pan_pp$pani_pp)] <- "remove"
  head(pan_pp)
  colnames(pan_pp) <- c("species", "genus", "family")
  head(pan_pp)  
  
# inset Panicum records into Poaceae records
# isolate panicum records from poa, remove erroneous family tag, and rename pp as family  
  poa2 <- poa %>% 
          filter(genus != "Panicum") %>%
          dplyr::select(-family) %>%
          droplevels()
  
  panicum_pp <- poa %>% 
                filter(genus == "Panicum") %>%
                dplyr::select(-family)
  
  panicum_pp2 <- left_join(panicum_pp, pan_pp, by = c("species", "genus"))
  
  
# assign pathways by genus using Osborne et al. (2014)------------------------
  gen <- poa2 %>% 
         distinct(genus) %>%
         arrange(genus)
  
  gen2 <- left_join(gen, os, by = "genus")
  
  table(gen2$pp, exclude = NULL) # tidy up these
  
# assign remaining genera using Watson: https://www.delta-intkey.com
# note: there are 23 NAs and a few which are both C3 and C4
# if Watson can't resolve these, we will exclude from study
  gen2_na <- filter(gen2, is.na(gen2$pp) == T)
  gen2_na
  gen2_bothpp <- filter(gen2, pp == "C3 & C4")
  gen2_bothpp
  
# C3
  c3 <- c("Achnatherum",
          "Amelichloa",   
          "Amphibromus",
          "Anthosachne",  
          "Austrostipa",
          "Australopyrum",
          "Avellinia",
          "Chascolytrum",
          "Deyeuxia",
          "Dichanthelium",
          "Elytrigia",
          "Hookerochloa",
          "Jarava",
          "Lachnagrostis",
          "Lophopyrum",
          "Periballia",
          "Microlaena",
          "Molineriella",
          "Saxipoa", # no data on PP; genus recently moved from Poa, which is C3, so assuming that's conserved
          "Sylvipoa", # no data on PP; genus recently moved from Poa, which is C3, so assuming that's conserved
          "Sasaella",
          "Tetrarrhena",
          "Thinopyrum",
          "Walwhalleya",
          "Zotovia")
# C4
  c4 <- c("Aristida",
          "Diplachne",
          "Eragrostis",
          "Moorochloa",
          "Panicum",
          "Pseudopogonatherum",
          "Thellungia",
          "Zuloagaea")
  
# update gen2
  c3_index <- (gen2$genus %in% c3)
  gen2$pp[c3_index]<- "C3"
  
  c4_index <- (gen2$genus %in% c4)
  gen2$pp[c4_index] <- "C4"
  
  gen2$pp[is.na(gen2$pp)] <- "remove"
  
  table(gen2$pp, exclude = NULL)
  
# change pp to family
  gen3 <- gen2 %>%
          rename(family = pp)
  head(gen3)

  
# remove unresolved pathways -------------------------------------------
# merge genera and pathways
  poa3 <- left_join(poa2, gen3, by = "genus")
  head(poa3)
  table(poa3$family, exclude = F)
  
# add in Panicum
  poa4 <- bind_rows(poa3, panicum_pp2)
  table(poa4$family, exclude = F)

# make note of and remove taxa by species
  excl_taxa <- poa4 %>% 
                filter(family == "C3 & C4" |
                       family == "remove") %>%
                distinct(species, genus, .keep_all = T) %>%
                dplyr::select(species, genus, family)
  write.csv(excl_taxa, "Results/csv/excluded genera and species/excluded taxa.csv", row.names = F)
 
# remove taxa and append to other family data frame
  poa5 <- poa4 %>% 
          filter(family != "C3 & C4",
                 family != "remove")
  table(poa5$family, exclude = NULL)
  table(poa5$genus, exclude = NULL)
  
  families2 <- bind_rows(families, poa5)
  table(families2$family, exclude = NULL)
  table(families2$genus, exclude = NULL)[1:10]
  
  families3 <- families2 %>% mutate(family = if_else(family == "C3", "Poaceae_C3", 
                                            if_else(family == "C4", "Poaceae_C4", family)))
  
# save 
  saveRDS(families3, "Data files/ALA/cleaning steps/step 6 - pp family records.RDS")
  
# -------------------------------------------------------------------------    
  