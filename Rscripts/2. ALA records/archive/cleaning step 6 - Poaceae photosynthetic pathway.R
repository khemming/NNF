

# scope ----------------------------------------------------------
# assign each Poaceae species with a C3 or C4 pathway
# remove taxa which do not have this data


# library --------------------------------------------------------
  library(tidyverse)
  
  rm(list = ls())

# notes on assigning pathways ------------------------------------
# Photosynthetic pathways were assigned first using Osborne et al. (2014) and then
# Watson et al. (1992) via their website found here: https://www.delta-intkey.com/grass/index.htm
# for Watson, I went through and added them individually in the cases where Osborne lacked genera information
# ----------------------------------------------------------------

# data -----------------------------------------------------------
# Poaceae data
  ala <- readRDS("Data files/ALA/cleaning steps/step 5 - named and filtered records.RDS") %>% 
         filter(family == "Poaceae") %>% 
         filter(!genus %in% c("Steinchisma",      # intermediate C3/C4
                              "Alloteropsis",     # intermediate C3/C4
                              "Neurachne",        # intermediate C3/C4
                              "Cynochloris")) %>% # hybrid
         droplevels()
  
# ------------------------------------------------------------------
# excluded records by pp ------------------------------------------
# pp_excl <- readRDS("Data files/ALA/cleaning steps/step 5 - named and filtered records.RDS") %>%
#   filter(genus %in% c("Steinchisma",     # intermediate C3/C4
#                       "Alloteropsis",    # intermediate C3/C4
#                       "Neurachne",       # intermediate C3/C4
#                       "Cynochloris"))    # hybrid
# ------------------------------------------------------------------  
  
# all other records (to join Poaceae to make master data at end)
  ala_other_families <- readRDS("Data files/ALA/cleaning steps/step 5 - named and filtered records.RDS") %>% 
    filter(family != "Poaceae")
  
  ala$genus <- as.character(ala$genus)
  ala$species <- as.character(ala$species)
  
# Osborne et al. (2014)'s pp data   
  os <- read.csv("Data files/Poaceae C3-C4 pathways/c3-c4.csv", 
                 fileEncoding = "UTF-8-BOM", header = T)
  colnames(os) <- c("pp", "genus")
  os$genus <- as.character(os$genus)
  table(os$pp) # note there are 'C3 and C4's and 'unknown' categories
 
# panicum data (assigned by spp. not genus)
  panicum <- read.csv("Data files/Poaceae C3-C4 pathways/Panicum.csv", fileEncoding = "UTF-8-BOM", header = T) %>%
    dplyr::select(Species, Pathway)
  colnames(panicum) <- c("species", "pani.pp")
  head(panicum) 
  
# assign pathways by genus using Osborne ----------------------------------
  gen <- distinct(ala, genus) %>%
    arrange(., genus)
  
  gen.a <- left_join(gen, os, by = "genus")
 
  table(gen.a$pp, exclude = NULL) # tidy up these
  
# assign remaining genera 
# there are 24 NAs and a few which are both C3 and C4
# if Watson can't resolve these, we will exclude from study
  gen.is.na <- filter(gen.a, is.na(gen.a$pp) == T)
  gen.is.na
  gen.c3.c4 <- filter(gen.a, pp == "C3 & C4")
  gen.c3.c4
  
# C3  
  c3 <- c("Achnatherum",
          "Amelichloa",   # reference: https://www.delta-intkey.com/grass/www/nassella.htm
          "Amphibromus",
          "Anthosachne",  # reference: https://www.delta-intkey.com/grass/www/elymus.htm
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
          "Saxipoa",      # no data on PP; genus recently moved from Poa, which is C3, so assuming that's conserved
          "Sylvipoa",     # no data on PP; genus recently moved from Poa, which is C3, so assuming that's conserved
          "Sasaella",
          "Tetrarrhena",
          "Thinopyrum",
          "Walwhalleya",
          "Zotovia")
          
  c3.index <- (gen.a$genus %in% c3)
  gen.a$pp[c3.index]<- "C3"

# C4
  c4 <- c("Aristida",
          "Diplachne",
          "Eragrostis",
          "Moorochloa",
          "Panicum",
          "Pseudopogonatherum",
          "Thellungia",
          "Zuloagaea")
  
  c4.index <- (gen.a$genus %in% c4)
  gen.a$pp[c4.index] <- "C4"
  
# Panicum asigned by species 
  
  
# merge genera and pathways 
  table(gen.a$pp)
  
  ala_pp <- left_join(ala, gen.a, by = "genus") %>%
            droplevels()
  head(ala_pp)
  table(ala_pp$pp, exclude = F)
  
  
# join to other family records
  ala_all <- bind_rows(ala_pp, ala_other_families) %>% 
             droplevels()

# save --------------------------------------------------------------------
  saveRDS(ala_all, "Data files/ALA/cleaning steps/step 6 - named, filtered and study family records.RDS")
  
# -------------------------------------------------------------------------    