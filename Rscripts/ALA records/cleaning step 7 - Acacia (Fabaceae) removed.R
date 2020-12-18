

# library --------------------------------------------------------
  library(tidyverse)
  
  rm(list = ls())

# data -----------------------------------------------------------
  dat <- readRDS("Data files/ALA/cleaning steps/step 6 - pp family records.RDS")
  
# number and status of acacia species  
  aca <- dat %>% 
         distinct(species, .keep_all = T) %>%
         filter(genus == "Acacia")
  table(aca$status, exclude = NULL) # 1011 natives -- no nonatives
  
  dat2 <- dat %>% filter(family == "Fabaceae", 
                         genus != "Acacia")
  
  dat3 <- dat2 %>% mutate(family = "FNA")
  
  saveRDS(dat3, "Data files/ALA/master data/Fabaceae no acacia.RDS")  
# -----------------------------------------------------------------    