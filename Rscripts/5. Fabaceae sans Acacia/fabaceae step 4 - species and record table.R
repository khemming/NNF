

# library ------------------------------------------------
  library(janitor)
  library(tidyverse)

# data ---------------------------------------------------
# Fabaceae with no Acacia (fna)  
  fna <- readRDS("Data files/ALA/master data/Fabaceae no acacia.RDS")
  glimpse(fna)
  fab <- readRDS("Data files/ALA/master data/master records.RDS") %>%
         dplyr::select(-study_family) %>%
         filter(family == "Fabaceae")

  dat <- rbind(fab, fna)

# data table --------------------------------------------
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
  
  dat2 <- left_join(fam_ss, fam_rs, by = "family") %>%
          adorn_totals()
  dat2  

  write.csv(dat2, "Results/Fabaceae sans Acacia/csv/Spp and rec table.csv")  

# -----------------------------------------------------------
  
    