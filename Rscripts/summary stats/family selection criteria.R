#####################################################
# families that meet the selection criteria
#####################################################

# scope ---------------------------------------------
# which families meet the minimum required data to be analysed?

# library -------------------------------------------
  library(tidyverse)

# data ----------------------------------------------
# summary table based off Plantae downloaded records
  summary <- read.csv("Results/csv/summary statistics/Plantae families by species and records.csv")

# filter data ---------------------------------------  
# filters:
# >=100 native spp.  
# >=25 nn spp. (note that Chenopodiaceae and Scrophulariaceae have 24 non-native spp. and satisfy all other crieteria)
# >= 40k native records
  fams <- summary %>% filter(native.spp >= 100,
                             nn.spp >= 25,
                             native.rec >= 40000) %>%
                      dplyr::select(family)
  
  
# save
  write.csv(fams, "Data files/ALA/cleaning steps/family list beta.csv", row.names = F)

# -----------------------------------------------------