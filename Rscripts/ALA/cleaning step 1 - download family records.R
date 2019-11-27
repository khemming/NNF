#####################################################
############# download family records ###############
#####################################################
# scope -----------------------------------------------
# download the records for the most invasive plant families of Australia
# use ALA4R package


# library --------------------------------------------------------
  library(ALA4R)
  library(tidyverse)
  library(data.table)

  rm(list = ls())

# required field columns from ALA
# note--------------------------------------------------
# field definitions: https://docs.google.com/spreadsheet/ccc?key=0AjNtzhUIIHeNdHhtcFVSM09qZ3c3N3ItUnBBc09TbHc
# these fields I selected below are required for processing data for accuracy
# mostly for culling records missing relevant fields (i..e year or species) and checking for duplicates
# ---------------------------------------------------------------
# required fields from download
  dl_fields <- c("year",
                "island",
                "country",
                "state",
                "latitude",
                "longitude",
                "coordinate_uncertainty",
                "rank",
                "family",
                "genus",
                "species",
                "id",
                "collector")

# all columns are returned; to subset out required ones only, we need a new list because some names change   
  subset_fields <- c("year",
                     "island",
                     "country",
                     "state",
                     "latitude",
                     "longitude",
                     "coordinateUncertaintyInMetres",
                     "rank",
                     "family",
                     "genus",
                     "species",
                     "id",
                     "collector")
  
# download records by family ------------------------------------  
# Asteraceae ----------------------------------------------------
  ast <- occurrences(taxon="family:Asteraceae",
                   fields= dl_fields,
                   download_reason_id = 11,
                   method = "offline",
                   email = "kyle.hemming@canberra.edu.au")
  ast_dat <- ast$data
  
  ast_dat_b <- select(ast_dat, subset_fields)
  
  saveRDS(ast_dat_b, "./Data files/ALA/Asteraceae raw records.RDS")
  
# Fabaceae ------------------------------------------------
  fab <- occurrences(taxon="family:Fabaceae",
                     fields= dl_fields,
                     download_reason_id = 11,
                     method = "offline",
                     email = "kyle.hemming@canberra.edu.au")
  fab_dat <- fab$data
  
  fab_dat_b <- select(fab_dat, subset_fields)
  
  saveRDS(fab_dat_b, "./Data files/ALA/Fabaceae raw records.RDS")
  
  rm(list=setdiff(ls(), c("dl_fields", "subset_fields")))

# Brassicaceae ----------------------------------------------
  bra <- occurrences(taxon="family:Brassicaceae",
                     fields= dl_fields,
                     download_reason_id = 11,
                     method = "offline",
                     email = "kyle.hemming@canberra.edu.au")
  bra_dat <- bra$data
  
  bra_dat_b <- select(bra_dat, subset_fields)
  
  saveRDS(bra_dat_b, "./Data files/ALA/Brassicaceae raw records.RDS")
  
  rm(list=setdiff(ls(), c("dl_fields", "subset_fields")))
  
  
# Cyperaceae -------------------------------------------  
  cyp <- occurrences(taxon="family:Cyperaceae",
                     fields= dl_fields,
                     download_reason_id = 11,
                     method = "offline",
                     email = "kyle.hemming@canberra.edu.au")
  cyp_dat <- cyp$data
  
  cyp_dat_b <- select(cyp_dat, subset_fields)
  
  saveRDS(cyp_dat_b, "./Data files/ALA/Cyperaceae raw records.RDS")
  
  rm(list=setdiff(ls(), c("dl_fields", "subset_fields")))
  
# Malvaceae --------------------------------------------------
  mal <- occurrences(taxon="family:Malvaceae",
                     fields= dl_fields,
                     download_reason_id = 11,
                     method = "offline",
                     email = "kyle.hemming@canberra.edu.au")
  mal_dat <- mal$data
  
  mal_dat_b <- select(mal_dat, subset_fields)
  
  saveRDS(mal_dat_b, "./Data files/ALA/Malvaceae raw records.RDS")
  
  rm(list=setdiff(ls(), c("dl_fields", "subset_fields")))
  
# Myrtaceae --------------------------------------------------
  myr <- occurrences(taxon="family:Myrtaceae",
                     fields= dl_fields,
                     download_reason_id = 11,
                     method = "offline",
                     email = "kyle.hemming@canberra.edu.au")
  myr_dat <- myr$data
  
  myr_dat_b <- select(myr_dat, subset_fields)
  
  saveRDS(myr_dat_b, "./Data files/ALA/Myrtaceae raw records.RDS")
  
  rm(list=setdiff(ls(), c("dl_fields", "subset_fields")))
  
# Proteaceae ------------------------------------------------
  pro <- occurrences(taxon="family:Proteaceae",
                     fields= dl_fields,
                     download_reason_id = 11,
                     method = "offline",
                     email = "kyle.hemming@canberra.edu.au")
  pro_dat <- pro$data
  
  pro_dat_b <- select(pro_dat, subset_fields)
  
  saveRDS(pro_dat_b, "./Data files/ALA/Proteaceae raw records.RDS")
  
  rm(list=setdiff(ls(), c("dl_fields", "subset_fields")))
  
# Solanaceae -------------------------------------------  
  sol <- occurrences(taxon="family:Solanaceae",
                     fields= dl_fields,
                     download_reason_id = 11,
                     method = "offline",
                     email = "kyle.hemming@canberra.edu.au")
  sol_dat <- sol$data
  
  sol_dat_b <- select(sol_dat, subset_fields)
  
  saveRDS(sol_dat_b, "./Data files/ALA/Solanaceae raw records.RDS")
  
  rm(list=setdiff(ls(), c("dl_fields", "subset_fields")))
# -------------------------------------------------------------    