###########################################################
# download Plantae records
###########################################################

# library --------------------------------------------------------
  library(ALA4R)
  
  rm(list = ls())
  
# all Plantae data -----------------------------------------------------------
  occurrences(taxon = "kingdom:Plantae",
              fields= dl_fields,
              download_reason_id = 11,
              method = "offline",
              email = "kyle.hemming@canberra.edu.au")
  
# ---------------------------------------------------------------------------

               
               
  