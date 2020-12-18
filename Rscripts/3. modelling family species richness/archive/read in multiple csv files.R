############################################################################
# Summary of selected families' species richness, records etc.
############################################################################

# library ------------------------------------------------------------------
  library(tidyverse)
  library(data.table)

  rm(list = ls())

# scope --------------------------------------------------------------------
# not preliminary: these are final data 
# sections as follows:
# 1. records and raw species richness
# 2. iNEXT richness
# 3. scaled richness
# 4. native:non-native correlation
# 5. native R2 values

# data ---------------------------------------------------------------------
  setwd("./Results/csv/iNEXT")

  filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
  
  for(i in filenames){
    assign(i, read.csv(paste(i, ".csv", sep="")))
  }

# 1. records and species richness ------------------------------------------




# 2. iNEXT species richness ------------------------------------------------  
rm(list = ls())

# rasters
setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/scaled richness")

current.list <- list.files(pattern = ".grd")
names <- gsub(pattern = "\\.grd$", "", current.list)
c.stack <- stack(current.list)
names(c.stack) <- names
list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)

spp <- as.data.frame(c.stack, na.rm = F)
glimpse(spp)

setwd("C:/Users/s436862/Dropbox/Non-native-families")

# retrieve iNEXT species richness ------------------------------------------


# -------------------------------------------------------------------------- 