########################################################
# modelling relationships between non-native families and environmental variables
########################################################


# library ----------------------------------------------
  library(dplyr)
  library(broom)
  library(magrittr)
  library(raster)

# data --------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/observed richness")

  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  spp <- as.data.frame(c.stack, na.rm = F)
  glimpse(spp)
  
  setwd("C:/Users/s436862/Dropbox/Non-native-families")
  
# environmental and anthropogenic varaible selection ------------
# note this is also the order these pop up in the coefficient plots, so vary it here if need be
  evs <- read.csv("C:/Users/s436862/Dropbox/Poaceae/Results/EVs/CSV/100 km EFs scaled.csv", header = T)
  
  spp.ev <- cbind(spp, evs) %>%
         filter(cell.category.v2 == "land") %>%
         dplyr::select(-cell.id, -cell.category.v1, -cell.category.v2)
  head(spp.ev)

  factors <- c("prop.cover", "amt",   "ap",     "arid",   "clay",             
               "elev",       "hii",   "iso",    "mdr",    "pawc", 
               "pcoldq" ,    "pdrym", "pet",    "pewc",   "ps", 
               "pwarmq",     "pwetm", "pwetq" , "rz",     "sp",
               "st",         "tar",   "tcoldm", "tcoldq", "tdryq",            
               "th",         "ts",    "twarmm", "twarmq", "twetq")
  
  as.formula(paste("y~", paste(factors, collapse="+")))
  
  
# models --------------------------------------------------------------
# Asteraceae ----------------------------------------------------------
# native
  y <- Asteraceae.native
  a.nat <- lm(Asteraceae.native ~ .,data = spp.ev[, c(1, 16:45)])
  summary(a.nat) 
  a.nat.sum <- tidy(a.nat)
  a.nat.ci <- confint(a.nat)
# non-native
  a.non <- lm(Asteraceae.non.native ~ pcoldq + pwarmq + amt + ts + arid + pewc + th + hii + prop.cover, data = spp.ev)
  summary(a.non) 
  a.non.sum <- tidy(a.non)
  a.non.ci <- confint(a.non)
  
