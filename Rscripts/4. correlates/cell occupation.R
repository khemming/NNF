

# library -----------------------------------------------------
  library(raster)
  library(tidyverse)
  
  rm(list = ls())
  
# data --------------------------------------------------------
# australia 
  aus <- raster("Data files/Australia/Australia 1156.grd")
  
# record data
  dat <- readRDS("Data files/ALA/master data/master records.RDS")
  
# 22 families
  fams <-  read.csv("Results/csv/families and predictors/study families.csv", stringsAsFactors = F)[1:22, 1]
  glimpse(fams)
  

# cell occupation ---------------------------------------------
# output matrix
  j <- data.frame(family = fams, 
                  nat_1 = NA,
                  nonnat_1 = NA,
                  prop_1 = NA,
                  nat_15 = NA,
                  nonnat_15 = NA,
                  prop_15 = NA)
  j
  
  
  for (i in 1:length(fams)) {
    
  # native records  
    nat <- dat %>% filter(family == fams[i], status == "native")
    xy <- cbind(nat$longitude, nat$latitude)
    occ <- rasterize(xy, aus, fun = function(x,...) {length(unique(na.omit(x))) })
    occ2 <- data.frame(getValues(occ))
  # 1 or more record  
    j$nat_1[i]  <- nrow(occ2 %>% filter(!is.na(getValues.occ.)))
  # 15 or more records
    j$nat_15[i] <- nrow(occ2 %>% filter(!is.na(getValues.occ.),
                                 getValues.occ. >= 15))
    
  # nonnative records  
    nonnat <- dat %>% filter(family == fams[i], status == "nonnative")
    xy <- cbind(nonnat$longitude, nonnat$latitude)
    occ <- rasterize(xy, aus, fun = function(x,...) {length(unique(na.omit(x))) })
    occ2 <- data.frame(getValues(occ))
  # 1 or more record  
    j$nonnat_1[i]  <- nrow(occ2 %>% filter(!is.na(getValues.occ.)))
  # 15 or more records
    j$nonnat_15[i] <- nrow(occ2 %>% filter(!is.na(getValues.occ.),
                                        getValues.occ. >= 15))
   
  # proportion of native:nonnative occupation for 1 and 15 or more 
    j$prop_1[i] <- j$nonnat_1[i]/j$nat_1[i]
    j$prop_15[i] <- j$nonnat_15[i]/j$nat_15[i]
    }
    
  j
  
  
  write.csv(j, "Results/csv/correlates/cell occupation.csv", row.names = F)  

# -------------------------------------------------------------