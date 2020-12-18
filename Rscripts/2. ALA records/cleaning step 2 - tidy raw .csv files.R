

# library --------------------------------------------------------
  library(ALA4R)
  library(tidyverse)
  library(data.table)

  rm(list = ls())

# data -------------------------------------------------------------  
# check header row for required columns and their names
  header <- read.csv("Data files/ALA/raw data/headings.csv", header = T, sep = "")
  
# required columns 
  fields <- c("Record ID", 
                     "Species",
                     "Year",     
                     "Latitude",
                     "Longitude",
                     "Coordinate Uncertainty in Metres",
                     "Taxon Rank",
                     "Family",
                     "Genus",
                     "Collector")

# convert raw.csvs to RDS --------------------------------------------
  csv_RDS <- function(location, 
                      file1, 
                      file2,
                      file3,
                      file4)
  {
    
  # file 1
    f1a <- paste0(location, file1)  
    f1b <- fread(f1a, header= T)
    f1 <- f1b[, ..fields] # double dot indicates it's an object not a column name
    
  # file 2
    f2a <- paste0(location, file2)  
    f2b <- fread(f2a, header= T)
    f2 <- f2b[, ..fields] 
    
  # file 3  
    f3a <- paste0(location, file3)  
    f3b <- fread(f3a, header= T)
    f3 <- f3b[, ..fields] 
    
  # file 4
    f4a <- paste0(location, file4)  
    f4b <- fread(f4a, header= T)
    f4 <- f4b[, ..fields] 
    
  # combine into single file
    plantae <- bind_rows(f1, f2, f3, f4)
    
    saveRDS(plantae, "Data files/ALA/cleaning steps/step 2 - raw record file.RDS")
    
  }
  
# run function -------------------------------------------------------------
# ignore warnings
  csv_RDS("Data files/ALA/raw data/",
          "data.csv",
          "data_part2.csv",
          "data_part3.csv",
          "data_part4.csv")
  
# -------------------------------------------------------------------------
  
