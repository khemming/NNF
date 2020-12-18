##########################################################
# correaltions group in plot and csv
##########################################################


# library ------------------------------------------------
  library(tidyverse)
  library(ggThemeAssist)
  library(raster)
  library(ggrepel)

  rm(list = ls())

# data ---------------------------------------------------
# observed rasters
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/scaled")
  
  current_list <- list.files(pattern = "observed.grd")
  names_long <- gsub(pattern = ".observed.grd$", "", current_list)
  c_stack <- stack(current_list)
  names(c_stack) <- names_long
  list2env(setNames(unstack(c_stack), names(c_stack)), .GlobalEnv)
  
  spp <- as.data.frame(c_stack, na.rm = F)
  glimpse(spp)
  
  setwd("C:/Users/s436862/Dropbox/Non-native-families")
  
# family, prefix, and record numbers
  prefix <- read.csv("C:/Users/s436862/Dropbox/Non-native-families/Results/csv/summary statistics/master family summary table.csv", stringsAsFactors = F) %>%
    filter(study_family == "yes") %>%
    dplyr::select(prefix)
    
  rec <- read.csv("C:/Users/s436862/Dropbox/Non-native-families/Results/csv/summary statistics/master family summary table.csv", stringsAsFactors = F) %>%
            filter(study_family == "yes") %>%
            dplyr::select(family, native_rec, non_native_rec, total_rec) 
  head(rec) 
  
  fam_names_short <- read.csv("C:/Users/s436862/Dropbox/Non-native-families/Results/csv/summary statistics/master family summary table.csv", stringsAsFactors = F) %>%
    filter(study_family == "yes") %>%
    dplyr::select(family)
  

# data transformed for plot input ----------------------------------------
  rec <- rec %>% arrange(correlation) %>%
            mutate(family = fct_reorder(family, correlation))
  rec$family <- factor(rec$family, levels = rec$family[order(rec$correlation)])
  
  head(rec)
  glimpse(rec)
  
# correlation plot --------------------------------------------------------
# correlation x family
  ggplot(rec, aes(y = correlation, x = family)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 16)) +
    scale_y_continuous(limits = c(-0.2, 0.8),
                       breaks = c(seq(-0.2, 0.8, 0.2))) +
    geom_point(aes(y = correlation), size = 3) +
    geom_hline(aes(yintercept = 0),
               colour = "black", 
               size = 0.6, 
               linetype = "dashed") +
    labs(x = "Taxon",
         y = "Native-non-native correlation") 
  
  ggsave("./Results/correlation plots/scaled//grouped/family correlations no weights.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# correlation x records ---------------------------------------------------------------
  ggplot(rec, aes(y = correlation, x = log(total_rec))) +
    theme_classic() +
    theme(axis.text.x = element_text(hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 16)) +
    scale_y_continuous(limits = c(-0.2, 0.8),
                       breaks = c(seq(-0.2, 0.8, 0.2))) +
    geom_point(size = 2) +
    labs(x = "total records (log)",
         y = "Native-non-native correlation") 
  
  ggsave("./Results/correlation plots/scaled//grouped/correlation by total records.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  

# weighted by total records -------------------------------------------------
# note, Plantae has too many records for good resolution for other groups, is therefore removed
  rec2 <- rec %>% filter(family != "Plantae")
  
  ggplot(rec2, aes(x = family, y = correlation, size = total_rec)) +
    geom_point() +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 16)) +
    scale_y_continuous(limits = c(-0.2, 0.8),
                       breaks = c(seq(-0.2, 0.8, 0.2))) +
    
    geom_hline(aes(yintercept = 0),
               colour = "black", 
               size = 0.6, 
               linetype = "dashed") +
    labs(size = "Total\nrecords",
         x = "Taxon",
         y = "Native-non-native correlation")
  
  ggsave("./Results/correlation plots/scaled//grouped/correlations weighed by total records.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
  

# ------------------------------------------------------------------------
  
