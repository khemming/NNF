##########################################################
# individual family-level correlation plots
##########################################################

# library ------------------------------------------------
  library(tidyverse)
  library(ggThemeAssist)
  library(raster)
  
  rm(list = ls())

# data ---------------------------------------------------
# rasters
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/scaled")
  
  current.list <- list.files(pattern = "observed.grd")
  names_long <- gsub(pattern = "/.observed.grd$", "", current.list)
  c_stack <- stack(current.list)
  names(c_stack) <- names_long
  list2env(setNames(unstack(c_stack), names(c_stack)), .GlobalEnv)
  
  spp <- as.data.frame(c_stack, na.rm = F)
  glimpse(spp)
  
  setwd("C:/Users/s436862/Dropbox/Non-native-families")
  
# list of familiy/taxa names 
  names_list_long <- read.csv("Results/csv/summary statistics/master family summary table.csv", stringsAsFactors = F) %>%
    filter(study_family == "yes") %>%
    dplyr::select(family)
  
  
# correlation plot function ------------------------------------------
# required: 
#          family (in quotes)
#          native column (from df "spp)
#     non-"                           "  
  cor_scaled <- function (family, native.col, non.native.col) {
  
  # correlation scores  
    cor <- round(cor(native.col, non.native.col, use = "complete.obs", method = "spearman"), 2)
    cor.label <- paste0("r = ", cor)
  # save 
    save <- paste0("Results/correlation plots/scaled/individual/", family, ".jpeg")
    
    
  # plot
    q <- ggplot(aes(x = native.col[,1], y = non.native.col[,1]), data = spp) +
                geom_point(shape = "circle", size = 1.5) +
                theme_bw() + 
                theme(panel.border = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      title = element_text(size = 18),
                      axis.line = element_line(colour = "black", size = 1),
                      axis.text.x = element_text(colour = "black", size = 14),
                      axis.text.y = element_text(colour = "black", size = 14),
                      axis.ticks = element_line(size = 1)) +
                scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                                   expand = c(0, 0.05),
                                   limits = c(0, 1)) +
                scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                                   limits = c(0, 1),
                                   expand = c(0, 0.05)
                                   ) +
                geom_abline(intercept = 0, slope = 1, size = 1) +
                annotate("text", x = 0.8, y = .95, 
                         label = cor.label, size = 5) +
                labs(title = family,
                     x = "Native richness",
                     y = "Non-native richness" )
      ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
    
      return(q)
  }
  
# -------------------------------------------------------- 
# required: 
#          family (in quotes)
#          native column (from df "spp)
#     non-"                           "  
# loop through families ----------------------------------
  for (i in 1:nrow(names_list_long)) {

    call.nat <- paste0(names_list_long[i, 1], ".native.observed.grd")
    nat.col <- dplyr::select(spp, call.nat)

    call.non.nat <- paste0(names_list_long[i, 1], ".non.native.observed.grd")
    non.nat.col <- dplyr::select(spp, call.non.nat)
    
    cor_scaled(names_list_long[i, 1],
               nat.col,
               non.nat.col)
  }
# ---------------------------------------------------------
  
  
  
  
  
  
  
  
  
  