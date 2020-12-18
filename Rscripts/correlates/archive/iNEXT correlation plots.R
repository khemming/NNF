##########################################################
# correlation plots
##########################################################

# library ------------------------------------------------
  library(tidyverse)
  library(ggThemeAssist)
  library(raster)

  rm(list = ls())

# data ---------------------------------------------------
# family list & summary stats table
  names_table <- read.csv("Results/csv/summary statistics/master family summary table.csv", stringsAsFactors = F) %>%
          filter(study_family == "yes") %>%
          dplyr::select(family)
  
# rasters
  setwd("./Results/rasters/iNEXT")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  spp <- as.data.frame(c.stack, na.rm = F)
  glimpse(spp)
  
  setwd("C:/Users/s436862/Dropbox/Non-native-families")

# correlation plot function ---------------------------------------------
# requires:
# family  = family name
# nat     = native column fromm spp data frame
# non.nat = non-native "                     "
  inext_cor <- function(family, nat, non.nat){
  
  # correlation
    cor <- round(cor(nat, non.nat, use = "complete.obs", method = "spearman"), 2)
    cor.label <- paste0("r = ", cor)
  
  # breaks
    sp <- data.frame(cbind(nat, non.nat))
    sp2 <- sp %>% filter(!is.na(nat) & !is.na(non.nat))
    
  # save
    save <- paste0("./Results/correlation plots/iNEXT/", family, ".jpeg")
    
    # plot 
    q <- ggplot(aes(x = nat[,1], y = non.nat[,1]), data = spp) +
      geom_point(shape = "circle", size = 1) +
      theme_bw() + 
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 18),
            axis.text.y = element_text(colour = "black", size = 18),
            axis.ticks = element_blank()) +
      scale_x_continuous(breaks = c(1,
                                    round((max(sp2[,1])*0.25), 0), 
                                    round((max(sp2[,1])*0.5), 0), 
                                    round((max(sp2[,1])*0.75), 0), 
                                    round(max(sp2[,1]), 0)), 
                         limits = c(0, max(sp2[,1]*1.1)), 
                         expand = c(0, 0.5)) +
      scale_y_continuous(breaks = c(1,
                                    round((max(sp2[,2])*0.25), 0), 
                                    round((max(sp2[,2])*0.5), 0), 
                                    round((max(sp2[,2])*0.75), 0), 
                                    round(max(sp2[,2]), 0)), 
                         limits = c(0, max(sp2[,2]*1.1)), 
                         expand = c(0, 0.5)) +
      #geom_abline(intercept = 0, slope = 1, size = 1) +
      annotate("text", x = max(sp2[,1]-2), y = max(sp2[,2]), 
               label = cor.label, size = 5) +
      labs(title = family,
           x = "native richness",
           y = "non-native richness") +
      theme(axis.title = element_text(size = 18),
            title = element_text(size = 24))
    
    ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
    
    return(q)
    
  }
  
# loop through families ----------------------------------
  for (i in 1:nrow(names_table)) {
  
    family <- names_table[i, ]
    call.nat <- paste0(family, ".native")
    nat.col <- dplyr::select(spp, call.nat)
    
    call.non.nat <- paste0(family, ".non.native")
    non.nat.col <- dplyr::select(spp, call.non.nat)
    
    inext_cor(family,
              nat.col,
              non.nat.col)
  }

# --------------------------------------------------------