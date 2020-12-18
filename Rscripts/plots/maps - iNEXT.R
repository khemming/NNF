


# library ------------------------------------------------
  library(raster)
  library(tidyverse)
  library(ggThemeAssist)
  library(gplots)
  library(RColorBrewer)
  library(ggmap)
  library(rgdal)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(forcats)
  library(maps)
  
  rm(list = ls())

# data ---------------------------------------------------
# lists of familiy/taxa names and status
  fam_short <- read.csv("Results/csv/summary statistics/master family summary table.csv", stringsAsFactors = F) %>%
                 filter(study_family == "yes") %>% 
                 dplyr::select(family)
    
  fam_long <- as.data.frame(rep(fam_short$family, each = 2))
  colnames(fam_long) <- "family"
  fam_long$status <- rep(c("native", "non-native"), by = nrow(fam_long))
  
# iNEXT rasters 
# observed 
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/iNEXT")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names_long
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)

  setwd("C:/Users/s436862/Dropbox/Non-native-families")

# raw maps (v5) ---------------------------------------------------------
  ras_v5 <- function(title, save, raster, leg_scale)  
    
  {
  # AUS border + NA fill
    oz1 <- borders("world", region = "Australia", fill = "grey60")
    oz2 <- borders(database = "world", regions = "Australia", colour = "black")
    
  # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
  # display.brewer.all() 
  # (for more info)
    
  # Plot
    q <- gplot(raster) + 
      theme_map() +
      ggtitle(title) +
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           limits = leg_scale,                             
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = "Species\nrichness") + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "right",
            legend.position = "right",
            legend.key.size = unit(1, "cm"),
            legend.key.width = unit(1,"cm"),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            aspect.ratio = 0.88,
            plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5)
      ) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  } 
  
# richness plots --------------------------------------------------------
  for (i in 1:nrow(fam_long)) {
    
    title <- paste(fam_long$family[i], fam_long$status[i])
    save <- paste0("Results/maps/iNEXT/", title, ".jpeg")
    raster <- c.stack[[i]]
    leg_scale <- c(0, cellStats(raster, stat = 'max', na.rm = T))
    
    ras_v5(title, save, raster, leg_scale)
    
  }

# ---------------------------------------------------------------------------
  