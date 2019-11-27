##########################################################
# observed maps
##########################################################

# scope --------------------------------------------------
# map the distributions of species richness of native and non-native plant families of Australia

# library ------------------------------------------------
  library(tidyverse)
  library(ggThemeAssist)
  library(raster)
  library(gplots)
  library(RColorBrewer)
  library(ggmap)
  library(rgdal)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(forcats)
  library(maps)
  
# data ---------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/observed richness")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  spp <- as.data.frame(c.stack, na.rm = F)
  glimpse(spp)
  
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/observed maps")

# observed maps ---------------------------------------------------------
# function -- remember legend (leg) requirements
  ras_v3 <- function (title, raster, save, scale)  
    
  {
  # AUS border + NA fill
    oz1 <- borders("world", region = "Australia", fill = "grey60", bg = "white")
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
                           limits = scale,                               
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
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            aspect.ratio = 0.88,
            plot.title = element_text(size = 26, face = "bold")
      ) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
  
 
# fun function ---------------------------------------------------------------------
# title, raster, save, legend scale
  
# Asteraceae -------------------------------------------------------------------
# native
  ras_v3("Native Asteraceae",
         Asteraceae.native, 
         "Asteraceae.native.jpeg", 
         c(1, cellStats(Asteraceae.native, stat = 'max', na.rm = T)))
# non.native
  ras_v3("Non-native Asteraceae",
         Asteraceae.non.native, 
         "Asteraceae.non.native.jpeg", 
         c(1, cellStats(Asteraceae.non.native, stat = 'max', na.rm = T)))
  
# Brassicaceae -------------------------------------------------------------------
# native
  ras_v3("Native Brassicaceae",
         Brassicaceae.native, 
         "Brassicaceae.native.jpeg", 
         c(1, cellStats(Brassicaceae.native, stat = 'max', na.rm = T)))
# non.native
  ras_v3("Non-native Brassicaceae",
         Brassicaceae.non.native, 
         "Brassicaceae.non.native.jpeg", 
         c(1, cellStats(Brassicaceae.non.native, stat = 'max', na.rm = T)))
  
# Fabaceae -------------------------------------------------------------------
# native
  ras_v3("Native Fabaceae",
         Fabaceae.native, 
         "Fabaceae.native.jpeg", 
         c(1, cellStats(Fabaceae.native, stat = 'max', na.rm = T)))
# non.native
  ras_v3("Non-native Fabaceae",
         Fabaceae.non.native, 
         "Fabaceae.non.native.jpeg", 
         c(1, cellStats(Fabaceae.non.native, stat = 'max', na.rm = T)))

# Cyperaceae -------------------------------------------------------------------
# native
  ras_v3("Native Cyperaceae",
         Cyperaceae.native, 
         "Cyperaceae.native.jpeg", 
         c(1, cellStats(Cyperaceae.native, stat = 'max', na.rm = T)))
# non.native
  ras_v3("Non-native Cyperaceae",
         Cyperaceae.non.native, 
         "Cyperaceae.non.native.jpeg", 
         c(1, cellStats(Cyperaceae.non.native, stat = 'max', na.rm = T)))

# Myrtaceae -------------------------------------------------------------------
# native
  ras_v3("Native Myrtaceae",
         Myrtaceae.native, 
         "Myrtaceae.native.jpeg", 
         c(1, cellStats(Myrtaceae.native, stat = 'max', na.rm = T)))
# non.native
  ras_v3("Non-native Myrtaceae",
         Myrtaceae.non.native, 
         "Myrtaceae.non.native.jpeg", 
         c(1, cellStats(Myrtaceae.non.native, stat = 'max', na.rm = T)))

# Proteaceae -------------------------------------------------------------------
# native
  ras_v3("Native Proteaceae",
         Proteaceae.native, 
         "Proteaceae.native.jpeg", 
         c(1, cellStats(Proteaceae.native, stat = 'max', na.rm = T)))
# non.native
  # ras_v3("Non-native Asteraceae",
  #        Asteraceae.non.native, 
  #        "Asteraceae.non.native.jpeg", 
  #        c(1, cellStats(Asteraceae.non.native, stat = 'max', na.rm = T)))
# Solanaceae -------------------------------------------------------------------
# native
  ras_v3("Native Solanaceae",
         Solanaceae.native, 
         "Solanaceae.native.jpeg", 
         c(1, cellStats(Solanaceae.native, stat = 'max', na.rm = T)))
# non.native
  ras_v3("Non-native Solanaceae",
         Solanaceae.non.native, 
         "Solanaceae.non.native.jpeg", 
         c(1, cellStats(Solanaceae.non.native, stat = 'max', na.rm = T)))

# -------------------------------------------------------------------------------