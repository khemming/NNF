##########################################################
# predicred distributions of native species, and potential distrributions of non-native species
##########################################################

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
  
  rm(list = ls())

# data ---------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/scaled richness")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  spp <- as.data.frame(c.stack, na.rm = F)
  glimpse(spp)
  
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/maps")

# scaled maps (v5) ---------------------------------------------------------
  ras_v5 <- function (title, save, raster)  
    
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
                           limits = c(0, 1),                               
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = "Relative\nspecies\nrichness") + 
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
  
 
# predicted richness plots --------------------------------------------------------
# required: title, save, raster
  
# Asteraceae ----------------------------------------------------------------------
# native
  ras_v5("Observed native Asteraceae",
         "observed native Asteraceae.jpeg",
         Asteraceae.native)

# non.native
  ras_v5("Observed non-native Asteraceae",
         "observed non-native Asteraceae.jpeg", 
         Asteraceae.non.native)

# predicted native 
  ras_v5("Predicted native Asteraceae",
         "predicted native Asteraceae.jpeg", 
         predicted.Asteraceae.native)
  
# potential non-native  
  ras_v5("Non-native Asteraceae",
         "potential non-native Asteraceae.jpeg", 
         potential.Asteraceae.non.native)
  
# Brassicaceae -------------------------------------------------------------------
# native
  ras_v5("Observed native Brassicaceae",
         "observed native Brassicaceae.jpeg",
         Brassicaceae.native)
  
# non.native
  ras_v5("Observed non-native Brassicaceae",
         "observed non-native Brassicaceae.jpeg", 
         Brassicaceae.non.native)
  
# predicted native 
  ras_v5("Predicted native Brassicaceae",
         "predicted native Brassicaceae.jpeg", 
         predicted.Brassicaceae.native)
  
# potential non-native  
  ras_v5("Non-native Brassicaceae",
         "potential non-native Brassicaceae.jpeg", 
         potential.Brassicaceae.non.native)
  

# Cyperaceae -------------------------------------------------------------------
# native
  ras_v5("Observed native Cyperaceae",
         "observed native Cyperaceae.jpeg",
         Cyperaceae.native)
  
# non.native
  ras_v5("Observed non-native Cyperaceae",
         "observed non-native Cyperaceae.jpeg", 
         Cyperaceae.non.native)
  
# predicted native 
  ras_v5("Predicted native Cyperaceae",
         "predicted native Cyperaceae.jpeg", 
         predicted.Cyperaceae.native)
  
# potential non-native  
  ras_v5("Non-native Cyperaceae",
         "potential non-native Cyperaceae.jpeg", 
         potential.Cyperaceae.non.native)

# Fabaceae -------------------------------------------------------------------
# native
  ras_v5("Observed native Fabaceae",
         "observed native Fabaceae.jpeg",
         Fabaceae.native)
  
# non.native
  ras_v5("Observed non-native Fabaceae",
         "observed non-native Fabaceae.jpeg", 
         Fabaceae.non.native)
  
# predicted native 
  ras_v5("Predicted native Fabaceae",
         "predicted native Fabaceae.jpeg", 
         predicted.Fabaceae.native)
  
# potential non-native  
  ras_v5("Non-native Fabaceae",
         "potential non-native Fabaceae.jpeg", 
         potential.Fabaceae.non.native)
  
# Malvaceae -------------------------------------------------------------------
# native
  ras_v5("Observed native Malvaceae",
         "observed native Malvaceae.jpeg",
         Malvaceae.native)
  
# non.native
  ras_v5("Observed non-native Malvaceae",
         "observed non-native Malvaceae.jpeg", 
         Malvaceae.non.native)
  
# predicted native 
  ras_v5("Predicted native Malvaceae",
         "predicted native Malvaceae.jpeg", 
         predicted.Malvaceae.native)
  
# potential non-native  
  ras_v5("Non-native Malvaceae",
         "potential non-native Malvaceae.jpeg", 
         potential.Malvaceae.non.native)
  
# Myrtaceae -------------------------------------------------------------------
# native
  ras_v5("Observed native Myrtaceae",
         "observed native Myrtaceae.jpeg",
         Myrtaceae.native)
  
# non.native
  ras_v5("Observed non-native Myrtaceae",
         "observed non-native Myrtaceae.jpeg", 
         Myrtaceae.non.native)
  
# predicted native 
  ras_v5("Predicted native Myrtaceae",
         "predicted native Myrtaceae.jpeg", 
         predicted.Myrtaceae.native)
  
# potential non-native  
  ras_v5("Non-native Myrtaceae",
         "potential non-native Myrtaceae.jpeg", 
         potential.Myrtaceae.non.native)

# Plantae -------------------------------------------------------------------
# native
  ras_v5("Observed native Plantae",
         "observed native Plantae.jpeg",
         Plantae.native)
  
# non.native
  ras_v5("Observed non-native Plantae",
         "observed non-native Plantae.jpeg", 
         Plantae.non.native)
  
# predicted native 
  ras_v5("Predicted native Plantae",
         "predicted native Plantae.jpeg", 
         predicted.Plantae.native)
  
# potential non-native  
  ras_v5("Non-native Plantae",
         "potential non-native Plantae.jpeg", 
         potential.Plantae.non.native)
  
# Solanaceae -------------------------------------------------------------------
# native
  ras_v5("Observed native Solanaceae",
         "observed native Solanaceae.jpeg",
         Solanaceae.native)
  
# non.native
  ras_v5("Observed non-native Solanaceae",
         "observed non-native Solanaceae.jpeg", 
         Solanaceae.non.native)
  
# predicted native 
  ras_v5("Predicted native Solanaceae",
         "predicted native Solanaceae.jpeg", 
         predicted.Solanaceae.native)
  
# potential non-native  
  ras_v5("Non-native Solanaceae",
         "potential non-native Solanaceae.jpeg", 
         potential.Solanaceae.non.native)


# ---------------------------------------------------------------------------