
########################################################################################
# plot predictor varaibles 100 km scale maps
########################################################################################

# library ---------------------------------------------------------------------
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
   
# data ------------------------------------------------------------------------------  
  setwd("./Results/rasters/predictor variables")
  files <- list.files(pattern = ".grd")
  short_names <- gsub(pattern = ".grd", "", files)
  stack <- stack(files)
  names(stack) <- short_names
  #list2env(setNames(unstack(stack), names(ctack)), .GlobalEnv)
  setwd("C:/Users/s436862/Dropbox/Non-native-families")
  
  
  
  short_names_df <- data.frame(short_names)
  
  title_names <- c("Annual mean temperature",
                   "Annual precipitation",
                   "Aridity",
                   "Elevation",
                   "Human influence index",
                   
                   "Isothermality",
                   "Mean diurnal range",
                   "Plant available water capacity",
                   "Precipitation of the coldest quarter",
                   "Precipitation of the driest month",
                   
                   "Precipitation of the driest quarter",
                   "Plant-extractable the water capacity",
                   "Precipitation seasonality",
                   "Precipitation of the warmest quarter",
                   "Precipitation of the wettest month",
                   
                   "Precipitation of the wettest quarter",
                   "Potential storage of water in the root zone",
                   "Potential storage of water in the soil profile",
                   "Potential storage of water derived from soil texture",
                   "Temperature annual range",
                   
                   "Minimum temperature of the coldest Month",
                   "Mean temperature of the coldest quarter",
                   "Mean temperature of driest quarter",
                   "Topographic heterogeneity",
                   "Temperature seasonality",
                   
                   "Maxmimum temperature of the warmest month",
                   "Mean temperature of the warmest quarter",
                   "Mean temperature of the wettest quarter")
  
# insert units here
  units <- c("")
  
  names_df <- cbind(short_names, title_names)
  names_df <- data.frame(names_df)
  
# plot function ------------------------------------------------------------------------
  ras_v5 <- function (title, save, raster)  
    
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
                           #limits = c(0, 1),                               
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = "") + 
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
  
# maps ------------------------------------------------------------------
# loop function
  for (i in 1:nrow(names_df)) {
    
    ras_v5(paste(names_df$title_names[i]), 
           paste0("Results/maps/predictor variables/", names_df$short_names[i], ".jpeg"), 
           stack[[i]])
    
  }
  
  
# -------------------------------------------------------------------------
  
  
  
  
  
  
  
    