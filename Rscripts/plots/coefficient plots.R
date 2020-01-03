#################################################################
# iNEXT model coefficients plots
#################################################################

# aim -------------------------------------------------------------------
# produce plots showing how native and non-native species richness relate to environmental variables for the most ocmmon non-native plant families

# library --------------------------------------------------------------
  library(gplots)
  library(RColorBrewer)
  library(raster)
  library(ggmap)
  library(rgdal)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(ggThemeAssist)
  library(gplots)
  library(tidyverse)
  library(forcats)
  library(maps)

  rm(list = ls())

# load  workspace from iNEXT model (script) -------------------------------------
  load("Data files/Rdata/model_coefficients.RData")

# environmental variable labels
  plot.names <-  c("Winter\nrainfall", 
                   "Summer\nrainfall", 
                   "Annual mean\ntemperature", 
                   "Temperature\nseasonality", 
                   "Aridity", 
                   "Topographic\nheterogeneity",
                   "Human\nactivity")

# plot function ----------------------------------------------------------------------
  coef_status_plot <- function(family, native_label, non_native_label) {
    
    dat <- data.frame(coef.ar[,,family])
    dat[,3:5] %<>% lapply(function(x) as.numeric(as.character(x)))
    dat$plot.names <- plot.names
    r2_text <- paste0("Adj. r2 = ", r2)
    
    ggplot(dat, aes(y = plot.names, colour = status)) +
      theme_classic() +
      scale_colour_manual(labels = c(non_native_label, native_label), 
                          values = c("grey30", "grey60")) +
      geom_vline(aes(xintercept = 0),
                 colour = "black", 
                 size = 0.6, 
                 linetype = "dashed") +
      labs(x = "Parameter estimate",
           y = "Environmental and anthropogenic variables") +
      geom_point(aes(x = estimate), size = 4) +
      geom_errorbarh(aes(xmin = lower.ci, xmax = upper.ci),
                     size = 1, height = 0.1)
    
    savefile <- paste0("Results/coefficient plots/", family, ".jpeg")
    
    ggsave(savefile, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  } # fun end
  
  
# run function ----------------------------------------------
# family (in parentheses)
# native_label: native + adj r2 value
# non-"       " non-"                " 
  coef_status_plot("Asteraceae",   
                   "native\nadj. r2 = 0.45", 
                   "non-native\nadj. r2 = 0.25")
  
  coef_status_plot("Brassicaceae", 
                   "native\nadj. r2 = 0.11",
                   "non-native\nadj. r2 = 0.41")
  
  coef_status_plot("Cyperaceae",   
                   "native\nadj. r2 = 0.60",
                   "non-native\nadj. r2 = 0.43")
  
  coef_status_plot("Fabaceae",   
                   "native\nadj. r2 = 0.41", 
                   "non-native\nadj. r2 = 0.53")
  
  coef_status_plot("Malvaceae",   
                   "native\nadj. r2 = 0.50", 
                   "non-native\nadj. r2 = 0.11")
  
  coef_status_plot("Myrtaceae",   
                   "native\nadj. r2 = 0.28",
                   "non-native\nadj. r2 = __")
  
  coef_status_plot("Plantae",   
                   "native\nadj. r2 = 0.35", 
                   "non-native\nadj. r2 = 0.57")
  
  coef_status_plot("Proteaceae",   
                   "native\nadj. r2 = 0.28", 
                   "non-native\nadj. r2 = __")
  
  coef_status_plot("Solanaceae",   
                   "native\nadj. r2 = 0.19", 
                   "non-native\nadj. r2 = 0.32")
  
  
 
  
  