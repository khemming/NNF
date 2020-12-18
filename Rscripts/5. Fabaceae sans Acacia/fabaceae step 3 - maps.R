

# library ------------------------------------------------
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
  library(tidyverse)
  
  rm(list = ls())

# data ---------------------------------------------------
# shape file
  oz <- readOGR("Data files/Australia/Australia shapefile.shp")
  plot(oz)
  
# Fabaceae no Acacia (FNA)
  setwd("C:/Users/s436862/Dropbox/NNF")
  fna_scale <- raster("Results/Fabaceae sans Acacia/rasters/iNEXT/FNA_native.grd")
  fna_obs <- raster("Results/Fabaceae sans Acacia/rasters/scaled/FNA_native_observed.grd")
  fna_pred <- raster("Results/Fabaceae sans Acacia/rasters/predicted/FNA_native_predicted.grd")
  fna_pot <- raster("Results/Fabaceae sans Acacia/rasters/potential/FNA_nonnative__potential.grd")
  fna_obs_pot <- raster("Results/Fabaceae sans Acacia/rasters/potential/FNA_obs_nat_potential.grd")

# scaled maps (v10) --------------------------------------------------------
# observed maps
  ras_v10 <- function(raster, title, family, sr_breaks, sr_max, legend_title){
      
    # scale breaks and labels
      log_sr <- log(sr_breaks)
      scale_sr <- log_sr/max(log_sr)
      
      log_mx <- log(cellStats(sr_max, "max", na.rm = T))
      scale_mx <- log_mx/max(log_sr)
      
      leg_scale <- scale_sr / scale_mx
      
    # spatial points dataframe 
      raster_spdf <- as(raster, "SpatialPixelsDataFrame")
      raster_df <- as.data.frame(raster_spdf)
      colnames(raster_df) <- c("value", "x", "y")
    
    # km scale bar  
      km_scale <- raster_df %>% rename(lat = y, long = x)
      km_pos <- as.vector(data.frame(x = 130, 
                                     y = -40))
      
    # pretty colours
      colr <- rev(brewer.pal(11, "Spectral"))
    
    # save 
      save_txt <- paste0("results/maps/scaled/", title, ".jpeg")  
      
    # plot
      q <- ggplot() +  
        ggtitle(title) +
        geom_polygon(data = oz, aes(x = long, y = lat, group = group),
                     fill = "grey60") +
        geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +             
        geom_polygon(data = oz, colour = "grey1", 
                     aes(x = long, y = lat, group = group), fill = NA, size = 0.7) + 
        scale_fill_gradientn(colours = colr, 
                             limits = c(min(leg_scale), max(leg_scale)),            
                             breaks = leg_scale, 
                             labels = sr_breaks,
                             space = "Lab",
                             name = legend_title) +
        coord_fixed(ratio = 38/33, xlim = c(112, 155), ylim = c(-45, -7)) +
        #ggsn::scalebar(km_scale, dist = 500, dist_unit = "km",  st.size = 4, st.dist = 0.05, st.bottom = T, height = 0.05, transform = T, model = 'WGS84', location = "topright", anchor = km_pos)
        theme_map() +
        theme(legend.direction = "vertical",
              legend.justification = "right",
              legend.position = "right",
              legend.key.size = unit(18, "cm"),
              legend.key.width = unit(1,"cm"),
              legend.title = element_text(size = 16),
              legend.text = element_text(size = 12),
              legend.box.spacing = unit(0.01, "cm"),
              plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5)) +
        guides(fill = guide_colorbar(barheight = unit(5, "cm")))
    
    plot(q)  
    
  # save 
    save_1 <- paste0("results/maps/scaled/", title, ".jpeg")  
    save_2 <- paste0("Results/major figures by family/", family, "/", title, ".jpeg")
    ggsave(save_1, plot = last_plot(), height = 15, width = 15, units = "cm", dpi = 500, device = "jpeg")
    ggsave(save_2, plot = last_plot(), height = 15, width = 15, units = "cm", dpi = 500, device = "jpeg")
    
    
  } 
  
# observed map
  raster <- fna_obs
  title <- "FNA_observed"
  family <- "Fabaceae sans Acacia"
    
  sr_breaks <- c(110, 75, 25, 5, 1)
  sr_max <- fna_scale
  legend_title <- "Species\nrichness"
    
  ras_v10(raster,    title,  family, 
          sr_breaks, sr_max, legend_title)
  
# predicted map
  raster <- fna_pred
  title <- "FNA_predicted"
  family <- "Fabaceae sans Acacia"
  
  sr_breaks <- c(110, 75, 25, 5, 1)
  sr_max <- fna_scale
  legend_title <- "Species\nrichness"
  
  ras_v10(raster,    title,  family, 
          sr_breaks, sr_max, legend_title)

# invasion potentital -------------------------------------------------------  
# note: version 9_po maintains scaled legend to 0 and 1
  ras_v9_po <- function(raster, title, sr_breaks, legend_title){
    
  # spatial points dataframe
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
    
  # km scale bar
    km_scale <- raster_df %>% rename(lat = y, long = x)
    km_pos <- as.vector(data.frame(x = 130,
                                   y = -40))
    
  # pretty colours
    colr <- rev(brewer.pal(11, "Spectral"))
  # save
    save_txt <- paste0("results/maps/scaled/", title, ".jpeg")
    
  # plot
    q <- ggplot() +
      ggtitle(title) +
      geom_polygon(data = oz, aes(x = long, y = lat, group = group),
                   fill = "grey60") +
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +
      geom_polygon(data = oz, colour = "grey1",
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.7) +
      scale_fill_gradientn(colours = colr,
                           limits = c(0, 1),
                           breaks = sr_breaks,
                           labels = sr_breaks,
                           space = "Lab",
                           name = legend_title) +
      coord_fixed(ratio = 38/33, xlim = c(112, 155), ylim = c(-45, -7)) +
      theme_map() +
      theme(legend.direction = "vertical",
            legend.justification = "right",
            legend.position = "right",
            legend.key.size = unit(18, "cm"),
            legend.key.width = unit(1,"cm"),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            legend.box.spacing = unit(0.01, "cm"),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5)) +
      guides(fill = guide_colorbar(barheight = unit(5, "cm")))
      # ggsn::scalebar(km_scale, dist = 500, dist_unit = "km",  st.size = 4, st.dist = 0.05, st.bottom = T, height = 0.05, transform = T, model = 'WGS84', location = "topright", anchor = km_pos)
      plot(q)
    
    # save 
    save_1 <- paste0("results/maps/scaled/", title, ".jpeg")  
    save_2 <- paste0("Results/major figures by family/", family, "/", title, ".jpeg")
    ggsave(save_1, plot = last_plot(), height = 15, width = 15, units = "cm", dpi = 500, device = "jpeg")
    ggsave(save_2, plot = last_plot(), height = 15, width = 15, units = "cm", dpi = 500, device = "jpeg")
    
  } # finish function
  
  
# potential by predicted native maps
  raster <- fna_pot
  title <- "FNA_potential"
  family <- "Fabaceae sans Acacia"
  
  sr_breaks <- c(0, 0.5, 1)
  legend_title <- "Scaled\nlog-\nspecies\nrichness"
    
    ras_v9_po(raster, title, sr_breaks, legend_title)
    
    
# potential by observed native maps
  raster <- fna_obs_pot
  title <- "FNA_potential_observed"
  sr_breaks <- c(0, 0.5, 1)
  legend_title <- "Scaled\nlog-\nspecies\nrichness"
    
  ras_v9_po(raster, title, sr_breaks, legend_title)
    
# --------------------------------------------------------------------------  
  
  