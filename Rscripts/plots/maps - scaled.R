

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
  
# 22 families
  names22 <- read.csv("Results/csv/family summary statistics/study families.csv", stringsAsFactors = F)[1:22, 1]
  head(names22)
  names44 <- rep(names22, each = 2)
  
# iNEXT richness
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/iNEXT")
  current.list <- list.files(pattern = ".grd")
  inext_names <- gsub(pattern = "\\.grd$", "", current.list)
  st.scale <- stack(current.list)
  names(st.scale) <- inext_names
  
# observed 
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/scaled")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  ob.stack <- stack(current.list)
  names(ob.stack) <- names_long
  
# predicted  
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/predicted")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  pr.stack <- stack(current.list)
  names(pr.stack) <- names_long
  
# potential by predicted native
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/potential")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  po.stack <- stack(current.list)
  names(po.stack) <- names_long
  
# potential by observed native
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/potential by observed native")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  ob.po.stack <- stack(current.list)
  names(ob.po.stack) <- names_long
  
  setwd("C:/Users/s436862/Dropbox/NNF")

# scale bar --------------------------------------------------------------
# iNEXT species richness to scale map legends
  j <- vector()
  
  for (i in 1:length(names44)){
    
    j[i] <- ceiling(cellStats(st.scale[[i]], "max", na.rm = T))
    
  }
  j
                   # 1 - 5                 # max:
  leg_lab <- list(c(35,  25, 10,  5, 1),   # 32
                  c(15,  10,  5, 1),       # 13
                  c(15,  10,  5, 1),       # 15
                  c(10,   5, 1),           # 8
                  c(35,  20, 10,  5, 1),   # 35  
                  # 6 - 10     
                  c(8,    5,  3, 1),       # 8
                  c(30,  20,  5, 1),       # 30
                  c(15,  10,  5, 1),       # 11
                  c(90,  60, 30, 10, 1),   # 88
                  c(50,  35, 15, 5, 1),    # 46  
                  # 11 - 15     
                  c(20,  15,  5, 1),       # 18
                  c(12,  8,   5, 1),       # 11
                  c(25,  15,  5, 1),       # 21
                  c(25,  15,  5, 1),       # 25
                  c(75,  50,  25, 10, 5),  # 73  
                  # 16 - 20     
                  c(8,    5,  3, 1),       # 6
                  c(30,  20,  5, 1),       # 28
                  c(10,   5, 1),           # 10
                  c(100,  75, 40, 20, 5),  # 98
                  c(12,  8,   5, 1),       # 11
                  # 21 - 25    
                  c(50,  35, 15, 5, 1),    # 47
                  c(5,   3,  1),           # 5
                  c(35,  20, 10,  5, 1),   # 34
                  c(12,  8,   5, 1),       # 12
                  c(150, 100, 50, 20, 5),  # 144  
                  # 26 - 30     
                  c(55,  40, 20, 5, 1),    # 55
                  c(25,  15,  5, 1),       # 23
                  c(8,    5,  3, 1),       # 8
                  c(30,  20,  5, 1),       # 30
                  c(20,  15,  5, 1),       # 16  
                  # 31 - 35     
                  c(60,  40, 20, 5, 1),    # 56
                  c(12,  8,   5, 1),       # 11
                  c(15,  10,  5, 1),       # 13
                  c(10,   5, 1),           # 9
                  c(55,  40, 20, 5, 1),    # 52  
                  # 36 - 40     
                  c(45,  25,  10, 5),      # 41
                  c(160, 120, 60, 20, 5),  # 155
                  c(50,  35, 15, 5, 1),    # 50
                  c(50,  35, 15, 5, 1),    # 46
                  c(8,    5,  3, 1),       # 7  
                  # 41 - 44     
                  c(35,  20, 10,  5, 1),  # 33
                  c(6,    3,  1),         # 6
                  c(20,  15,  5, 1),      # 20
                  c(20,  15,  5, 1))      # 17  
  
# scaled maps (v10) --------------------------------------------------------
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
    
    
  } # finish function
# adding north and scale bars
  pred_v10 <- function(raster, title, sr_breaks, sr_max, legend_title){
    
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
    
    # species richness colours
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
      ggsn::scalebar(km_scale, dist = 500, dist_unit = "km",  st.size = 4, st.dist = 0.05, st.bottom = T, height = 0.05, transform = T, model = 'WGS84', anchor = km_pos) +
      north(data = km_scale, symbol = 1, anchor = c(y = -37, x = 139),
            x.min = 115, x.max = 150, y.min = 0, y.max = -7, scale = 0.15) +
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
    
    ggsave(save_txt, plot = last_plot(), width = 15, height = 15, units = "cm", dpi = 500, device = "jpeg")
  } # finish
# maps v9 ---------------------------------------------------------------  
# raster = raster
# title = plot and save title
# family = family name
# sr_breaks = the raw species richness legend labels (i.e. leg_lab)
# sr_max = max species richness
# legend_title = legend units

# observed maps
  for (i in 1:length(names(ob.stack))) {
    
    raster <- ob.stack[[i]]
    title <- paste0(names(ob.stack)[i])
    family <- names44[i]
    
    sr_breaks <- leg_lab[[i]]
    sr_max <- st.scale[[i]]
    legend_title <- "Species\nrichness"
    
    ras_v9(raster,    title,  family, 
           sr_breaks, sr_max, legend_title)
  }
  
# predicted maps
  for (i in 1:length(names(pr.stack))) {
    
    raster <- pr.stack[[i]]
    title <- paste0(names(pr.stack)[i])
    family <- names44[i]
    
    sr_breaks <- leg_lab[[i]]
    sr_max <- st.scale[[i]]
    legend_title <- "Species\nrichness"
    
    ras_v9(raster,    title,  family, 
           sr_breaks, sr_max, legend_title)
    
  }
  
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
  for (i in 1:length(names(po.stack))) {
    
    raster <- po.stack[[i]]
    title <- paste0(names(po.stack)[i])
    family <- names22[i]
    sr_breaks <- c(0, 0.5, 1)
    legend_title <- "Scaled\nlog-\nspecies\nrichness"
    
    ras_v9_po(raster, title, sr_breaks, legend_title)
    
  }
  
# potential by observed native maps
  for (i in 1:length(names(ob.po.stack))) {
    
    raster <- ob.po.stack[[i]]
    title <- paste0(names(ob.po.stack)[i])
    sr_breaks <- c(0, 0.5, 1)
    legend_title <- "Scaled\nlog-\nspecies\nrichness"
    
    ras_v9_po(raster, title, sr_breaks, legend_title)
    
  }  

# --------------------------------------------------------------------------  
  
  