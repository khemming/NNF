
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
names22 <- read.csv("Results/csv/summary statistics/study families.csv", stringsAsFactors = F)[1:22, 1]
head(names22)
names44 <- rep(names22, each = 2)


# # iNEXT richness for scale bar
#   setwd("C:/Users/Hemming/Dropbox/NNF/Results/rasters/iNEXT")
#   current.list <- list.files(pattern = ".grd")
#   inext_names <- gsub(pattern = "\\.grd$", "", current.list)
#   inext <- stack(current.list)
#   names(inext) <- inext_names
#   list2env(setNames(unstack(inext), names(inext)), .GlobalEnv)
# st.scale <- stack(native_C3, native_C4, native_total, 
#                   native_C3, native_C4, native_total)

# quadratic predicted  
setwd("C:/Users/Hemming/Dropbox/NNF/Results/rasters/quadratic rasters")
current.list <- list.files(pattern = "_predicted.grd")
names_long <- gsub(pattern = "\\.grd$", "", current.list)
q.pr.stack <- stack(current.list)
names(q.pr.stack) <- names_long

# quadratic potential  
setwd("C:/Users/Hemming/Dropbox/NNF/Results/rasters/quadratic rasters")
current.list <- list.files(pattern = "_potential.grd")
names_long <- gsub(pattern = "\\.grd$", "", current.list)
q.po.stack <- stack(current.list)
names(q.po.stack) <- names_long

setwd("C:/Users/Hemming/Dropbox/NNF")

# scaled maps (v8) ---------------------------------------------------------
ras_v8 <- function(raster, title, family){
  
  # spatial points dataframe 
  raster_spdf <- as(raster, "SpatialPixelsDataFrame")
  raster_df <- as.data.frame(raster_spdf)
  colnames(raster_df) <- c("value", "x", "y")
  # pretty colours
  colr <- rev(brewer.pal(11, "Spectral"))
  
  
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
                         breaks = c(0, 0.5, 1),
                         labels = c(0, 0.5, 1),
                         space = "Lab",
                         name = "Scaled\nlog-\nrichness") +
    coord_fixed(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
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
  save_1 <- paste0("results/maps/quadratic/", title, ".jpeg")  
  save_2 <- paste0("Results/major figures by family/", family, "/", title, "_quadratic", ".jpeg")
  ggsave(save_1, plot = last_plot(), height = 12, width = 15, units = "cm", dpi = 500, device = "jpeg")
  ggsave(save_2, plot = last_plot(), height = 12, width = 15, units = "cm", dpi = 500, device = "jpeg")
  
  
  
  
} # finish function

# run ------------------------------------------------------------------------------
# quadratic predicted maps
for (i in 1:length(names(q.pr.stack))) {
  
  raster <- q.pr.stack[[i]]
  title <- paste0(names(q.pr.stack)[i])
  family <- names44[i]
  
  ras_v8(raster, title, family)
  
}

# quadratic potential maps
for (i in 1:length(names(q.po.stack))) {
  
  raster <- q.po.stack[[i]]
  title <- paste0(names(q.po.stack)[i])
  family <- names22[i]
  
  ras_v8(raster, title, family)
  
}

# --------------------------------------------------------------------