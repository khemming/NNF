##########################################################
# correlation plots
##########################################################

# scope --------------------------------------------------
# correlate observed native and non-native richness for the most common non-native plant families in Australia
# indivudally, and then a final one with all results included

# library ------------------------------------------------
  library(tidyverse)
  library(ggThemeAssist)
  library(raster)
  
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
  
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/correlation plots/scaled")

# correlation plots ------------------------------------------
# required: 
#          family (in quotes)
#          native column (from df "spp)
#     non-"                           "  
  cor.scaled <- function (family, native.col, non.native.col) {
  
  tlab <- family
  xlab <- "Native richness"
  ylab <- "Non-native richness" 
  save <- paste0(family, ".jpeg")
  
# axes scales  
  sp <- data.frame(cbind(native.col, non.native.col))
  sp2 <- sp %>% filter(!is.na(native.col) & !is.na(non.native.col))
  
# correlation scores  
  cor <- round(cor(native.col, non.native.col, use = "complete.obs", method = "spearman"), 2)
  cor.label <- paste0("r = ", cor)
  
# plot
  q <- ggplot(aes(x = native.col, y = non.native.col), data = spp) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 0.8, y = .95, 
             label = cor.label, size = 5) +
    labs(title = tlab,
         x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18),
          title = element_text(size = 24))
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  return(q)
  }
  
# -------------------------------------------------------- 
# required: 
#          family (in quotes)
#          native column (from df "spp)
#     non-"                           "  
  
# Asteraceae
  cor.scaled("Asteraceae",
             spp$Asteraceae.native,
             spp$Asteraceae.non.native)
  
# Brassicaceae
  cor.scaled("Brassicaceae",
             spp$Brassicaceae.native,
             spp$Brassicaceae.non.native)
  
# Cyperaceae
  cor.scaled("Cyperaceae",
             spp$Cyperaceae.native,
             spp$Cyperaceae.non.native)
  
# Fabaceae
  cor.scaled("Fabaceae",
             spp$Fabaceae.native,
             spp$Fabaceae.non.native)
  
# Malvaceae
  cor.scaled("Malvaceae",
             spp$Malvaceae.native,
             spp$Malvaceae.non.native)
  
# Myrtaceae
  cor.scaled("Myrtaceae",
             spp$Myrtaceae.native,
             spp$Myrtaceae.non.native)
  
# Plantae
  cor.scaled("Plantae",
             spp$Plantae.native,
             spp$Plantae.non.native)
  
# Proteaceae
  # cor.scaled("Asteraceae",
  #            spp$Asteraceae.native,
  #            spp$Asteraceae.non.native)
  # 

  
# Solanaceae
  cor.scaled("Solanaceae",
             spp$Solanaceae.native,
             spp$Solanaceae.non.native)

# ----------------------------------------------------------    
  
  