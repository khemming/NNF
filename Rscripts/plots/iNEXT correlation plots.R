##########################################################
# raw correlation plots
##########################################################

# scope --------------------------------------------------
# correlate observed native and non-native richness for the most common non-native plant families in Australia

# library ------------------------------------------------
  library(tidyverse)
  library(ggThemeAssist)
  library(raster)

# data ---------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/iNEXT")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  spp <- as.data.frame(c.stack, na.rm = F)
  glimpse(spp)
  
  setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/correlation plots/iNEXT")

# correlation plots ------------------------------------------

# Asteraceae --------------------------------------------------
# family
  nat <- spp$Asteraceae.native
  non.nat <- spp$Asteraceae.non.native
  tlab <- "Asteraceae"
  xlab <- "Native richness"
  ylab <- "Non-native richness" 
  save <- "Asteraceae.jpeg"
  
# axes scales  
  sp <- data.frame(cbind(nat, non.nat))
  sp2 <- sp %>% filter(!is.na(nat) & !is.na(non.nat))
  x.scale <- max(sp2$nat)
  x.scale # adjust breaks accordingly
  x.breaks <- c(10, 30, 50, 70) 
  
  y.scale <- max(sp2$nat) 
  y.scale # adjust breaks accordingly
  y.breaks <- c(10, 30, 50, 70) 
  
# correlation scores  
  cor <- round(cor(nat, non.nat, use = "complete.obs", method = "spearman"), 2)
  cor 
  cor.label <- paste0("r = ", cor)
  
# plot -------------------------------------------
  ggplot(aes(x = nat, y = non.nat), data = spp) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = x.breaks, 
                       limits = c(1, x.scale), 
                       expand = c(0, 0.5)) +
    scale_y_continuous(breaks = y.breaks, 
                       limits = c(1, y.scale), 
                       expand = c(0, 0.5)) +
    #geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 65, y = 37, 
             label = cor.label, size = 5) +
    labs(title = tlab,
         x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18),
          title = element_text(size = 24))
  
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
# ------------------------------------------------------------------  
  
# -------------------------------------------------------- 
  
# Brassicaceae --------------------------------------------------
# family
  nat <- spp$Brassicaceae.native
  non.nat <- spp$Brassicaceae.non.native
  tlab <- "Brassicaceae"
  xlab <- "Native richness"
  ylab <- "Non-native richness" 
  save <- "Brassicaceae.jpeg"
  
# axes scales  
  sp <- data.frame(cbind(nat, non.nat))
  sp2 <- sp %>% filter(!is.na(nat) & !is.na(non.nat))
  x.scale <- max(sp2$nat)
  x.scale # adjust breaks accordingly
  x.breaks <- c(5, 10, 15, 20) 
  
  y.scale <- max(sp2$nat) 
  y.scale # adjust breaks accordingly
  y.breaks <- c(5, 10, 15, 20) 
  
# correlation scores  
  cor <- round(cor(nat, non.nat, use = "complete.obs", method = "spearman"), 2)
  cor 
  cor.label <- paste0("r = ", cor)
  
  # plot -------------------------------------------
  ggplot(aes(x = nat, y = non.nat), data = spp) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = x.breaks, 
                       limits = c(1, x.scale), 
                       expand = c(0, 0.5)) +
    scale_y_continuous(breaks = y.breaks, 
                       limits = c(1, y.scale), 
                       expand = c(0, 0.5)) +
    #geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 15, y = 17, 
             label = cor.label, size = 5) +
    labs(title = tlab,
         x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18),
          title = element_text(size = 24))
  
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
# ------------------------------------------------------------------  
  
# Cyperaceae --------------------------------------------------
# family
  nat <- spp$Cyperaceae.native
  non.nat <- spp$Cyperaceae.non.native
  tlab <- "Cyperaceae"
  xlab <- "Native richness"
  ylab <- "Non-native richness" 
  save <- "Cyperaceae.jpeg"
  
# axes scales  
  sp <- data.frame(cbind(nat, non.nat))
  sp2 <- sp %>% filter(!is.na(nat) & !is.na(non.nat))
  x.scale <- max(sp2$nat)
  x.scale # adjust breaks accordingly
  x.breaks <- c(20, 40, 60, 80) 
  
  y.scale <- max(sp2$nat) 
  y.scale # adjust breaks accordingly
  y.breaks <- c(20, 40, 60, 80) 
  
# correlation scores  
  cor <- round(cor(nat, non.nat, use = "complete.obs", method = "spearman"), 2)
  cor 
  cor.label <- paste0("r = ", cor)
  
  # plot -------------------------------------------
  ggplot(aes(x = nat, y = non.nat), data = spp) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = x.breaks, 
                       limits = c(1, x.scale), 
                       expand = c(0, 2)) +
    scale_y_continuous(breaks = y.breaks, 
                       limits = c(1, y.scale), 
                       expand = c(0, 3)) +
    #geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 65, y = 37, 
             label = cor.label, size = 5) +
    labs(title = tlab,
         x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18),
          title = element_text(size = 24))
  
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
# ------------------------------------------------------------------  
  
# Fabaceae --------------------------------------------------
# family
  nat <- spp$Fabaceae.native
  non.nat <- spp$Fabaceae.non.native
  tlab <- "Fabaceae"
  xlab <- "Native richness"
  ylab <- "Non-native richness" 
  save <- "Fabaceae.jpeg"
  
# axes scales  
  sp <- data.frame(cbind(nat, non.nat))
  sp2 <- sp %>% filter(!is.na(nat) & !is.na(non.nat))
  x.scale <- max(sp2$nat)
  x.scale # adjust breaks accordingly
  x.breaks <- c(1, 40, 80, 120) 
  
  y.scale <- max(sp2$nat) 
  y.scale # adjust breaks accordingly
  y.breaks <- c(1, 40, 80, 120) 
  
# correlation scores  
  cor <- round(cor(nat, non.nat, use = "complete.obs", method = "spearman"), 2)
  cor 
  cor.label <- paste0("r = ", cor)
  
# plot -------------------------------------------
  ggplot(aes(x = nat, y = non.nat), data = spp) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = x.breaks, 
                       limits = c(1, x.scale), 
                       expand = c(0, 0.5)) +
    scale_y_continuous(breaks = y.breaks, 
                       limits = c(1, y.scale), 
                       expand = c(0, 3)) +
    #geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 100, y = 100, 
             label = cor.label, size = 5) +
    labs(title = tlab,
         x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18),
          title = element_text(size = 24))
  
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
# ------------------------------------------------------------------  
  
# Malvaceae --------------------------------------------------
# family
  nat <- spp$Malvaceae.native
  non.nat <- spp$Malvaceae.non.native
  tlab <- "Malvaceae"
  xlab <- "Native richness"
  ylab <- "Non-native richness" 
  save <- "Malvaceae.jpeg"

# axes scales  
  sp <- data.frame(cbind(nat, non.nat))
  sp2 <- sp %>% filter(!is.na(nat) & !is.na(non.nat))
  x.scale <- max(sp2$nat)
  x.scale # adjust breaks accordingly
  x.breaks <- c(5, 20, 35, 50) 
  
  y.scale <- max(sp2$nat) 
  y.scale # adjust breaks accordingly
  y.breaks <- c(5, 20, 35, 50) 
  
# correlation scores  
  cor <- round(cor(nat, non.nat, use = "complete.obs", method = "spearman"), 2)
  cor 
  cor.label <- paste0("r = ", cor)
  
  # plot -------------------------------------------
  ggplot(aes(x = nat, y = non.nat), data = spp) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = x.breaks, 
                       limits = c(1, x.scale), 
                       expand = c(0, 3)) +
    scale_y_continuous(breaks = y.breaks, 
                       limits = c(1, y.scale), 
                       expand = c(0, 3)) +
    #geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 40, y = 25, 
             label = cor.label, size = 5) +
    labs(title = tlab,
         x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18),
          title = element_text(size = 24))
  
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
# ------------------------------------------------------------------  
  
# Myrtaceae --------------------------------------------------
# family
  nat <- spp$Myrtaceae.native
  non.nat <- spp$Myrtaceae.non.native
  tlab <- "Myrtaceae"
  xlab <- "Native richness"
  ylab <- "Non-native richness" 
  save <- "Myrtaceae.jpeg"
  
# axes scales  
  sp <- data.frame(cbind(nat, non.nat))
  sp2 <- sp %>% filter(!is.na(nat) & !is.na(non.nat))
  x.scale <- max(sp2$nat)
  x.scale # adjust breaks accordingly
  x.breaks <- c(1, 30, 60, 90) 
  
  y.scale <- max(sp2$nat) 
  y.scale # adjust breaks accordingly
  y.breaks <- c(1, 30, 60, 90) 
  
# correlation scores  
  cor <- round(cor(nat, non.nat, use = "complete.obs", method = "spearman"), 2)
  cor 
  cor.label <- paste0("r = ", cor)
  
  # plot -------------------------------------------
  ggplot(aes(x = nat, y = non.nat), data = spp) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = x.breaks, 
                       limits = c(1, x.scale), 
                       expand = c(0, 0.5)) +
    scale_y_continuous(breaks = y.breaks, 
                       limits = c(1, y.scale), 
                       expand = c(0, 3)) +
    #geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 65, y = 37, 
             label = cor.label, size = 5) +
    labs(title = tlab,
         x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18),
          title = element_text(size = 24))
  
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
# ------------------------------------------------------------------  
  
  # Plantae --------------------------------------------------
  # family
  nat <- spp$Plantae.native
  non.nat <- spp$Plantae.non.native
  tlab <- "Plantae"
  xlab <- "Native richness"
  ylab <- "Non-native richness" 
  save <- "Plantae.jpeg"
  
  # axes scales  
  sp <- data.frame(cbind(nat, non.nat))
  sp2 <- sp %>% filter(!is.na(nat) & !is.na(non.nat))
  x.scale <- max(sp2$nat)
  x.scale # adjust breaks accordingly
  x.breaks <- c(300, 600, 900, 1200) 
  
  y.scale <- max(sp2$nat) 
  y.scale # adjust breaks accordingly
  y.breaks <- c(300, 600, 900, 1200) 
  
  # correlation scores  
  cor <- round(cor(nat, non.nat, use = "complete.obs", method = "spearman"), 2)
  cor 
  cor.label <- paste0("r = ", cor)
  
  # plot -------------------------------------------
  ggplot(aes(x = nat, y = non.nat), data = spp) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = x.breaks, 
                       limits = c(1, x.scale), 
                       expand = c(0, 3)) +
    scale_y_continuous(breaks = y.breaks, 
                       limits = c(1, y.scale), 
                       expand = c(0, 30)) +
    #geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 1000, y = 800, 
             label = cor.label, size = 5) +
    labs(title = tlab,
         x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18),
          title = element_text(size = 24))
  
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
# ------------------------------------------------------------------  
  
# Solanaceae --------------------------------------------------
# family
  nat <- spp$Solanaceae.native
  non.nat <- spp$Solanaceae.non.native
  tlab <- "Solanaceae"
  xlab <- "Native richness"
  ylab <- "Non-native richness" 
  save <- "Solanaceae.jpeg"
  
# axes scales  
  sp <- data.frame(cbind(nat, non.nat))
  sp2 <- sp %>% filter(!is.na(nat) & !is.na(non.nat))
  x.scale <- max(sp2$nat)
  x.scale # adjust breaks accordingly
  x.breaks <- c(1, 5, 10, 15) 
  
  y.scale <- max(sp2$nat) 
  y.scale # adjust breaks accordingly
  y.breaks <- c(1, 5, 10, 15) 
  
# correlation scores  
  cor <- round(cor(nat, non.nat, use = "complete.obs", method = "spearman"), 2)
  cor 
  cor.label <- paste0("r = ", cor)
  
  # plot -------------------------------------------
  ggplot(aes(x = nat, y = non.nat), data = spp) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = x.breaks, 
                       limits = c(1, x.scale), 
                       expand = c(0, 0.5)) +
    scale_y_continuous(breaks = y.breaks, 
                       limits = c(1, y.scale), 
                       expand = c(0, 0.5)) +
    #geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 14, y = 15, 
             label = cor.label, size = 5) +
    labs(title = tlab,
         x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18),
          title = element_text(size = 24))
  
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
# ------------------------------------------------------------------  
  
  
# ------------------------------------------------------  
  
  
  

  
  
  