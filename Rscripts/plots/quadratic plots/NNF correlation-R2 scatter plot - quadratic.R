  
# library -------------------------------------------------------------
  library(tidyverse)
  library(ggrepel)
  
  rm(list = ls())
  
# data -----------------------------------------------------------------
# model results
  load("Data files/Rdata/quadratic_model_data.RData")
  
# 22 families
  names22 <- read.csv("Results/csv/summary statistics/study families.csv", stringsAsFactors = F)[1:22, 1]
  glimpse(names)
  prefix <- substr(names22, 1, 3)
  prefix
  prefix[18:19] <- c("C3", "C4") # Poaceae C3 and C4
  prefix

# family names by status (36)
  setwd("C:/Users/Hemming/Dropbox/NNF/Results/rasters/iNEXT")
  current.list <- list.files(pattern = ".grd")
  names36 <- gsub(pattern = ".grd$", "", current.list)
  setwd("C:/Users/Hemming/Dropbox/NNF")
  
# r2 
  r2 <- read.csv("Results/csv/NNFC correlates/r2.csv")  
  head(r2)
  
# quadratic r2 
  qr2 <- read.csv("Results/csv/NNFC correlates/quadratic r2.csv")  %>%
         rename(nat_qr2 = nat_r2) %>%
         dplyr::select(family, nat_qr2)
  
# correlation matrix
  cormat <- read.csv("Results/csv/NNFC correlates/correlation matrix.csv")
  
# NNFC - R2 plot ----------------------------------------------------
  ggplot(aes(x = nat_r2, y = correlation), data = cormat) +
    geom_point(shape = "circle", size = 3) +
    theme_bw() + 
    theme(title = element_text(size = 18),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 14),
          axis.text.y = element_text(colour = "black", size = 14),
          axis.ticks.length = unit(0.3, "cm"),
          axis.ticks = element_line(size = 1),
          legend.position = "none") +
    geom_hline(yintercept = 0.3, linetype="dashed", colour = "black", size = 1) +
    geom_vline(xintercept = 0.3, linetype="dashed", colour = "black", size = 1) +
    geom_text(x = -0.2,  y = 0.8,  label = "B", size = 8) + 
    geom_text(x =  0.35, y = 0.8,  label = "A", size = 8) + 
    geom_text(x = -0.2,  y = 0.25, label = "D", size = 8) + 
    geom_text(x =  0.35, y = 0.25, label = "C", size = 8) +
    scale_y_continuous(breaks = c(-0.1, 0.3, 0.7),
                       limits = c(-0.2, 0.8)) +
    scale_x_continuous(breaks = c(-0.1, 0.3, 0.7),
                       limits = c(-0.2, 0.8)) +
    labs(x = "Native R2",
         y = "Native-nonnative correlation") +
    geom_text_repel(aes(label = prefix), size = 4, 
                    vjust = 0.5,
                    hjust = -0.2,
                    point.padding = 0.25,
                    force = T) # for more info: https://stackoverflow.com/questions/15624656/label-points-in-geom-point
  
  ggsave("Results/correlation plots/NNFC quadratic R2.jpeg", plot = last_plot(), dpi = 500, width = 15, height = 14, units = "cm", device = "jpeg")
  
# r2 - quadratic R2 plot
  plot(qr2$nat_qr2 ~ r2$nat_r2) # interesting
  
  r2$nat_qr2 <- qr2$nat_qr2
  
  r2_cor <- paste0("r = ", round(cor(r2$nat_qr2, r2$nat_r2, method = "pearson"), 3))
  
# plot with family labels
  ggplot(aes(x = nat_r2, y = nat_qr2), data = r2) +
    geom_point(shape = "circle", size = 3) +
    theme_bw() + 
    theme(title = element_text(size = 18),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 14),
          axis.text.y = element_text(colour = "black", size = 14),
          axis.ticks.length = unit(0.3, "cm"),
          axis.ticks = element_line(size = 1),
          legend.position = "none") +
    geom_abline(intercept = 0, linetype = "dashed") +
    geom_text(x =  0, y = 0.9, label = r2_cor, size = 8) +
    scale_y_continuous(breaks = c(0, 0.4, 0.8),
                       limits = c(-0.1, 0.9)) +
    scale_x_continuous(breaks = c(0, 0.4, 0.8),
                       limits = c(-0.1, 0.9)) +
    labs(x = "Native R2",
         y = "Native quadratic R2") +
    geom_text_repel(aes(label = prefix), size = 4, 
                    vjust = 0.5,
                    hjust = -0.2,
                    point.padding = 0.25,
                    force = T) # for more info: https://stackoverflow.com/questions/15624656/label-points-in-geom-point
  
  ggsave("Results/correlation plots/r2 comparison.jpeg", plot = last_plot(), dpi = 500, width = 15, height = 14, units = "cm", device = "jpeg")
  

# -----------------------------------------------------------------------------