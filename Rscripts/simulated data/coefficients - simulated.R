

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
  
# similar relationships (A) -------------------------------------------------
# data
  nat <- data.frame(pv =       c("Temperature", "Precipitation", "Human\nimpact"),
                    ci_low =   c(-20,           10,              -5),
                    estimate = c(-15,           15,               0),
                    ci_high =  c(-10,           20,               5))
  
  nnat_sim <- data.frame(pv =   c("Temperature", "Precipitation", "Human\nimpact"),
                     ci_low =   c(-19,           11,               -4),
                     estimate = c(-14,           16,                1),
                     ci_high =  c(-9,            21,                 6))
  fct_inorder(rev(nat$pv))
  fct_inorder(rev(nnat_sim$pv))
  
# plot
  ggplot(nat, aes(y = pv), width = 0.1) +
    theme_classic() +
    geom_vline(aes(xintercept = 0),colour = "black", size = 0.9, linetype = "dashed") +
    geom_point(data = nat, aes(x = estimate), colour = "blue", 
                                              size = 7, position = position_nudge(y = -0.11)) +
               geom_errorbarh(data = nat, aes(xmin = ci_low, xmax = ci_high), 
               size = 2, height = 0, colour = "blue", position = position_nudge(y = -0.11)) +
    geom_point(data = nnat_sim, aes(x = estimate), colour = "red", 
                                               size = 7, position = position_nudge(y = 0.11)) +
               geom_errorbarh(data = nnat_sim, aes(xmin = ci_low, xmax = ci_high), 
               size = 2, height = 0, colour = "red", position = position_nudge(y = 0.11)) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 2.7, ymax = 3.3, 
              size = 0.5, fill = "red", alpha = 0.05) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 1.7, ymax = 2.3, 
              size = 0.5, fill = "blue", alpha = 0.05) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 0.7, ymax = 1.3, 
              size = 0.5, fill = "grey1", alpha = 0.05) +
    labs(x = "Mean estimate",
       y = "",
       title = "") +
    scale_x_continuous(limits = c(-23, 23),
                       breaks = c(-16, 0, 16),
                       labels = c("Negative", "0", "Positive")) +
    theme(axis.ticks = element_blank(),
          legend.title = element_text(size = 18),
          legend.position = "right", 
          axis.title = element_blank(),
          plot.title = element_text(size = 20),
          axis.line = element_line(colour = "black", size = 1.5),
          axis.text.x = element_text(colour = "black", size = 18, vjust = 67),
          axis.text.y = element_text(colour = "black", size = 18))
  
    ggsave("Results/simulated results/similar coefficients A.jpeg", plot = last_plot(), width = 12, height = 10, units = "cm", dpi = 500, device = "jpeg")
  
# dissimilar relationships (B, D) ------------------------------------------------
# data
  nat <- data.frame(pv =          c("Temperature", "Precipitation", "Human\nimpact"),
                    ci_low =      c(-8,             -3,              -5),
                    estimate =    c(-3,              2,               0),
                    ci_high =     c( 2,              7,               5))
  nnat_dissim <- data.frame(pv =  c("Temperature", "Precipitation", "Human\nimpact"),
                       ci_low =   c(-3,             -2,              10),
                       estimate = c( 2,              3,              15),
                       ci_high =  c( 7,              8,              20))
  fct_inorder(rev(nat$pv))
  fct_inorder(rev(nnat_dissim$pv)) 
  
  ggplot(nat, aes(y = pv), width = 0.1) +
    theme_classic() +
    geom_vline(aes(xintercept = 0),colour = "black", size = 0.9, linetype = "dashed") +
    geom_point(data = nat, aes(x = estimate), colour = "blue", 
                                                   size = 7, position = position_nudge(y = -0.11)) +
    geom_errorbarh(data = nat, aes(xmin = ci_low, xmax = ci_high), 
                   size = 2, height = 0, colour = "blue", position = position_nudge(y = -0.11)) +
    geom_point(data = nnat_dissim, aes(x = estimate), colour = "red", 
                                                   size = 7, position = position_nudge(y = 0.11)) +
    geom_errorbarh(data = nnat_dissim, aes(xmin = ci_low, xmax = ci_high), 
                   size = 2, height = 0, colour = "red", position = position_nudge(y = 0.11)) +
    labs(x = "Mean estimate",
         y = "") +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 2.7, ymax = 3.3, 
              size = 0.5, fill = "red", alpha = 0.05) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 1.7, ymax = 2.3, 
              size = 0.5, fill = "blue", alpha = 0.05) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 0.7, ymax = 1.3, 
              size = 0.5, fill = "grey1", alpha = 0.05) +
    scale_x_continuous(limits = c(-23, 23)) +
    theme(axis.ticks = element_blank(),
          legend.title = element_text(size = 18),
          legend.position = "right", 
          axis.title = element_blank(),
          plot.title = element_text(size = 22, face = "bold"),
          axis.line = element_line(colour = "black", size = 1.5),
          axis.text.x = element_blank(),
          axis.text.y = element_text(colour = "black", size = 18))
  
  
    ggsave("Results/simulated results/dissimilar coefficients B, D.jpeg", plot = last_plot(), width = 12, height = 10, units = "cm", dpi = 500, device = "jpeg")
    
    
# dissimilar relationships (C) ---------------------------------------
# data
  nat <- data.frame(pv =          c("Temperature", "Precipitation", "Human\nimpact"),
                    ci_low =      c(-20,           10,              -5),
                    estimate =    c(-15,           15,               0),
                    ci_high =     c(-10,           20,               5))  
  nnat_dissim <- data.frame(pv =  c("Temperature", "Precipitation", "Human\nimpact"),
                            ci_low =   c(-9,            -1,              10),
                            estimate = c(-4,             4,              15),
                            ci_high =  c( 1,             9,              20))
  fct_inorder(rev(nat$pv))
  fct_inorder(rev(nnat_dissim$pv)) 
  
# plot  
  ggplot(nat, aes(y = pv), width = 0.1) +
    theme_classic() +
    geom_vline(aes(xintercept = 0),colour = "black", size = 0.9, linetype = "dashed") +
    geom_point(data = nat, aes(x = estimate), colour = "blue", 
               size = 7, position = position_nudge(y = -0.11)) +
    geom_errorbarh(data = nat, aes(xmin = ci_low, xmax = ci_high), 
                   size = 2, height = 0, colour = "blue", position = position_nudge(y = -0.11)) +
    geom_point(data = nnat_dissim, aes(x = estimate), colour = "red", 
               size = 7, position = position_nudge(y = 0.11)) +
    geom_errorbarh(data = nnat_dissim, aes(xmin = ci_low, xmax = ci_high), 
                   size = 2, height = 0, colour = "red", position = position_nudge(y = 0.11)) +
    labs(x = "Mean estimate",
         y = "") +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 2.7, ymax = 3.3, 
              size = 0.5, fill = "red", alpha = 0.05) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 1.7, ymax = 2.3, 
              size = 0.5, fill = "blue", alpha = 0.05) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 0.7, ymax = 1.3, 
              size = 0.5, fill = "grey1", alpha = 0.05) +
    scale_x_continuous(limits = c(-23, 23)) +
    theme(axis.ticks = element_blank(),
          legend.title = element_text(size = 18),
          legend.position = "right", 
          axis.title = element_blank(),
          plot.title = element_text(size = 22, face = "bold"),
          axis.line = element_line(colour = "black", size = 1.5),
          axis.text.x = element_blank(),
          axis.text.y = element_text(colour = "black", size = 18))
  
  
  ggsave("Results/simulated results/dissimilar coefficients C.jpeg", plot = last_plot(), width = 12, height = 10, units = "cm", dpi = 500, device = "jpeg")
# ---------------------------------------------------------------------