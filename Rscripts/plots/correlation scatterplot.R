
# library -------------------------------------------------------------
  library(tidyverse)
  library(ggrepel)
  
  rm(list = ls())
  
# data -----------------------------------------------------------------
# model results
  load("Data files/Rdata/linear_model_data.RData")
  
# 22 families
  names22 <- read.csv("Results/csv/family summary statistics/study families.csv", stringsAsFactors = F)[1:22, 1]
  glimpse(names22)
  
# names by status (44)
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/iNEXT")
  current.list <- list.files(pattern = ".grd")
  names36 <- gsub(pattern = ".grd$", "", current.list)
  setwd("C:/Users/s436862/Dropbox/NNF")
  
# correlation matrix
  cormat <- read.csv("Results/csv/NNFC correlates/NNFC.csv") %>%
         arrange(correlation)
  cormat$family <- factor(cormat$family, levels = cormat$family)
  
# individual family species richness estimates by status
  nat_fam <- fpv %>% dplyr::select(contains("_native"))
  nonnat_fam <- fpv %>% dplyr::select(contains("_nonnative"))
  
# NNFC plot ----------------------------------------------------
  q <- ggplot(aes(x = family, y = correlation), data = cormat) +
    geom_point(shape = "circle", size = 3) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(colour = "black", size = 14),
          axis.ticks.length = unit(0.3, "cm"),
          axis.ticks = element_line(size = 1),
          legend.position = "none") +
    geom_hline(yintercept = 0, linetype="dashed", colour = "black", size = 1) +
    scale_y_continuous(breaks = c(-0.2, 0, 0.4, 0.8),
                       limits = c(-0.2, 0.8)) +
    labs(x = "Families",
         y = "Native-nonnative correlation") +
    theme(title = element_text(size = 18))
  plot(q)
  ggsave("Results/correlation plots/NNFC.jpeg", plot = last_plot(), dpi = 500, width = 15, height = 14, units = "cm", device = "jpeg")
  
# family-specific correlation plots -----------------------------------
# note: expand might need tweaking
  corr_fun <- function(dat, title, xlab, ylab, label, save1, save2) {
    q <- ggplot(aes(x = x, y = y), data = dat) +
      geom_point(shape = "circle", size = 2) +
      theme_bw() + 
      labs(x = xlab, 
           y = ylab,
           title = title) +
      theme(panel.border = element_blank(),
            plot.title = element_text(size = 22),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            panel.grid = element_blank(), 
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 18),
            axis.text.y = element_text(colour = "black", size = 18),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(colour = "black", size = 1)) +
      scale_x_continuous(breaks = c(0, 0.5, 1), 
                         limits = c(0, 1), 
                         expand = c(0, 0.05)) +
      scale_y_continuous(breaks = c(0, 0.5,  1), 
                         limits = c(0, 1), 
                         expand = c(0, 0.05)) +
      annotate("text", x = 0.2, y = 1, label = label, size = 10) 
    
    ggsave(save1, plot = last_plot(), dpi = 500, width = 15, height = 15, units = "cm", device = "jpeg")  
    ggsave(save2, plot = last_plot(), dpi = 500, width = 15, height = 15, units = "cm", device = "jpeg")  
    plot(q)
  }
 
# -------------------------------------------------------  
# run for all families
  for (i in 1:length(names22)){
    
  # data
    x <- unlist(nat_fam[i])
    y <- unlist(nonnat_fam[i])
    dat <- data.frame(cbind(x, y))
  # labels
    title <- paste(names22[i], "correlation")
    xlab <- "Native richness (scaled)"
    ylab <- "Nonnative richness (scaled)" 
  # correlation
    cor <- sprintf("%.2f", round(cor(x, y, use = "complete.obs", method = "spearman"), 2))
    label <- paste0("r = ", cor)
  # save
    save1 <- paste0("Results/correlation plots/", title, ".jpeg")
    save2 <- paste0("Results/major figures by family/", names22[i], "/correlation.jpeg")
  # run  
    corr_fun(dat, title, xlab, ylab, label, save1, save2)
    
  }
  
# ---------------------------------------------------------------------  