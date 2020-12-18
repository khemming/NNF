  
# library -------------------------------------------------------------
  library(tidyverse)
  library(ggrepel)
  
  rm(list = ls())
  
# data -----------------------------------------------------------------
# native-nonnative family (nnf) correlation
  nnf <- read.csv("Results/csv/correlates/NNF correlation.csv", stringsAsFactors = F)
  head(nnf) 

# model-selection R2
  r2 <- read.csv("Results/csv/correlates/model selection r2.csv", stringsAsFactors = F)
  head(r2)

  dat <- left_join(nnf, r2, by = "family") %>%
         mutate(prefix = substr(family, 1, 3)) # family labels for plot
  dat$prefix[c(18, 19)] <- c("C3", "C4")      # special C3/C4 cases
  dat$colr <- ifelse(dat$correlation > 0.3 & dat$nat_r2 > 0.4, "A",
              ifelse(dat$correlation > 0.3 & dat$nat_r2 < 0.4, "B",
              ifelse(dat$correlation < 0.3 & dat$nat_r2 > 0.4, "C",
              "D")))
  
  
  
# quad-linear R2 plot ----------------------------------------------------
  cor(dat$correlation, dat$nat_r2, method = "pearson")
  
  ggplot(aes(x = nat_r2, y = correlation, col = colr), data = dat) +
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
          legend.position = "NA") +
    geom_hline(yintercept = 0.3, linetype="dashed", colour = "black", size = 0.75) +
    geom_vline(xintercept = 0.4, linetype="dashed", colour = "black", size = 0.75) +
    geom_text(x =  0.02, y = 0.7, label = "B", size = 8, colour = "blue") + 
    geom_text(x =  0.43, y = 0.7, label = "A", size = 8, colour = "darkgreen") + 
    geom_text(x =  0.02, y = 0.25, label = "D", size = 8, colour = "darkred") + 
    geom_text(x =  0.43, y = 0.25, label = "C", size = 8, colour = "#E69F22") +
    scale_y_continuous(breaks = seq(-0.1, 0.7, 0.2)) +
    scale_x_continuous(breaks = seq(0, 0.8, 0.2)) +
    scale_colour_manual(values = c("darkgreen", "blue", "#E69F22", "darkred")) +
    labs(x = "Native R2",
         y = "Native-nonnative correlation") +
    geom_text_repel(aes(label = prefix), size = 4, 
                    vjust = 0,
                    hjust = 0,
                    point.padding = 0.1,
                    force = T) # for more info: https://stackoverflow.com/questions/15624656/label-points-in-geom-point
  ggsave("Results/correlation plots/NNFC R2.jpeg", plot = last_plot(), dpi = 500, width = 15, height = 14, units = "cm", device = "jpeg")
  
# save as table ----------------------------------------------
  dat2 <- dat %>% 
          select(colr, family, prefix, correlation, nat_r2, nonnat_r2) %>%
          arrange(colr, desc(correlation))
  
  dat2
  write.csv(dat2, "Results/csv/correlates/NNFC r2 by A-D group.csv", row.names = F)
# -------------------------------------------------------------