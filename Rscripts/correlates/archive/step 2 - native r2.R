
# to do: calculate R2 manually with predicted richness -- did i even predict richness?


# library -------------------------------------------------------------
  library(tidyverse)
  library(ggrepel)

  rm(list = ls())

# data ----------------------------------------------------------------

  
# correlation scores
  corr <- read.csv("Results/csv/summary statistics/correlation matrix.csv", header = T)
  head(corr)

# r2 (just obtaining native r2 values)
  nat_seq <- seq(1, nrow(corr)*2, 2)
  r2 <- read.csv("Results/csv/summary statistics/r2.csv", header = T) %>%
        slice(nat_seq) %>%
        dplyr::select(r2)
# data of first occurrence
  fo <- read.csv("Results/csv/summary statistics/date of first occurrence.csv") %>%
    mutate(scaled_fo = 2020 - first_occ) %>%
    dplyr::select(scaled_fo)
  
# family names and that 
  prefix <- read.csv("Results/csv/summary statistics/master family summary table.csv") %>%
    filter(study_family == "yes") %>%
    dplyr::select(prefix, total_rec)
  
  dat <- bind_cols(prefix, corr, r2, fo) %>%
    filter(prefix != "Pla") %>%
    mutate(col_col = c("three", "two",   "three", "three",
                       "one",   "one",   "three", "one",
                       "one",   "three", "one",   "two",
                       "two",   "three", "three", "one", 
                       "three", "two"))
  glimpse(dat)

# correlation label
  overall.corr <- cor(dat$correlation, dat$r2, method = "spearman")
  o_c <- format(round(overall.corr, 2), nsmall = 2)
  o_c
  cor.label <- paste0("r = ", o_c)
  
# plots -----------------------------------------------------------
# raw ----------------------------------------------
  ggplot(aes(x = r2, y = correlation), data = dat) +
    geom_point(shape = "circle", size = 5) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 2),
          axis.text.x = element_text(colour = "black", size = 16),
          axis.text.y = element_text(colour = "black", size = 16),
          legend.position = "none") +
    scale_x_continuous(breaks = seq(0.2, 0.8, 0.2),
                       limits = c(0.18, 0.8)) +
    scale_y_continuous(breaks = seq(-0.2, 0.8, 0.2),
                       limits = c(-0.2, 0.8)) +
  scale_colour_manual(values = c("darkgreen", "darkred", "#E69F22")) +
    labs(x = "Native R2",
         y = "Correlation of status") +
    geom_text_repel(aes(label = prefix), size = 4,
                    vjust = 0.5,
                    hjust = -0.2,
                    point.padding = 0.25,
                    force = T) + # for more info: https://stackoverflow.com/questions/15624656/label-points-in-geom-point
    theme(title = element_text(size = 28))
  
  
  ggsave("Results/correlation plots/scaled/grouped/r2 correlation raw.jpeg", plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
# coloured ----------------------------------------------
  ggplot(aes(x = r2, y = correlation, col = col_col), data = dat) +
    geom_point(shape = "circle", size = 5) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 2),
          axis.text.x = element_text(colour = "black", size = 16),
          axis.text.y = element_text(colour = "black", size = 16),
          legend.position = "none") +
    scale_x_continuous(breaks = seq(0.2, 0.8, 0.2),
                       limits = c(0.18, 0.8)) +
    scale_y_continuous(breaks = seq(-0.2, 0.8, 0.2),
                       limits = c(-0.2, 0.8)) +
    scale_colour_manual(values = c("darkgreen", "darkred", "#E69F22")) +
    labs(x = "Native R2",
         y = "Correlation of status") +
    geom_text_repel(aes(label = prefix), size = 4, 
                    vjust = 0.5,
                    hjust = -0.2,
                    point.padding = 0.25,
                    force = T) + # for more info: https://stackoverflow.com/questions/15624656/label-points-in-geom-point
    theme(title = element_text(size = 28))
  
  
  ggsave("Results/correlation plots/scaled/grouped/r2 correlation coloured.jpeg", plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
# weighted by records -------------------------------------------------
  ggplot(aes(x = r2, y = correlation, 
             col = col_col, size = total_rec), data = dat) +
    geom_point(shape = "circle") +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 2),
          axis.text.x = element_text(colour = "black", size = 16),
          axis.text.y = element_text(colour = "black", size = 16),
          legend.position = "none") +
    scale_x_continuous(breaks = seq(0.2, 0.8, 0.2),
                       limits = c(0.18, 0.8)) +
    scale_y_continuous(breaks = seq(-0.2, 0.8, 0.2),
                       limits = c(-0.2, 0.8)) +
    scale_colour_manual(values = c("darkgreen", "darkred", "#E69F22")) +
    labs(x = "Native R2",
         y = "Correlation of status") +
    geom_text_repel(aes(label = prefix), size = 4, 
                    vjust = 0.5,
                    hjust = -0.2,
                    point.padding = 0.25,
                    force = T) + # for more info: https://stackoverflow.com/questions/15624656/label-points-in-geom-point
    theme(title = element_text(size = 28))
  
  
  
  ggsave("Results/correlation plots/scaled/grouped/r2 correlation weighted by records.jpeg", plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  
# weighted by first occurrence -------------------------------------------
  ggplot(aes(x = r2, y = correlation, 
             col = col_col, size = scaled_fo), data = dat) +
    geom_point(shape = "circle") +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 2),
          axis.text.x = element_text(colour = "black", size = 16),
          axis.text.y = element_text(colour = "black", size = 16),
          legend.position = "none") +
    scale_x_continuous(breaks = seq(0.2, 0.8, 0.2),
                       limits = c(0.18, 0.8)) +
    scale_y_continuous(breaks = seq(-0.2, 0.8, 0.2),
                       limits = c(-0.2, 0.8)) +
    scale_colour_manual(values = c("darkgreen", "darkred", "#E69F22")) +
    labs(x = "Native R2",
         y = "Correlation of status") +
    geom_text_repel(aes(label = prefix), size = 4, 
                    vjust = 0.5,
                    hjust = -0.2,
                    point.padding = 0.25,
                    force = T) + # for more info: https://stackoverflow.com/questions/15624656/label-points-in-geom-point
    theme(title = element_text(size = 28))
  
  
  
  ggsave("Results/correlation plots/scaled/grouped/r2 correlation weighted by first occurrence.jpeg", plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
# ----------------------------------------------------------
  