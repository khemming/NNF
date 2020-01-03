#######################################################################
# native-environ R2 vs. native:non-native corr
#######################################################################

# scope --------------------------------------------------------------
# determine if there is a relationhip between (a) how well environmental variables explain native species richness estimates and (b) how correlated native:non-native species richnesses are

# library -------------------------------------------------------------
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------
  load("Data files/Rdata/model_coefficients.RData")


# correlate native and non-native species richnesses ------------------
# a) generate correlations between native and non-native specides richness
# b) extract r2 from relevant native model
  
# Asteraceae - m1
  hist(Asteraceae.native)
  hist(Asteraceae.non.native)
  ast.cor <- cor(spp$Asteraceae.native, 
                   spp$Asteraceae.non.native, 
                   use = "complete.obs", 
                   method = "spearman")
  ast.r2 <- summary(m1)$adj.r.squared
  
# Brassicaceae - m3
  hist(Brassicaceae.native)
  hist(Brassicaceae.non.native)
  bra.cor <- cor(spp$Brassicaceae.native, 
                 spp$Brassicaceae.non.native, 
                 use = "complete.obs", 
                 method = "spearman")
  bra.r2 <- summary(m3)$adj.r.squared
  
  
# Cyperaceae - m5
  hist(Cyperaceae.native)
  hist(Cyperaceae.non.native)
  cyp.cor <- cor(spp$Cyperaceae.native, 
                 spp$Cyperaceae.non.native, 
                 use = "complete.obs", 
                 method = "spearman")
  cyp.r2 <- summary(m5)$adj.r.squared
  
# Fabaceae - m7
  hist(Fabaceae.native)
  hist(Fabaceae.non.native)
  fab.cor <- cor(spp$Fabaceae.native, 
                 spp$Fabaceae.non.native, 
                 use = "complete.obs", 
                 method = "spearman")
  fab.r2 <- summary(m7)$adj.r.squared
  
# Malvaceae - m9
  hist(Malvaceae.native)
  hist(Malvaceae.non.native)
  mal.cor <- cor(spp$Malvaceae.native, 
                 spp$Malvaceae.non.native, 
                 use = "complete.obs", 
                 method = "spearman")
  mal.r2 <- summary(m9)$adj.r.squared
  
# Myrtaceae
  hist(Myrtaceae.native)
  hist(Myrtaceae.non.native)
  myr.cor <- cor(spp$Myrtaceae.native, 
                 spp$Myrtaceae.non.native, 
                 use = "complete.obs", 
                 method = "spearman")
  myr.r2 <- summary(m11)$adj.r.squared
  
# Plantae
  hist(Plantae.native)
  hist(Plantae.non.native)
  pla.cor <- cor(spp$Plantae.native, 
                 spp$Plantae.non.native, 
                 use = "complete.obs", 
                 method = "spearman")
  pla.r2 <- summary(m13)$adj.r.squared
  
# Proteaceae - m15
  # hist(Proteaceae.native)
  # hist(Proteaceae.non.native)
  # bra.cor <- cor(spp$Proteaceae.native, 
  #                spp$Proteaceae.non.native, 
  #                use = "complete.obs", 
  #                method = "spearman")
  # bra.r2 <- summary(m15)$adj.r.squared
  
# Solanaceae - m16
  hist(Solanaceae.native)
  hist(Solanaceae.non.native)
  sol.cor <- cor(spp$Solanaceae.native, 
                 spp$Solanaceae.non.native, 
                 use = "complete.obs", 
                 method = "spearman")
  sol.r2 <- summary(m16)$adj.r.squared
  
# data frame
  cors <- c(ast.cor, bra.cor, cyp.cor, fab.cor, mal.cor, myr.cor, pla.cor, sol.cor)
  r2 <-   c(ast.r2,  bra.r2,  cyp.r2,  fab.r2,  mal.r2,  myr.r2,  pla.r2,  sol.r2)
  
  cors.r2 <- data.frame(cbind(r2, cors))
 
# preview plot  
  plot(cors.r2$r2, cors.r2$cors)
  r2.cor.cor <- round(cor(cors.r2$r2, cors.r2$cors, use = "complete.obs", method = "pearson"), 2)
  r2.cor.cor
  
# ggplot plot ------------------------------------------------
# question: how to label data points?? 
# i.e. each dot with it's associated family 3-letter tag?
  
  cor.label <- paste0("r = ", r2.cor.cor)
  save <- "Results/correlation plots/native EV r2 vs. native-non-native/correlation plot.jpeg"
  
  ggplot(aes(x = r2, y = cors), data = cors.r2) +
    geom_point(shape = "circle", size = 2) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 12),
          axis.text.y = element_text(colour = "black", size = 12),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = c(0, 0.3, 0.6, 0.9),
                       limits = c(0, 0.8)) +
    scale_y_continuous(breaks = c(-0.3, 0, 0.3, 0.6, 0.9),
                       limits = c(-0.3, 0.9)) +
    #geom_abline(intercept = 0, slope = 1, size = 1) +
     annotate("text", x = 0.75, y = -0.2, 
              label = cor.label, size = 4) +
    labs(x = "native richness' environmental variable R2",
         y = "native-non-native correlation") +
    theme(title = element_text(size = 14))
  
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  
  
  
# ----------------------------------------------------------