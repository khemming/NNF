

# library --------------------------------------------------------------
  library(raster)
  library(ggthemes)
  library(ggThemeAssist)
  library(tidyverse)
  library(magrittr)
  
  rm(list = ls())

# data -----------------------------------------------------------------
# nonnative data
  load("Data files/Rdata/quadratic_model_data.RData")
# native data  
  nat_ci <- read.csv("Results/Fabaceae sans Acacia/csv/model estimates.csv")
  nnat_ci <- ci_list[["Fabaceae_nonnative"]]
  
# preparing model data for plotting -----------------------------------------
# attain coefficients, confidence intervals, by family by status
  status <- rep(c("Native", "Nonnative"), each = 14)
# environmental variable labels
  plot_pvs <-  c("Human\nactivity",
                 "H.A.^2",
                 "Topographic\nheterogeneity",
                 "T.H.^2",
                 "Winter\nrainfall", 
                 "W.R.^2",  
                 "Summer\nrainfall",
                 "S.R.^2",
                 "Temperature\nseasonality",
                 "T.S.^2",
                 "Aridity",
                 "A.^2",
                 "Annual mean\ntemperature",
                 "A.M.T.^2")
  
# model to data frame function
    nat <- data.frame(pvs = rownames(nat_ci),
                      upper = nat_ci$upper,
                      estimate = nat_ci$est.,
                      lower = nat_ci$lower,
                      status = "Native") %>%
      slice(2:15) # removes intercept and proportion cover
    nnat <- data.frame(pvs = rownames(nat_ci),
                       upper = nnat_ci$upper,
                       estimate = nnat_ci$est.,
                       lower = nnat_ci$lower,
                       status = "Nonnative") %>%
      slice(2:15)
    
    long_df <- bind_rows(nat, nnat) %>%
      mutate(status = as.factor(status),
             plot_pvs = factor(rep(plot_pvs, 2), levels = plot_pvs))
    
# plot function ----------------------------------------------------------------------
  coef_status_plot <- function(dat, family, x_mn, x_mx) {
    
    q <- ggplot(dat, aes(y = plot_pvs, shape = status, colour = status)) +
      theme_classic() +
      scale_colour_manual(labels = c("Native", "Nonnative"),
                          values = c("blue", "red")) + 
      geom_vline(aes(xintercept = 0),
                 colour = "black", 
                 size = 0.9, 
                 linetype = "dashed") +
      labs(colour = "Status",
           shape = "Status",
           x = "Mean estimate",
           y = "") +
      geom_point(aes(x = estimate), size = 4, position = position_dodge(width = 0.55)) +
      geom_errorbarh(aes(xmin = lower, xmax = upper),
                     size = 1, position = position_dodge(width = 0.45), height = 0) +
      scale_x_continuous(limits = c(x_mn, x_mx),
                         breaks = c(round(x_mn, 2), 0, round(x_mx, 2)),
                         labels = c(round(x_mn, 2), 0, round(x_mx, 2))) +
      theme(legend.title = element_text(size = 18),
            legend.text = element_text(size = 16),
            legend.position = "bottom", 
            axis.title = element_text(size = 18, face = "bold"),
            plot.title = element_text(size = 22, face = "bold"),
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 18),
            axis.text.y = element_text(colour = "black", size = 14),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(colour = "black", size = 1),
            plot.caption = element_text(size = 14))
    
  # save into family-specific folder  
    save <- paste0("Results/major figures by family/", family, "/coefficient plot.jpeg")
    ggsave(save, plot = last_plot(), height = 12, width = 15, units = "cm", dpi = 500, device = "jpeg")
    
    plot(q) 
  }
  
# run function via loop --------------------------------------
# required: 
# dat     = model plotting dataframe (held in a list = output)
# family  = family name (without status) / names22
# lower/upper_ci = plot margins 
  coef_status_plot(long_df, "Fabaceae sans Acacia", 
                   x_mn <- floor(min(long_df$lower) * 10)/10,
                   x_mx <- ceiling(max(long_df$upper) * 10)/10)

# -------------------------------------------------------------
  
