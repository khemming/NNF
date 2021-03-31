

# library --------------------------------------------------------------
  library(raster)
  library(ggthemes)
  library(ggThemeAssist)
  library(tidyverse)
  library(magrittr)
  
  rm(list = ls())

# data -----------------------------------------------------------------
# model results
  load("Data files/Rdata/quadratic_model_data.RData")

# 22 families
  names22 <- read.csv("Results/csv/family summary statistics/study families.csv", stringsAsFactors = F)[1:22, 1]
  glimpse(names22)
  
# names by status (36)
  setwd("C:/Users/Hemming/Dropbox/NNF/Results/rasters/iNEXT")
  current.list <- list.files(pattern = ".grd")
  names44 <- gsub(pattern = ".grd$", "", current.list)
  setwd("C:/Users/Hemming/Dropbox/NNF")
  
# r2 
  r2 <- read.csv("Results/csv/NNFC correlates/quadratic r2.csv") %>%
        dplyr::select(family, qua_nat, qua_nn_nn, qua_nn_nat)
  
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
  model_df <- function(nat_ci, nnat_ci){
    nat <- data.frame(pvs = rownames(nat_ci),
                      lower = ifelse(nat_ci$lower < -0.60, -0.60, nat_ci$lower),
                      estimate = nat_ci$est.,
                      upper = ifelse(nat_ci$upper > 0.60, 0.60, nat_ci$upper),
                      status = "Native") %>%
           slice(2:15) # removes intercept and proportion cover
    nnat <- data.frame(pvs = rownames(nat_ci),
                       lower = ifelse(nnat_ci$lower < -0.60, -0.60, nnat_ci$lower),
                       estimate = nnat_ci$est.,
                       upper = ifelse(nnat_ci$upper > 0.60, 0.60, nnat_ci$upper),
                       status = "Nonnative") %>%
            slice(2:15)
    
    long_df <- bind_rows(nat, nnat) %>%
               mutate(status = as.factor(status),
                      plot_pvs = factor(rep(plot_pvs, 2), levels = plot_pvs))
    
    return(long_df)
  }
  
# run
# set output
  output <- list()
  
  for(i in 1:length(names22)){
    
    nat_seq <- seq(1, length(names44), 2)  # grabs every second model which, starting 1, are all natives
    nnat_seq <- seq(2, length(names44), 2) # vise versa, starting from 2
    
    output[[i]] <- model_df(ci_list[[nat_seq[i]]], ci_list[[nnat_seq[i]]])
    
    }
    
# check: reference estimate of HII
  ci_list[[1]]  
  output[[1]]   
  
# plot function ----------------------------------------------------------------------
  coef_status_plot <- function(dat, family, n_r2, nn_r2) {
    
    r2_lab = paste0("Native R2 = ", sprintf("%.3f", n_r2, 3), "\n", "Nonnative R2 = ", sprintf("%.3f", nn_r2, 3))
    
  # plot
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
      scale_x_continuous(limits = c(-0.7, 0.7),
                         breaks = seq(-0.6, 0.6, 0.3),
                         labels = seq(-0.6, 0.6, 0.3)) +
      # annotate("text", size = 4, x = -0.45, y = 1.1, label = r2_lab) +
      theme(legend.title = element_text(size = 18),
            legend.text = element_text(size = 16),
            legend.position = "bottom", 
            axis.title = element_text(size = 18, face = "bold"),
            plot.title = element_text(size = 22, face = "bold"),
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 18),
            axis.text.y = element_text(colour = "black", size = 12),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(colour = "black", size = 1),
            plot.caption = element_text(size = 14))
    
  # general save  
    save <- paste0("Results/coefficient plots/quadratic/", family, ".jpeg")
    ggsave(save, plot = last_plot(), height = 17, width = 20, units = "cm", dpi = 500, device = "jpeg")
  # save into family-specific folder  
    save <- paste0("Results/major figures by family/", family, "/quadratic coefficients.jpeg")
    ggsave(save, plot = last_plot(), height = 12, width = 15, units = "cm", dpi = 500, device = "jpeg")
  
    plot(q) 
  }
  
# run function via loop --------------------------------------
# required: 
# dat     = model plotting dataframe (held in a list -- output)
# family  = family name (without status) / names22
# nat_r2  = r2 of native
# nnat_r2 = r2 of nonnative predicted with nonnative
  
  for (i in 1:length(names22)) {
    
    coef_status_plot(output[[i]],
                     names22[i],
                     r2$qua_nat[i],
                     r2$qua_nn_nn[i])
  
  }

# note: some missing error bars with these plot parameters    

# -------------------------------------------------------------
  
