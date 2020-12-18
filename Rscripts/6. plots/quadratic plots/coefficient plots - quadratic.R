
# the aim here is to remove non-significant quadratic terms  

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
  names22 <- read.csv("Results/csv/families and predictors/study families.csv", stringsAsFactors = F)[1:22, 1]
  glimpse(names22)
  
# names by status (44)
  setwd("C:/Users/s436862/Dropbox/NNF/Results/rasters/iNEXT")
  current.list <- list.files(pattern = ".grd")
  names44 <- gsub(pattern = ".grd$", "", current.list)
  setwd("C:/Users/s436862/Dropbox/NNF")
  
# preparing model data for plotting -----------------------------------------
# attain coefficients, confidence intervals, by family by status
  status <- rep(c("Native", "Nonnative"), each = 14)
# environmental variable labels
  plot_pvs <-  list("HA^2",   "Human\nimpact",
                    "TH^2",   "Topographic\nheterogeneity",
                    "WR^2",   "Winter\nrainfall", 
                    "SR^2",   "Summer\nrainfall",
                    "TS^2",   "Temperature\nseasonality",
                    "ARD^2", "Aridity",
                    "AMT^2", "Annual mean\ntemperature")
  
# model to data frame function
  model_df <- function(nat_ci, nnat_ci){
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
    
  # remove rows that are quadratic that do not exclude zero
    long_df <- bind_rows(nat, nnat) %>%
      mutate(status = as.factor(status),
             type = rep(c("q", "l"), 14),
             excl = ifelse((lower < 0 & upper > 0), "yes", "no"),
             plot_pvs = factor(rep(plot_pvs, 2), levels = plot_pvs)) %>%
             droplevels() 
    
    ind_em <- long_df$pvs %in% long_df[(long_df$type == "q" & long_df$excl == "no"),]$pvs
    if (sum(ind_em)>0) {
      long_df[ind_em,]$excl <- "no"  
    }
    
    
     long_df2 <- long_df %>% filter(!(type == "q" & excl == "yes"))
    
    return(long_df2)
  }
  
# run
# set output and access to native/nonnative family pairs
  output <- list()
  nat_seq <- seq(1, length(names44), 2)   
  nnat_seq <- seq(2, length(names44), 2) 
  
  for(i in 1:length(names22)){
    output[[i]] <- model_df(ci_list[[nat_seq[i]]], ci_list[[nnat_seq[i]]])
    }
  
# check
  output[[7]]   
  
# plot function ----------------------------------------------------------------------
  coef_status_plot <- function(dat, family, x_mn, x_mx, brks) {
    
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
      geom_point(aes(x = estimate), size = 3, position = position_dodge(width = 0.5)) +
      geom_errorbarh(aes(xmin = lower, xmax = upper),
                     size = 1, position = position_dodge(width = 0.5), height = 0) +
      scale_x_continuous(limits = c(x_mn, x_mx),
                         breaks = brks,
                         labels = brks) +
      theme(legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.position = "bottom", 
            axis.title = element_text(size = 16),
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 16),
            axis.text.y = element_text(colour = "black", size = 14),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(colour = "black", size = 1),
            plot.caption = element_text(size = 14))
    
  # general save  
    save <- paste0("Results/coefficient plots/quadratic/", family, ".jpeg")
    ggsave(save, plot = last_plot(), height = 13, width = 15, units = "cm", dpi = 500, device = "jpeg")
  # save into family-specific folder  
    save <- paste0("Results/major figures by family/", family, "/quadratic coefficients.jpeg")
    ggsave(save, plot = last_plot(), height = 13, width = 15, units = "cm", dpi = 500, device = "jpeg")
  
    plot(q) 
  }
  
  # run function via loop --------------------------------------
  # required: 
  # dat     = model plotting dataframe (held in a list = output)
  # family  = family name (without status) / names22
  # x_mn/x_mx = x axis margins; rounded max and min of CIs 
  # breaks = formatted distance between mean estiamtes
  # notes  ----------------------------------------------
  # breaks made using the following code to determine optimum distances:
  #breaks = c(x_mn, round(x_mn/2, 2), 0, round(x_mx/2, 2), x_mx),
  #labels = c(x_mn, round(x_mn/2, 2), 0, round(x_mx/2, 2), x_mx)
  # breaks
  brks <- list(c(-0.5, 0, 0.5, 1),
               c(-0.6, -0.2, 0, 0.4, 0.8),
               c(-0.2, 0, 0.2, 0.4),
               c(-0.75, 0, 0.75, 1.5),
               c(-0.3, -0.15, 0, 0.15),    
               
               c(-0.6, -0.3, 0, 0.3),  
               c(-0.3, -0.15, 0, 0.15, 0.3),
               c(-0.8, -0.4, 0, 0.4, 0.8),
               c(-0.4, 0, 0.4, 0.8, 1.2),
               c(-0.3, 0, 0.3, 0.6),
               
               c(-10, -5, 0, 5, 10),
               c(-0.6, -0.3, 0, 0.3),
               c(-0.2, -0.1, 0, 0.1, 0.2),
               c(-0.6, -0.3, 0, 0.3, 0.6),
               c(-0.3, -0.15, 0, 0.15, 0.3),
               
               c(-0.6, -0.3, 0, 0.3),
               c(-0.4, -0.2, 0, 0.2, 0.4),
               c(-0.3, -0.15, 0, 0.15),
               c(-0.15, 0, 0.15, 0.3),
               c(-0.4, -0.2, 0, 0.2, 0.4),
               
               c(-0.6, -0.3, 0, 0.3, 0.6),
               c(-0.15, 0, 0.15, 0.3))
# ---------------------------------------------------------------------  
# check
# Asparagaceae
  i <- 1
  coef_status_plot(output[[i]],
                   names22[i],
                   x_mn <- floor(min(output[[i]]$lower) * 10)/10,
                   x_mx <- ceiling(max(output[[i]]$upper) * 10)/10,
                   brks[[i]])
  
  for (i in 1:length(names22)) {
    
    coef_status_plot(output[[i]],
                     names22[i],
                     x_mn <- floor(min(output[[i]]$lower) * 10)/10,
                     x_mx <- ceiling(max(output[[i]]$upper) * 10)/10,
                     brks[[i]])
    
  }


  
# -------------------------------------------------------------------

  