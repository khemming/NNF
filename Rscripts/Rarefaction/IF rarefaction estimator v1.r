
# Date created: 31/7/18
# Last updated: 5/2/19
# based on: v5 of Rarefaction chapter estimator script

# rarefaction  -------------------------------------------------------------------------
# for the top-five invasive families (bar Poaceae), calaculated from 10-50 records, for both native/exotic richness

# library -------------------------------------------------------------------------
  library(raster)
  library(dplyr)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")
  
# data ----------------------------------------------------------------------------
# raster  
  raster <- raster("Australia/aus 100-km.grd")  
  
# species  
  dat <- read.csv("AVH/2019 master data/AVH master data.csv")
  colnames(dat) <- c("scientific.name", 
                     "matched.scientific.name", 
                     "order",
                     "family", 
                     "genus", 
                     "species", 
                     "latitude",
                     "longitude", 
                     "IBRA.region", 
                     "state", 
                     "year",
                     "taxon.identification.issue", 
                     "inferred.duplicate.Record", 
                     "name.not.in.national.checklists", 
                     "status")
  
# five invasive families
  fam <- read.csv("C:/Users/s436862/Dropbox/Invasive families/Results/CSV/Records & species richness of plant families of Australia.csv", header = T)
  fam.names <- as.character(fam[2:6,1])
  
  ex.fam <- dplyr::select(dat, species, family, latitude, longitude, year, status) %>%
                  filter(family %in% fam.names) %>%
                  droplevels()
  table(ex.fam$family)
  
# rarefaction set-up -------------------------------------------------------  
# subset dataframe for family x status
# note: this produces 10x data frames to run -- maybe I can automate this better, not sure yet

# Asteraceae  
  ast.n <- filter(ex.fam, family == "Asteraceae" & status == 1) %>%
    dplyr::select(species, latitude, longitude) 
  
  ast.i <- filter(ex.fam, family == "Asteraceae" & status == 0) %>%
    dplyr::select(species, latitude, longitude)  
  
# Brassicaceae    
  bra.n <- filter(ex.fam, family == "Brassicaceae" & status == 1) %>%
    dplyr::select(species, latitude, longitude) 
  
  bra.i <- filter(ex.fam, family == "Brassicaceae" & status == 0) %>%
    dplyr::select(species, latitude, longitude)  

# Fabaceae    
  fab.n <- filter(ex.fam, family == "Fabaceae" & status == 1) %>%
    dplyr::select(species, latitude, longitude) 
  
  fab.i <- filter(ex.fam, family == "Fabaceae" & status == 0) %>%
    dplyr::select(species, latitude, longitude)  

# Myrtaceae   
  myr.n <- filter(ex.fam, family == "Myrtaceae" & status == 1) %>%
    dplyr::select(species, latitude, longitude) 
  
  myr.i <- filter(ex.fam, family == "Myrtaceae" & status == 0) %>%
    dplyr::select(species, latitude, longitude)  

# Solanaceae  
  sol.n <- filter(ex.fam, family == "Solanaceae" & status == 1) %>%
    dplyr::select(species, latitude, longitude) 
  
  sol.i <- filter(ex.fam, family == "Solanaceae" & status == 0) %>%
    dplyr::select(species, latitude, longitude)  
  
# rarefaction run ------------------------------------------------------------  
  # one at a time method
  # spp <- ast.n
  # spp <- ast.i
  # 
  # spp <- bra.n
  # spp <- bra.i
  # 
  # spp <- fab.n
  # spp <- fab.i
  # 
  # spp <- myr.n
  # spp <- myr.i
  # 
  # spp <- sol.n
  # spp <- sol.i
  
# number of records by grid square -------------------------------------------
  xy <- cbind(spp$longitude, spp$latitude)
  raster.rec <- rasterize(xy, raster, fun = function(x,...) length(x))
  df.rec <- getValues(raster.rec)
  
# assign each point in the dataframe to raster cell
  spp$cell <- raster::extract(raster, xy)
  
# list of cell numbers
  cell.list <- as.numeric(levels(factor(spp$cell)))
  
# number of records per cell -------------------------------------------------
# number of records for only cells with records in them
  n.rec <- table(spp$cell)
  nr <- data.frame(cell = as.numeric(names(n.rec)), 
                   n.rec = as.vector(n.rec))
  
  n.cell.rec.occ <- spp %>%
    group_by(cell) %>%
    summarise(n.rec = n())      

# number of records for all cells  
  n.cell.rec.tot <- data.frame(cell = 1:length(n.cell.rec.occ), n.tot = n.cell.rec.occ)
  
  
# check the dataframes line up
  ch.rv1 <- full_join(n.cell.rec.occ, n.cell.rec.tot)      
  plot(log(raster.rec) ~ log(n.rec), data = ch.rv1) # they agree
  
# Number of species per cell ---------------------------------------------
# number of species per cell
  spp.per.cell <- as.numeric(factor(spp$species))
  n.spp <- rasterize(xy, raster, field = spp.per.cell, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(n.spp)
  
# check this (spp richness per cell)
  check2 <- spp %>%
    group.by(cell) %>%
    summarise(n = length(unique(species))) 
  
  x <- getValues(n.spp)
  rv <- data.frame(cell = 1:length(x), n.tot = x)
  ch.rv <- full_join(check2, rv) # line up all goods
  plot(log(n) ~ log(n.tot), data = ch.rv) # lines up, cool
  
# add number of records, per cell, to the dataframe
  spp <- full_join(spp, nr) %>%
    arrange(cell)
  
# cell numbers with total records in each
  cell.list <- as.numeric(levels(factor(spp$cell)))
  
# function ----------------------------------------------
  rarefaction.solo <- function(sp, cutoff) 
  { 
    n <- length(sp)           # number of records           
    n.sp <- table(sp)         # number of records for each species    
    out <- numeric(length(n.sp))# vector to store estimate for each species
    
    # for each species, calculate the expected number of occurrences from n records
    for(i in 1:length(n.sp)) 
    {
      out[i] <- 1 - exp(lchoose((n - n.sp[i]), cutoff) - lchoose(n, cutoff))    
      # use lchoose (i.e. log scale) to avoid problems with big numbers
    }
    
    return(round(sum(out)))
  } # end function 
  
# Run function ----------------------------------------------------------
# Including 10, but really testing 15 - 50 cut off values (n.min)
  n.min <- seq(10, 50, 5)
  
# matrix to store rarefied cells' output, for a single cut off 
  out.rare <- matrix(NA, nrow = length(cell.list), ncol = 1) # length 1003
  
# matrix with all cell info (including NAs), for a single cut off
  a <- matrix(NA, nrow = length(raster), ncol = 9)
  
# function start
  for (i in 1:length(n.min)) {
    
    rec.no <- n.min[i]
    
    for(j in 1:length(cell.list)) { 
      cell <- dplyr::filter(spp, cell == cell.list[j]) # return only the cells with records in them
      sp <- as.character(cell$species)
      if(cell$n.rec[1] < n.min) out.rare[j] <- NA 
      else {out.rare[j, ] <- rarefaction.solo(sp, rec.no) 
      }
    }
    
    m <- matrix(NA, nrow = length(getValues(raster)), ncol = 1)
    m[cell.list, ] <- out.rare                         
    
    a[,i] <- m
    
  } # multi-cutoff end 
  
  rare.rich.cutoff <- a
  
# view -----------------------------------------------------------------------
  est_rich_10 <- setValues(raster, rare.rich.cutoff[, 1])
  plot(est_rich_10)
  est_rich_15 <- setValues(raster, rare.rich.cutoff[, 2])
  plot(est_rich_15)
  est_rich_20 <- setValues(raster, rare.rich.cutoff[, 3])
  plot(est_rich_20)
  est_rich_25 <- setValues(raster, rare.rich.cutoff[, 4])
  plot(est_rich_25)
  est_rich_30 <- setValues(raster, rare.rich.cutoff[, 5])
  plot(est_rich_30)
  est_rich_35 <- setValues(raster, rare.rich.cutoff[, 6])
  plot(est_rich_35)
  est_rich_40 <- setValues(raster, rare.rich.cutoff[, 7])
  plot(est_rich_40)
  est_rich_45 <- setValues(raster, rare.rich.cutoff[, 8])
  plot(est_rich_45)
  est_rich_50 <- setValues(raster, rare.rich.cutoff[, 9])
  plot(est_rich_50)
  
  
# data frame
  rarefied_list <- data.frame(rare.rich.cutoff)
  colnames(rarefied_list) <- c("rare_10", "rare_15", "rare_20", "rare_25", "rare_30", "rare_35", "rare_40", "rare_45", "rare_50")
  
  
# removing off-shore cells ------------------------------------------------
# Land/ocean & cell values
  cell.cat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/CSV/Terrestrial land categories.csv", header = T)
  
# Subset terrestrial cells out of data frames  
  poa.land <- cbind(cell.cat, rarefied_list)
  poa.terr <- filter(poa.land, cell.cat == "terrestrial")
  
# generate list of occupied cells
  cell.list.x <- poa.terr$cell.id
  
# make a matrix with all missing values (to get from 1003 cells to 2538)
  x <- matrix(NA, nrow = length(getValues(raster)), ncol = 9)
  
  poa.m <- as.matrix(poa.terr)
  poa.mm <- poa.m[, 3:11]
  class(poa.mm) <- "numeric"
  
# add the occupied cells
  cell.list.x <- poa.terr$cell.id
  x[cell.list.x, ] <- poa.mm
  
  colnames(x) <- c("rare_10", "rare_15", "rare_20", "rare_25", "rare_30", "rare_35", "rare_40", "rare_45", "rare_50")
# save  
  write.csv(x, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Poa 10 to 50 rarefied richness cut offs.csv", row.names = F)
  
  
# new script after this :) --------------------------------------------------------------  
  
# correlating 10-50 and percentage coverage -------------------------------------------------------- 
# Standard deviation of cell records vs. number of cells occupied (Not sure if need)
# We want a high SD, but high record number - let's see how this plays out
  cutoff <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Poa 10 to 50 rarefied richness cut offs.csv", header = T)
  
  # SD & CV of each cutoff value  
  cutoff.sd <- sapply(cutoff, sd, na.rm = T)
  
  cv <- function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)}
  cutoff.cv <- sapply(cutoff, cv)
  
  # just going to check this is ok
  cv.test.35 <- sd(cutoff$rare_35, na.rm = T) / mean(cutoff$rare_35, na.rm = T)
  cv.test.20 <- sd(cutoff$rare_20, na.rm = T) / mean(cutoff$rare_20, na.rm = T)
  # Es ok :) 
  
  # Number of cells with rarefied records
  # Not sure how to do one for each row altogether so I shall do manually
  r10 <- sum(table(cutoff$rare_10))
  r15 <- sum(table(cutoff$rare_15))
  r20 <- sum(table(cutoff$rare_20))
  r25 <- sum(table(cutoff$rare_25))
  r30 <- sum(table(cutoff$rare_30))
  r35 <- sum(table(cutoff$rare_35))
  r40 <- sum(table(cutoff$rare_40))
  r45 <- sum(table(cutoff$rare_45))
  r50 <- sum(table(cutoff$rare_50))
  cutoff.cell.tot <- rbind(r10, r15, r20, r25, r30, r35, r40, r45, r50)
  
  # percentage  
  p10 <- sum(table(cutoff$rare_10))/1003*100
  p15 <- sum(table(cutoff$rare_15))/1003*100
  p20 <- sum(table(cutoff$rare_20))/1003*100
  p25 <- sum(table(cutoff$rare_25))/1003*100
  p30 <- sum(table(cutoff$rare_30))/1003*100
  p35 <- sum(table(cutoff$rare_35))/1003*100
  p40 <- sum(table(cutoff$rare_40))/1003*100
  p45 <- sum(table(cutoff$rare_45))/1003*100
  p50 <- sum(table(cutoff$rare_50))/1003*100
  cutoff.per <- rbind(p10, p15, p20, p25, p30, p35, p40, p45, p50)
  
  # correlation matrix
  # 50 cut off 
  co.50 <- select(cutoff, rare_50)
  
  # other cutoffs (co)  
  co.others <- select(cutoff, rare_10, rare_15, rare_20, rare_25, rare_30,
                      rare_35, rare_40, rare_45, rare_50)  
  
  # correlation matrix
  total.cor <- cor(co.others, co.50, use = "complete.obs")  
  
  # rarefied names
  rarefied <- c(10, 15, 20, 25, 30, 35, 40, 45, 50)
  
  # dataframe
  cutoff.com <- cbind(rarefied, cutoff.cell.tot, cutoff.per, cutoff.sd, cutoff.cv, total.cor)
  colnames(cutoff.com) <- c("rarefied", "cell.total", "cell.percent", "SD", "CV", "correlation.w.50")  
  
  # save
  write.csv(cutoff.com, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Poa 10 to 50 rarefied richness cut off summary stats.csv", row.names = F)

   
  # ---------------------------------------------------------------------------------  

  

  
# 3.0 Sampling effort calculations -------------------------------------------------
# Species richness, Chao1 estimator, and rarefaction correlated against sampling effort.
# Ideally, this should show the efficacy of rarefaction at standardising species richness for sampling effort. 
# Data -------------------------------------------------------------------------
  spp <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
    filter(pp == "C3" | pp == "C4") # removing 'unknown' and 'mixed' pp classes
  
  
# 3.1 Calculate total record number ---------------------------------------------  
# Template of Australia
  raster <- raster("Australia/aus_100km_cell_id.grd")
  
# number of records by grid square
  xy <- cbind(spp$long, spp$lat)
  spp$cell <- raster::extract(raster, xy)
  
# Total records for every cell
  rec.tot <- rasterize(xy, raster, fun = function(x,...) length(x))
  plot(rec.tot)
  
# 3.2 Calculate species richness ---------------------------------------------------
# number of species per cell
  spp_per_cell <- as.numeric(factor(spp$species))
  spp.rich <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(spp.rich)
  
  # 3.3 Chao1 species richness estimation ---------------------------------------------
  # For Chao1 estimation: singletons, doubletons 
  f1 <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(which(table(x)==1)) })
  f2 <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(which(table(x)==2)) })
  
  # Convert to dataframe for calcuations
  n <- getValues(rec.tot) # total number of records per cell
  a <- getValues(spp.rich) # species richness per cell
  f1 <- getValues(f1) # species with only one record in each cell
  f2 <- getValues(f2) # speices with only two "                 "
  
  # Replace NAs with zeroes for SRE calculations
  raster_df <- getValues(raster)
  sre <- data.frame(raster_df, n, a, f1, f2)
  sre[is.na(sre)] <- 0
  sum(is.na(sre)) # cool
  
  # split them back up
  n <- sre$n
  a <- sre$a
  f1 <- sre$f1
  f2 <- sre$f2
  
  # Chao1 
  chao1.df <- ifelse(f2 == 0, 
                     (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1)))),
                     (a + (((n-1) / n) * ((f1^2)/(2*f2))))
  ) # 'if' won't handle two conditional statements  
  
  chao1 <- setValues(raster, chao1.df)
  plot(chao1)
  
  
  # Can easily do ACE estimation if need be, but I think this will suffice  
  
  
  
# 3.4 Rarefaction to 15 records not splitting for photosynthetic pathway -----------------------------------------------  
# number of records by grid square
  xy <- cbind(spp$long, spp$lat)
  n_tot <- rasterize(xy, raster, fun = function(x,...) length(x))
  n_tot_val <- getValues(n_tot)
  
# assign each point in the dataframe to raster cell
  spp$cell <- raster::extract(raster, xy)
  
# get a list of the cell numbers
  cell.list <- as.numeric(levels(factor(spp$cell)))
  
# number of records per cell --------------------------------------------
# number of records for only cells with records in them
  n_rec <- table(spp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), 
                   n_rec = as.vector(n_rec))
  n_cell_rec_occ <- spp %>%
    group_by(cell) %>%
    summarise(n_rec = n())      
  
# number of records for all cells  
  n_cell_rec_tot <- data.frame(cell = 1:length(n_tot_val), n_tot = n_tot_val)
  
# check the dataframes line up
  ch_rv1 <- full_join(n_cell_rec_occ, n_cell_rec_tot)      
  plot(log(n_tot) ~ log(n_rec), data = ch_rv1) # they agree
  
# Number of species per cell ---------------------------------------------
# number of species per cell
  spp_per_cell <- as.numeric(factor(spp$species))
  n_spp <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(n_spp)
  
# check this (spp richness per cell)
  check2 <- spp %>%
    group_by(cell) %>%
    summarise(n = length(unique(species))) 
  
  x <- getValues(n_spp)
  rv <- data.frame(cell = 1:length(x), n_tot = x)
  ch_rv <- full_join(check2, rv) # line up all goods
  plot(log(n) ~ log(n_tot), data = ch_rv) # lines up, cool
  
# add number of records, per cell, to the dataframe
  spp <- full_join(spp, nr) %>%
    arrange(cell)
  
# cell numbers with total records in each
  cell_list <- as.numeric(levels(factor(spp$cell)))
  
# Rarefaction (no PP split) function -------------------------------------
  rarefaction_solo <- function(sp, cutoff) 
  { 
    n <- length(sp)       # number of records           
    n_sp <- table(sp)     # number of records for each species    
    out <- numeric(length(n_sp))# vector to store estimate for each species
    
    # for each species, calculate the expected number of occurrences from n records
    for(i in 1:length(n_sp)) 
    {
      out[i] <- 1 - exp(lchoose((n - n_sp[i]), cutoff) - lchoose(n, cutoff))    
      # use lchoose (i.e. log scale) to avoid problems with big numbers
    }
    
    return(round(sum(out)))
  } # end function 
  
# Run function ----------------------------------------------------------
# matrix to store output
  out_rare <- matrix(nrow = length(cell_list)) # length 1009-1147 (not 2538)
  
# matrix with all cells
  rarefied_rich <- matrix(NA, nrow = length(getValues(raster)), ncol = 1)
  
# Cut off subsample
  cutoff <- 15
  
  for(j in 1:length(cell_list)) { 
    cell <- dplyr::filter(spp, cell == cell_list[j]) # return only the cells with records in them
    sp <- as.character(cell$species)
    if(cell$n_rec[1] < cutoff) out_rare[j] <- NA 
    else { out_rare[j, ] <- rarefaction_solo(sp, cutoff) 
    }
  }
  
# add the rarefied cells, one cutoff at a time  
  rarefied_rich[cell_list, 1] <- out_rare
  
  
# Plot
  est_rich_15 <- setValues(raster, rarefied_rich[, 1])
  plot(est_rich_15) 
  
  
# Removing off-shore cells -----------------------------------------------------------  
# Land/ocean & cell values
  cell.cat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/CSV/Terrestrial land categories.csv", header = T)
  
# Group and filter sampling effort (se)
  se <- cbind(cell.cat, n, a, chao1.df, rarefied_rich)
  se.terr <- filter(se, cell.cat == "terrestrial")
  
# generate list of occupied cells
  cell.list.x <- se.terr$cell.id
  
# make a matrix with all missing values (to get from 1003 cells to 2538)
  x <- matrix(NA, nrow = length(getValues(raster)), ncol = 4)
  
  se.m <- as.matrix(se.terr)
  se.mm <- se.m[, 3:6]
  class(se.mm) <- "numeric"
  
# add the occupied cells
  x[cell.list.x, ] <- se.mm
  
# Column names
  colnames(x) <- c("records", "sr", "chao1", "rarefied")
  
# Total records raster
  tot.rec.raster <- setValues(raster, x[,1])
  plot(tot.rec.raster)
  writeRaster(tot.rec.raster, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Sample effort comparisons/Total records.grd")
  
# Species richness raster
  sr.raster <- setValues(raster, x[,2])
  plot(sr.raster)
  writeRaster(rec.tot, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Sample effort comparisons/Species richness.grd")
  
# Chao1 raster
  chao1.raster <- setValues(raster, x[,3])
  plot(chao1.raster)
  writeRaster(rec.tot, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Sample effort comparisons/Chao1 species richness.grd")
  
# Rarefied richness raster
  rarefied.raster <- setValues(raster, x[,4])
  plot(rarefied.raster)
  writeRaster(rarefied.raster, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Sample effort comparisons/Rarefied to 15 species richness.grd")
  
  
# 3.5 Sample effort comparisons ----------------------------------------------------------
  cor <- cor(x, use = "complete.obs")
  
  write.csv(cor, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Sampling effort correlation matrix.csv", row.names = T)
  
  
  
  
  
  
  
  
  # -------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
