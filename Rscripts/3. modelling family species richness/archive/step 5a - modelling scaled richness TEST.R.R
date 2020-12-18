########################################################
# modelling relationships between non-native families and environmental variables
########################################################


# library ----------------------------------------------
library(dplyr)
library(broom)
library(magrittr)
library(raster)

rm(list = ls())

# data --------------------------------------------------
setwd("C:/Users/s436862/Dropbox/Non-native-families/Results/rasters/scaled")

current.list <- list.files(pattern = "observed.grd")
names <- gsub(pattern = ".observed.grd$", "", current.list)

c.stack <- stack(current.list)
names(c.stack) <- names

list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)

spp <- as.data.frame(c.stack, na.rm = F)
glimpse(spp)

setwd("C:/Users/s436862/Dropbox/Non-native-families")

# environmental and anthropogenic varaible selection ------------
# note this is also the order these pop up in the coefficient plots, so vary it here if need be
evs <- read.csv("C:/Users/s436862/Dropbox/Poaceae/Results/EVs/CSV/100 km EFs scaled.csv", header = T)

spp.ev <- cbind(spp, evs) %>%
  filter(cell.category.v2 == "land") %>%
  dplyr::select(-cell.id, -cell.category.v1, -cell.category.v2)
head(spp.ev)

factors <- c("prop.cover", "amt",   "ap",     "arid",   "clay",
             "elev",       "hii",   "iso",    "mdr",    "pawc",
             "pcoldq" ,    "pdrym", "pet",    "pewc",   "ps",
             "pwarmq",     "pwetm", "pwetq" , "rz",     "sp",
             "st",         "tar",   "tcoldm", "tcoldq", "tdryq",
             "th",         "ts",    "twarmm", "twarmq", "twetq")

  as.formula(paste("y~", paste(factors, collapse="+")))
# e.g. 
  m1 <- lm(Asteraceae.native ~ .,data = spp.ev[, c(1, factors)])

# vs_factors <- c("prop.cover", 
#                 "amt",   
#                 "arid", 
#                 "ts",
#                 "pcoldq" ,
#                 "pwarmq",       
#                 "th",
#                 "hii")




# models --------------------------------------------------------------
# Asteraceae ----------------------------------------------------------
# native
m1 <- lm(Asteraceae.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Asteraceae.native"), 35:64)]) #1 = spp col
summary(m1) 
m1.sum <- tidy(m1)
m1.ci <- confint(m1)
# non-native
m2 <- lm(Asteraceae.non.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Asteraceae.non.native"), 35:64)])
summary(m2) 
m2.sum <- tidy(m2)
m2.ci <- confint(m2)

# Brassicaceae ----------------------------------------------------------
# native
m3 <- lm(Brassicaceae.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Brassicaceae.native"), 35:64)])
summary(m3) 
m3.sum <- tidy(m3)
m3.ci <- confint(m3)

# non-native
m4 <- lm(Brassicaceae.non.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Brassicaceae.non.native"), 35:64)])
summary(m4) 
m4.sum <- tidy(m4)
m4.ci <- confint(m4)

# Cyperaceae ----------------------------------------------------------
# native
m5 <- lm(Cyperaceae.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Cyperaceae.native"), 35:64)])
summary(m5) 
m5.sum <- tidy(m5)
m5.ci <- confint(m5)

# non-native
m6 <- lm(Cyperaceae.non.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Cyperaceae.non.native"), 35:64)])
summary(m6) 
m6.sum <- tidy(m6)
m6.ci <- confint(m6)

# Fabaceae ----------------------------------------------------------
# native
m7 <- lm(Fabaceae.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Fabaceae.native"), 35:64)])
summary(m7) 
m7.sum <- tidy(m7)
m7.ci <- confint(m7)

# non-native
m8 <- lm(Fabaceae.non.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Fabaceae.non.native"), 35:64)])
summary(m8) 
m8.sum <- tidy(m8)
m8.ci <- confint(m8)

# Malvaceae ----------------------------------------------------------
# native
m9 <- lm(Malvaceae.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Malvaceae.native"), 35:64)])
summary(m9) 
m9.sum <- tidy(m9)
m9.ci <- confint(m9)

# non-native
m10 <- lm(Malvaceae.non.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Malvaceae.non.native"), 35:64)])
summary(m10) 
m10.sum <- tidy(m10)
m10.ci <- confint(m10)

# Myrtaceae ---------------------------------------------------------
# native
m11 <- lm(Myrtaceae.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Myrtaceae.native"), 35:64)])
summary(m11) 
m11.sum <- tidy(m11)
m11.ci <- confint(m11)

# non-native
m12 <- lm(Myrtaceae.non.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Myrtaceae.non.native"), 35:64)])
summary(m12) 
m12.sum <- tidy(m12)
m12.ci <- confint(m12)

# Plantae ----------------------------------------------------------
# native
m13 <- lm(Plantae.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Plantae.native"), 35:64)])
summary(m13) 
m13.sum <- tidy(m13)
m13.ci <- confint(m13)

# non-native
m14 <- lm(Plantae.non.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Plantae.non.native"), 35:64)])
summary(m14) 
m14.sum <- tidy(m14)
m14.ci <- confint(m14)

# Proteaceae ----------------------------------------------------------
# native
m15 <- lm(Proteaceae.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Proteaceae.native"), 35:64)])
summary(m15) 
m15.sum <- tidy(m15)
m15.ci <- confint(m15)

# non-native
# m16 <- lm(Proteaceae.non.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Proteaceae.native"), 35:64)])
# summary(m16) 
# m16.sum <- tidy(m16)
# m16.ci <- confint(m16)

# Solanaceae ----------------------------------------------------------
# keeping the same order of model numbers because Proteaceae ain't got none of them exotics
# native
m16 <- lm(Solanaceae.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Solanaceae.native"), 35:64)])
summary(m16) 
m16.sum <- tidy(m16)
m16.ci <- confint(m16)

# non-native
m17 <- lm(Solanaceae.non.native ~ .,data = spp.ev[, c(which(colnames(spp.ev)=="Solanaceae.non.native"), 35:64)])
summary(m17) 
m17.sum <- tidy(m17)
m17.ci <- confint(m17)

# ----------------------------------------------------------------------

# coefficient data (array format) --------------------------------------
# hooray for the array

# dim 1: rows; coefficient names 
row.names <- rep(vs_factors[2:8], 2) # note we are dropping prop.cover

# dim 2: columns:
#               parameter (names of evs)
#               estimate (mean estimate)
#               lower.ci (lower confidence interval)
#               upper.ci (lower confidence interval)
#               plot.names (proper parameter names; I fill in next script)

col.names <- c("status", "estimate", "lower.ci", "upper.ci", "plot.names")

# dim 3: matrices, based on each family (9-sih)
matrix.names <- c("Asteraceae",
                  "Brassicaceae",
                  "Cyperaceae",
                  "Fabaceae",
                  "Malvaceae",
                  "Myrtaceae",
                  "Plantae",
                  "Proteaceae",
                  "Solanaceae")
# array    
coef.ar <- array(data = NA,
                 dim = c(14, 6, 9),
                 dimnames = list(row.names, col.names, matrix.names))

coef.ar[1:14,2,] <- row.names
coef.ar[1:14,"status",] <- rep(c("native", "non-native"), each = 7)
coef.ar[1:14,,1]

# note -------------------------------------------------- 
# 2:8 excludes the intercept and prop.cover terms
# 'tibble' makes extracting estimates weird and difficult  
# odd model numbers are native; odd are non-native
# e.g. m1 is native and m2 is non-native
# except at the end with proteaceae, it only has one so for Solanceae it's switched
# -------------------------------------------------------

# Asteraceae --------------------------------------------
# m1  
coef.ar[1:7,3,"Asteraceae"] <- as.data.frame(m1.sum[2:8,2])[,1]
coef.ar[1:7,4:5,"Asteraceae"] <- m1.ci[2:8,]

# m2  
coef.ar[8:14,3,"Asteraceae"] <- as.data.frame(m2.sum[2:8,2])[,1]
coef.ar[8:14,4:5,"Asteraceae"] <- m2.ci[2:8,]

# Brassicaceae ------------------------------------------
# m3  
coef.ar[1:7,3,"Brassicaceae"] <- as.data.frame(m3.sum[2:8,2])[,1]
coef.ar[1:7,4:5,"Brassicaceae"] <- m3.ci[2:8,]

# m4  
coef.ar[8:14,3,"Brassicaceae"] <- as.data.frame(m4.sum[2:8,2])[,1]
coef.ar[8:14,4:5,"Brassicaceae"] <- m4.ci[2:8,]

# Cyperaceae ------------------------------------------  
# m5  
coef.ar[1:7,3,"Cyperaceae"] <- as.data.frame(m5.sum[2:8,2])[,1]
coef.ar[1:7,4:5,"Cyperaceae"] <- m5.ci[2:8,]

# m6  
coef.ar[8:14,3,"Cyperaceae"] <- as.data.frame(m6.sum[2:8,2])[,1]
coef.ar[8:14,4:5,"Cyperaceae"] <- m6.ci[2:8,]

# Fabaceae ------------------------------------------   
# m7  
coef.ar[1:7,3,"Fabaceae"] <- as.data.frame(m7.sum[2:8,2])[,1]
coef.ar[1:7,4:5,"Fabaceae"] <- m7.ci[2:8,]

# m8  
coef.ar[8:14,3,"Fabaceae"] <- as.data.frame(m8.sum[2:8,2])[,1]
coef.ar[8:14,4:5,"Fabaceae"] <- m8.ci[2:8,]

# Malvaceae ------------------------------------------     
# m9  
coef.ar[1:7,3,"Malvaceae"] <- as.data.frame(m9.sum[2:8,2])[,1]
coef.ar[1:7,4:5,"Malvaceae"] <- m9.ci[2:8,]

# m10  
coef.ar[8:14,3,"Malvaceae"] <- as.data.frame(m10.sum[2:8,2])[,1]
coef.ar[8:14,4:5,"Malvaceae"] <- m10.ci[2:8,]

# Myrtaceae ------------------------------------------     
# m11  
coef.ar[1:7,3,"Myrtaceae"] <- as.data.frame(m11.sum[2:8,2])[,1]
coef.ar[1:7,4:5,"Myrtaceae"] <- m11.ci[2:8,]

# m12  
coef.ar[8:14,3,"Myrtaceae"] <- as.data.frame(m12.sum[2:8,2])[,1]
coef.ar[8:14,4:5,"Myrtaceae"] <- m12.ci[2:8,]

# Plantae -------------------------------------------------     
# m13  
coef.ar[1:7,3,"Plantae"] <- as.data.frame(m13.sum[2:8,2])[,1]
coef.ar[1:7,4:5,"Plantae"] <- m13.ci[2:8,]

# m14  
coef.ar[8:14,3,"Plantae"] <- as.data.frame(m14.sum[2:8,2])[,1]
coef.ar[8:14,4:5,"Plantae"] <- m14.ci[2:8,]

# Proteaceae -----------------------------------------------     
# m15  
coef.ar[1:7,3,"Proteaceae"] <- as.data.frame(m15.sum[2:8,2])[,1]
coef.ar[1:7,4:5,"Proteaceae"] <- m15.ci[2:8,]

# Solanaceae ------------------------------------------------
# m16  
coef.ar[8:14,3,"Solanaceae"] <- as.data.frame(m16.sum[2:8,2])[,1]
coef.ar[8:14,4:5,"Solanaceae"] <- m16.ci[2:8,]

# m17  
coef.ar[1:7,3,"Solanaceae"] <- as.data.frame(m17.sum[2:8,2])[,1]
coef.ar[1:7,4:5,"Solanaceae"] <- m17.ci[2:8,]

# check residuals ------------------------------------------------------
par(mfrow = c(2, 2))
plot(m1$residuals)
plot(m2$residuals)
plot(m3$residuals)
plot(m4$residuals)

plot(m5$residuals)
plot(m6$residuals)
plot(m7$residuals)
plot(m8$residuals)

plot(m9$residuals)
plot(m10$residuals)
plot(m11$residuals)
plot(m12$residuals)

plot(m13$residuals)
plot(m14$residuals)
plot(m15$residuals)
plot(m16$residuals)

plot(m17$residuals)

# histogram  
hist(m1$residuals)
hist(m2$residuals)
hist(m3$residuals)
hist(m4$residuals)

hist(m5$residuals)
hist(m6$residuals)
hist(m7$residuals)
hist(m8$residuals)

hist(m9$residuals)
hist(m10$residuals)
hist(m11$residuals)
hist(m12$residuals)

hist(m13$residuals)
hist(m14$residuals)
hist(m15$residuals)
hist(m16$residuals) 

hist(m17$residuals)

# save Rdata ---------------------------------------------------------  
save.image("Data files/Rdata/model_coefficients.RData")

# ---------------------------------------------------------------------  
