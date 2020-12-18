  
  
# library --------------------------------------------------
  library(raster)
  library(rgdal)
  library(dplyr)
  library(rmapshaper)
  
  rm(list = ls())
  
# 1 km template raster------------------------------------
  temp <- raster(res = 0.8333333,
                 crs = "+init=epsg:3577 +proj=aea +lat_1=-7 +lat_2=-45 +lat_0=112 +lon_0=155 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                 xmn = 110.8, xmx = 155.8,
                 ymn = -46.26667, ymx = -7.1)
  values(temp) <- 1:ncell(temp)
  plot(temp)
  
# shapefile
  oz <- readOGR("Data files/Australia/STE11aAust.shp")
  crs(oz) <- "+init=epsg:3577 +proj=aea +lat_1=-7 +lat_2=-45 +lat_0=112 +lon_0=155 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
# simplify outline
  oz2 <- ms_simplify(oz, keep = .01,    # proportion of points
                     weighting = 0.7) # smoothing index
  plot(oz2)
  
# convert back to 1 km 
  temp2 <- disaggregate(temp, fact = 100, method = "bilinear")
  
  temp3 <- mask(temp2, oz2)
  plot(temp3)
  
# save
  writeRaster(temp3, "Data files/Australia/Australia 1 km.grd", overwrite = T)
  
  writeOGR(oz2, ".", "Data files/Australia/Australia shapefile", driver = "ESRI Shapefile")
  
# -------------------------------------------------------------
