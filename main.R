# Authors: Rob Maas & Wilmar van Ommeren
# Date: January 2015

# Load libraries
library(sp)
library(raster)
library(RColorBrewer)

# Load source scripts

# Load data
DEM <- raster("data/AHN 5m/ahn_wageningen_5m.tif")

# Filled up DEM
waterheight <- 8#meter
DEM.filled <- calc(DEM, fun=function(x){
  x[x>waterheight]<-NA;
  x<-x-waterheight;
  return (x)})

# Plot flooded areas
colorPal <- rev(colorRampPalette(brewer.pal(9, "Blues"))(20)) # Create color palette
spplot (DEM.filled,col.regions = colorPal, main='Waterheight [m]')

