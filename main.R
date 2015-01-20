# Authors: Rob Maas & Wilmar van Ommeren
# Date: January 2015

# Load libraries
library(sp)
library(raster)
library(RColorBrewer)
library(igraph)

# Load source scripts
source('./r/fill.up.R')
source('./r/create.breach.R')

# Load data
DEM <- raster("data/AHN 5m/ahn_wageningen_5m.tif")

# Filled up DEM, no breach
waterheight <- 7#meter
DEM.filled <- fill.up(DEM, waterheight)

# Plot flooded areas
colorPal <- colorRampPalette(brewer.pal(9, "Blues"))(20) # Create color palette
spplot (DEM.filled,col.regions = colorPal, main='Waterheight [m]')

# Filled up DEM, single breach
plot(DEM)
breach<-drawExtent(show=T, col='red')
breachRast <- crop(DEM,breach)
breachRast <- create.breach(breachRast)
DEMwithbreach<-mosaic(DEM,breachRast,fun=min)
DEMwithbreach.filled <- fill.up(DEMwithbreach, waterheight)
spplot (DEMwithbreach.filled,col.regions = colorPal, main='Waterheight [m]')

# Clump flooded areas
clumps.flood<-clump(DEMwithbreach.filled)
clumps.floodEMP <- setValues(raster(clumps.flood), 1)#create empty raster
clumps.floodEMP[is.na(clumps.flood)] <- NA #set NA values
clumps.flood<-clump(clumps.floodEMP) #clump connected flooded area's

# Select clumps connected to breach
clump.intersect<-intersect(clumps.flood, breach)
clumps.breach<-unique(clump.intersect@data@values)
clumps.flood[clumps.flood!=clumps.breach]<-NA
plot(clumps.flood)

