# Authors: Rob Maas & Wilmar van Ommeren
# Date: January 2015

# Load libraries
library(sp)
library(raster)
library(RColorBrewer)
library(igraph)
library(rgeos)

# Load source scripts
source('./r/fill.up.R')
source('./r/create.breach.R')

# Load data
DEM <- raster("data/AHN 5m/ahn2_5_65az1.tif")
plot(DEM)

# Project parameter(s)
waterheight <- 2#meter
breach.point <- click()
breach.width = 150
breach.height = 0

# Plot colorPallettes
Watercol <- colorRampPalette(brewer.pal(9, "Blues"))(20) 

# Create spatial breach point
breach.point<-data.frame(breach.point)
coordinates(breach.point) <- ~x+y

# Create spatial breach point buffer
breach.area<-buffer(breach.point, width =breach.width)
plot(breach.area, add=T)

# extract values and select height of breach
breach.mask <- mask(DEM, breach.area)
breach.rast <- setValues(breach.mask, breach.height)#create empty raster
breach.rast[is.na(breach.mask)] <- NA #set NA values

# combine DEM & breach
DEMwithbreach <- mosaic(DEM, breach.rast,fun=min)
DEMwithbreach.filled <- fill.up(DEMwithbreach, waterheight)

# Clump flooded areas
clump.flood<-clump(DEMwithbreach.filled)
clump.floodEMP <- setValues(raster(clump.flood), 1)#create empty raster
clump.floodEMP[is.na(clump.flood)] <- NA #set NA values
clump.flood<-clump(clump.floodEMP) #clump connected flooded area's

# Select clump connected to breach
clump.intersect<-intersect(clump.flood, breach.area)
clump.connect<-unique(clump.intersect@data@values)
flooded.area<-clump.connect[!is.na(clump.connect)]
clump.flood[clump.flood!=flooded.area]<-NA

# Set waterheight
water.dept <- mask(DEMwithbreach.filled, clump.flood)
spplot (water.dept, col.regions = Watercol, 
        main='Flooded Area', sub='Waterheigth [m]', 
        xlab='Longitude',ylab='Latitude', scales = list(draw = TRUE),
        sp.layout=list(list("sp.polygons", breach.area, col='red',
                            fill='red',first=FALSE))
        )

