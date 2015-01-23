# Authors: Rob Maas & Wilmar van Ommeren
# Date: January 2015

# Load libraries
library(sp)
library(raster)
library(RColorBrewer)
library(igraph)
library(rgeos)
library(rasterVis)

# Load source scripts
source('./r/fill.up.R')
source('./r/calculate.breach.area.R')
source('./r/calculate.flooded.area.R')

# Load data
DEM <- raster("data/AHN 5m/ahn2_5_65az1.tif")

# Project parameter(s)
water.height <- 2   #meter
plot(DEM)          #needed for the click
no.of.breaches = 2
breach.point <- click(n=no.of.breaches)
breach.width = 150
breach.height = 1


# Calculate breach area
breach.area<-calculate.breach.area(breach.point, breach.width)

# Calculate flooded area
flooded.area <- calculate.flooded.area(breach.area, breach.height, water.height, DEM)

# Plot flooded area
Pallette <- colorRampPalette(brewer.pal(9, "Blues"))(20)
spplot (flooded.area, col.regions = Pallette, 
        main='Flooded Area', sub='Waterheight [m]', 
        xlab='Longitude',ylab='Latitude', scales = list(draw = TRUE)
        , sp.layout=list(list('sp.polygons', breach.area, col='red', first=FALSE)))

