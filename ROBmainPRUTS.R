# Authors: Rob Maas & Wilmar van Ommeren
# Date: January 2015

# Load libraries
library(sp)
library(raster)
library(RColorBrewer)
library(igraph)
library(rgeos)
library(rgdal)
library(rasterVis)
library(rJava)
library(OpenStreetMap)
library(proj4)

# Load source scripts
source('./r/fill.up.R')
source('./r/calculate.breach.area.R')
source('./r/calculate.flooded.area.R')
source('./r/merge.breach.DEM.R')
source('./r/create.openstreetmap.R')

## Load data
# DEM <- getData('alt', country='NLD', mask=F)
DEM <- raster('data/AHNschouw/schouw.tif')
crs(DEM)

# Project parameter(s)
water.height <- 2   #meter
plot(DEM)          #needed for the click
no.of.breaches = 1
breach.point <- click(n=no.of.breaches)
breach.width = 150
breach.height = 1

# Calculate breach area
breach.area<-calculate.breach.area(breach.point, breach.width)

# Calculate flooded area
flooded.area <- calculate.flooded.area(breach.area, breach.height, water.height, DEM)

### Create (elements for) plot ###

## Create openstreetmap basemap layer for plot
osm <- create.openstreetmap(flooded.area)

## Northarrow
arrow.x <- extent(flooded.area)[1] + 0.02*(extent(flooded.area)[2]-extent(flooded.area)[1])
arrow.y <- extent(flooded.area)[4] - 0.1*(extent(flooded.area)[4]-extent(flooded.area)[3])
arrow.loc <- c(arrow.x,arrow.y)
arrow.scale <- (extent(flooded.area)[4]-extent(flooded.area)[3])/10

## Scalebar
scale.x <- extent(flooded.area)[2] - 0.12*(extent(flooded.area)[2]-extent(flooded.area)[1])
scale.y <- extent(flooded.area)[3] + 0.02*(extent(flooded.area)[4]-extent(flooded.area)[3])
scale.loc <- c(scale.x,scale.y)
scale.scale <- (extent(flooded.area)[2]-extent(flooded.area)[1])/10

## Scalebar text
# Text 1
scale.text1.x <- scale.x
scale.text1.y <- scale.y+0.02*(extent(flooded.area)[4]-extent(flooded.area)[3])
scale.text1.loc <- c(scale.text1.x,scale.text1.y)
scale.text1.txt <- "0km"

# Text 2
scale.text2.x <- extent(flooded.area)[2]- 0.03*(extent(flooded.area)[2]-extent(flooded.area)[1])
scale.text2.y <- scale.y+0.02*(extent(flooded.area)[4]-extent(flooded.area)[3])
scale.text2.loc <- c(scale.text2.x,scale.text2.y)
scale.text2.txt <- paste(scale.scale,"km")

## Colorpalettes
waterPallette <- colorRampPalette(brewer.pal(9, "Blues"))(20)
DEMPallette<-colorRampPalette(c("darkseagreen","darkgreen","darkolivegreen","darkkhaki","darkgoldenrod", "cornsilk","beige"))(20)

## Plot flooded area
spplot (flooded.area, col.regions = waterPallette, main='Flooded Area', sub='Waterheight [m]', 
        xlab='Longitude',ylab='Latitude', scales = list(draw = TRUE),
        sp.layout=list(list('sp.polygons', breach.area, col='red', fill='red', first=FALSE),
                       list("SpatialPolygonsRescale", layout.north.arrow(), offset=arrow.loc, scale=arrow.scale, fill=c('white','black')), 
                       list("SpatialPolygonsRescale", layout.scale.bar(), offset=scale.loc, scale=scale.scale, fill=c('white','black')),
                       list("sp.text", scale.text1.loc, scale.text1.txt, font=1, cex = 0.75),
                       list("sp.text", scale.text2.loc, scale.text2.txt, font=1, cex = 0.75)))
