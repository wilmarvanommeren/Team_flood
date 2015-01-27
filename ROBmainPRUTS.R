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

# Plot flooded area
waterPallette <- colorRampPalette(brewer.pal(9, "Blues"))(20)
DEMPallette<-colorRampPalette(c("darkseagreen","darkgreen","darkolivegreen","darkkhaki","darkgoldenrod", "cornsilk","beige"))(20)


### INSERTING OPEN STREET MAP ###
# Extract the corners of the extent
topleft <- cbind(extent(flooded.area)[1], extent(flooded.area)[4])   # top left coordinate of extent
botright <- cbind(extent(flooded.area)[2], extent(flooded.area)[3])   # bottom right coordinate of extent

# Make the extent-corners spatial and give it the same projection as the flooded area.
extentcorners <- SpatialPoints(rbind(topleft, botright))
proj4string(extentcorners) <- crs(flooded.area)

# reproject extent to geographic coordinates to wgs84 for OpenStreetMap
extent.wgs84<- spTransform(extentcorners, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# retrieve basemap
osm <- openmap (c(bbox(extent.wgs84)[2,2], bbox(extent.wgs84)[1,1]), c(bbox(extent.wgs84)[2,1], bbox(extent.wgs84)[1,2]))

# reproject basemap back to flooded area projection
osm.trans <- openproj (osm, proj4string(extentcorners))

#plot
plot (osm.trans)
plot (flooded.area, add=T, col=waterPallette)


## plot flooded area
# plot(flooded.area,add=T,col=waterPallette)
# spplot (flooded.area, col.regions = waterPallette, 
#         main='Flooded Area', sub='Waterheight [m]', 
#         xlab='Longitude',ylab='Latitude', scales = list(draw = TRUE)
#         , sp.layout=list(list('sp.polygons', breach.area, col='red', first=FALSE)))+
#   as.layer(spplot(DEM, col=DEMPallette under=T))
