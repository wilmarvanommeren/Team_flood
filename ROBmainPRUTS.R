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

topleft <- c(extent(flooded.area)[1],extent(flooded.area)[4])
botright <- c(extent(flooded.area)[2],extent(flooded.area)[3])
extent(flooded.area)

osmproj <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

tlosm <- ptransform(topleft, crs(flooded.area), osmproj)
brosm <- ptransform(botright, crs(flooded.area), osmproj)

map <- openmap(tlosm,brosm,type='osm')

tlosm
# flood.osmextent <- projectExtent(flooded.area, osmproj)
# flood.osmextent@extent[2]
# topleft <- c(flood.osmextent@extent[1],flood.osmextent@extent[3])
# botright <- c(flood.osmextent@extent[2],flood.osmextent@extent[3])
# map <- openmap(topleft,botright,type='osm')
# plot(flood.osmextent)

?SpatialPolygons

 <- spTransform(extentpoly,osm())

map <- openmap(c(51.73744715506255,3.6388778686523438),c(51.63847621195153,3.99078369140625),type='osm')

crs(map)
a<-map$tiles

a$projection@projargs

map_trans <- openproj(topleft, projection = osm())
plot(map)
plot(map_trans, raster=TRUE)
plot(flooded.area,add=T,col=waterPallette)
?openmap

?projectExtent
a<-map[3]
str(a)
plot(map_longlat,raster=TRUE)
map("world",col="red",add=TRUE)

## plot flooded area
plot(flooded.area,add=T,col=waterPallette)
spplot (flooded.area, col.regions = waterPallette, 
        main='Flooded Area', sub='Waterheight [m]', 
        xlab='Longitude',ylab='Latitude', scales = list(draw = TRUE)
        , sp.layout=list(list('sp.polygons', breach.area, col='red', first=FALSE)))+
  as.layer(spplot(DEM, col.regions=DEMPallette), under=T)
