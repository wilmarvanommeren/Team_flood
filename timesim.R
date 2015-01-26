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


DEM <- raster("data/AHNschouw/schouw.tif")

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

## Time simulation loop ##
waterPallette <- colorRampPalette(brewer.pal(9, "Blues"))(20)
DEMPallette<-colorRampPalette(c("darkseagreen","darkgreen","darkolivegreen","darkkhaki","darkgoldenrod"))(20)
# testPallette <- colorRampPalette(c("red","orange","yellow","green","blue","purple"))

bufferstep <- 2000  # meter
plot(DEM, legend=F)
plot(breach.area,add=T,col='black')

for(i in 1:3){
  bufferdistprev <- bufferstep*(i-1)
  bufferdistnow <- bufferstep*i
  floodbuffer <- buffer(breach.area, width = bufferdistnow) - buffer(breach.area,width = bufferdistprev)
  flood.sub <- crop(flooded.area, extent(floodbuffer))
  floodmask <- mask(flood.sub,floodbuffer)
  if(i==1){plot(floodmask,add=T,col=waterPallette)}else{plot(floodmask,add=T,col=waterPallette,legend=F)}   
}

# testing outside loop
i <- 3

bufferstep <- 2000  # meter
plot(DEM, legend=F)
plot(breach.area,add=T,col='black')

bufferdistprev <- bufferstep*(i-1)
bufferdistnow <- bufferstep*i
floodbuffer <- buffer(breach.area, width = bufferdistnow) - buffer(breach.area,width = bufferdistprev)
flood.sub <- crop(flooded.area, extent(floodbuffer))
floodmask <- mask(flood.sub,floodbuffer)
if(i==1){plot(floodmask,add=T,col=waterPallette)}else{plot(floodmask,add=T,col=waterPallette,legend=F)}

