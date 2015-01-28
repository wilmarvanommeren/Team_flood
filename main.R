# Authors: Rob Maas & Wilmar van Ommeren
# Date: January 2015

# Load & install packages if required
if (!require(sp)){install.packages("sp")}
if (!require(igraph)){install.packages("igraph")}
if (!require(raster)){install.packages("raster")}
if (!require(rasterVis)){install.packages("rasterVis")}
if (!require(RColorBrewer)){install.packages("RColorBrewer")}
if (!require(proj4)){install.packages("proj4")}
if (!require(rgeos)){install.packages("rgeos")}
if (!require(rgdal)){install.packages("rgdal")}
if (!require(rJava)){install.packages("rJava")}
if (!require(OpenStreetMap)){install.packages("OpenStreetMap")}

# Load source scripts
source('./r/fill.up.R')
source('./r/calculate.breach.area.R')
source('./r/calculate.flooded.area.R')
source('./r/merge.breach.DEM.R')
source('./r/create.openstreetmap.R')

# Load data
DEM <- raster('./data/AHNschouw/ahn2_5_64gn2.tif')

# Project parameter(s)
water.height <- 2   #meter
plot(DEM)          #needed for the click
no.of.breaches = 1
breach.point <- click(n=no.of.breaches)
breach.width = 220

# Example multiple breach
multiple.breach<- read.csv("G:/Mijn documenten/Wageningen/Geoscripting/Team_flood/data/coords.csv")
breach.point <- subset(multiple.breach, select=1:2)
breach.width <- multiple.breach[3]

# Calculate breach area
breach.area<-calculate.breach.area(breach.point, breach.width)

# Include breach area in DEM
DEM.withbreach <- merge.breach.DEM(breach.area, DEM)


# Calculate flooded area
flooded.area <- calculate.flooded.area(breach.area, water.height, DEM, DEM.withbreach)

# Create openstreetmap layer for plot
osm <- create.openstreetmap(flooded.area)

# Plot flooded area
waterPallette <- colorRampPalette(brewer.pal(9, "Blues"))(20)
DEMPallette<-colorRampPalette(c("darkseagreen","darkgreen","darkolivegreen","darkkhaki","darkgoldenrod", "cornsilk","beige"))(20)
spplot (flooded.area, col.regions = waterPallette, 
        main='Flooded Area', sub='Waterheight [m]', 
        xlab='Longitude',ylab='Latitude', scales = list(draw = TRUE)
        , sp.layout=list(list('sp.polygons', breach.area, col='red', fill='red', first=FALSE
                              )
                         , sp.raster))

# Total flooded area
resolutionX <-xres(flooded.area)
resolutionY <-yres(flooded.area)
frequency <- freq(flooded.area, useNA='no') 
total.area.m2 <- sum(frequency[,2])*(resolutionX*resolutionY)
total.area.km2 <- total.area.m2/1000000

paste("The total flooded area is", format(round(total.area.km2, 2), nsmall=2), 'km2.')