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
if (!require(ggplot2)){install.packages("ggplot2")}

# Load source scripts
source('./r/fill.up.R')
source('./r/calculate.breach.area.R')
source('./r/calculate.flooded.area.R')
source('./r/merge.breach.DEM.R')
source('./r/create.openstreetmap.R')
source('./r/create.polygon.R')

# Load data
DEM <- raster('./data/AHNschouw/schouw.tif')

# # Example 1: single breach
water.height <- 2   #meter
plot(DEM)          #needed for the click
no.of.breaches = 1
breach.point <- click(n=no.of.breaches)
breach.width = 220

# Example 2: multiple breach
# water.height <- 2   #meter
# multiple.breach<- read.csv("./data/coords.csv")
# breach.point <- subset(multiple.breach, select=1:2)
# breach.width <- multiple.breach[3]


############# 1. Calculation of flooded area and base map ############# 
# Calculate breach area
breach.area<-calculate.breach.area(breach.point, breach.width)

# Include breach area in DEM
DEM.withbreach <- merge.breach.DEM(breach.area, DEM)

# Calculate flooded area
flooded.area <- calculate.flooded.area(breach.area, water.height, DEM, DEM.withbreach)

# Create openstreetmap layer for plot
osm <- create.openstreetmap(flooded.area)


############# 2. Plot basemap and flooded.area ############# 
# Create colors
waterPallette <- colorRampPalette(brewer.pal(9, "Blues"))(20)

# Create empty polygon to plot on (needed, because the osm needs to be added otherwise the projection will not be correct)
SPS <- create.polygon(flooded.area)

# Plot
plot(SPS, col='white', border='white')
plotRGB(raster(osm, crs=CRS(projection(flooded.area))), add=T)
plot(flooded.area, add=T, col=waterPallette)
plot(breach.area, add=T, col='red', border='red')


#############  3. Histogram of frequencies with the total flooded area ############# 
# Get cellsize
resolutionX <-xres(flooded.area)
resolutionY <-yres(flooded.area)

# Calculate Frequency and total flooded area
frequency <- freq(flooded.area, useNA='no')
frequency[,2]<-(frequency[,2]*(resolutionX*resolutionY))/1000000 
total.area.km2 <- sum(frequency[,2])

# Plot histogram of frequencies with the total flooded area
df <- data.frame(frequency) #Needed for plot
plot.title = 'Water Depth'
plot.subtitle = paste("Total flooded area:", format(round(total.area.km2, 2), nsmall=2),"km2")
qplot(df$value, df$count, geom="histogram", stat="identity", xlab="Meter", ylab="Area [km2]", fill=I("darkblue"), alpha=100)+
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), ""))))+
  scale_x_continuous(breaks=seq(min(df$value), max(df$value), 1)) + theme(legend.position='none')

