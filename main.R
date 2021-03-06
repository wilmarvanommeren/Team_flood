# Authors: Rob Maas & Wilmar van Ommeren
# Date: January 2015
# Shinyapps: wilmarvanommeren.shinyapps.io/floodrisk

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
if (!require(GISTools)){install.packages("GISTools")}
if (!require(googleVis)){install.packages("googleVis")}
if (!require(xlsx)){install.packages("xlsx")}

# Load source scripts
source('./r/fill.up.R')
source('./r/calculate.breach.area.R')
source('./r/calculate.flooded.area.R')
source('./r/merge.breach.DEM.R')
source('./r/create.openstreetmap.R')
source('./r/create.polygon.R')

# # Example 1: single breach
# DEM <- raster('./data/AHNschouw/smallschouw.tif') #Use manually downloaded DEM
# water.height <- 2   #meter
# plot(DEM)          #needed for the click
# no.of.breaches = 1
# breach.point <- click(n=no.of.breaches)
# breach.width = 5

# Example 2: multiple breach
DEM <- getData('alt', country='NLD', mask=F) #Use automatically downloaded DEM
water.height <- 1   #meter

multiple.file <- "./data/multiCountry.csv" #Load multiple breaches
if (grepl("*.csv", multiple.file)){
  multiple.breach <- read.csv(multiple.file)
} else {
  multiple.breach<- read.xlsx(multiple.file, sheetIndex=1)
}
read.xlsx(multiple.file, sheetIndex=1)
grepl("*.csv", multiple.file)
# Reproject data
RijksDS<-"+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"
DEM<-projectRaster(DEM, crs=CRS(RijksDS))
breach.point <- subset(multiple.breach, select=1:2)
breach.width <- multiple.breach[3]

############# 1. Calculation of flooded area and base map ############# 
# Calculate breach area
breach.area<-calculate.breach.area(breach.point, breach.width)

# Include breach area in DEM
DEM.withbreach <- merge.breach.DEM(breach.area, DEM)

# Calculate flooded area
flooded.area <- calculate.flooded.area(breach.area, water.height, DEM.withbreach[[1]])

# Create openstreetmap layer for plot
osm <- create.openstreetmap(flooded.area)


############# 2. Plot basemap and flooded.area ############# 
# Create colors
waterPallette <- colorRampPalette(brewer.pal(9, "Blues"))(20)

# Create empty polygon to plot on (needed, because the osm needs to be added otherwise the projection will not be correct)
SPS <- create.polygon(flooded.area)

# Plot
plot(SPS, col='white', border='white', xlab='Longitude', ylab='Latitude', axes=T, bty='n',
     xaxs='i', yaxs='i', main="Waterdepth in flooded area's [m]")
box("plot", col="white")  
plotRGB(raster(osm), add=T)
grid()
plot(flooded.area, add=T, col=waterPallette)
plot(breach.area, add=T, col='red', border='red')

# Create and add a North arrow that takes the extent of the plot in account
x.position<-extent(flooded.area)[1] + 0.1*(extent(flooded.area)[2]-extent(flooded.area)[1])
y.position<-extent(flooded.area)[4] - 0.15*(extent(flooded.area)[4]-extent(flooded.area)[3])
length <- (((extent(flooded.area)[4])-(extent(flooded.area)[3]))*0.035)
north.arrow(xb=x.position, yb=y.position, len=length, lab="N")
  

#############  3. Histogram of frequencies with the total flooded area ############# 
# Get cellsize
resolutionX <-xres(flooded.area)
resolutionY <-yres(flooded.area)

# Calculate Frequency and total flooded area
frequency <- freq(flooded.area, useNA='no')
frequency[,2]<-(frequency[,2]*(resolutionX*resolutionY))/1000000 
total.area.km2 <- sum(frequency[,2])

# Create dataframe
df <- data.frame(frequency)
names(df) <- c("Meter",'Area [km2]')

# # Googlevis Interactive Plot
# ticks<-paste("[",toString(unlist(df[,1])),"]", sep="")
# gvischart<-gvisColumnChart(df, options=list(
#   title='Water Depth Distribution',
#   axisTitlesPositon="out", 
#   hAxis=paste("{title: 'Meter', titleTextStyle: {color: 'black'}, ticks:",ticks,"}",sep=""), 
#   vAxis="{title: 'Flooded Area [km2]', titleTextStyle: {color: 'black'}}",             
#   legend="{position: 'none'}",
#   height=400
# ))
# plot(gvischart)
# paste("Total flooded area:", format(round(total.area.km2, 2), nsmall=2),"km2.")

# Plot histogram of frequencies with the total flooded area
plot.title = 'Water Depth'
plot.subtitle = paste("Total flooded area:", format(round(total.area.km2, 2), nsmall=2),"km2")
qplot(df$value, df$count, geom="histogram", stat="identity", xlab="Meter", ylab="Area [km2]", fill=I("darkblue"), alpha=100)+
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), ""))))+
  scale_x_continuous(breaks=seq(min(df$value), max(df$value), 1)) + theme(legend.position='none')