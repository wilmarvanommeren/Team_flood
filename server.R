# Authors: Rob Maas & Wilmar van Ommeren
# Date: January 2015

# Load libraries
library(shiny)
library(sp)
library(raster)
library(RColorBrewer)
library(igraph)
library(rgeos)
library(rasterVis)

options(shiny.maxRequestSize = 30*1024^2)#set file upload
# Load source scripts
source("G:/Mijn documenten/Wageningen/Geoscripting/Team_flood/r/fill.up.R")
source("G:/Mijn documenten/Wageningen/Geoscripting/Team_flood/r/calculate.breach.area.R")
source("G:/Mijn documenten/Wageningen/Geoscripting/Team_flood/r/calculate.flooded.area.R")

# Plot flooded area with shiny
shinyServer(function(input, output){#Create plot from the inputvariables
  output$plot<-renderPlot({
    #Create DEM's
    RB1 <- as.integer(input$RB1) #Select radiobutton
    crs<-CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs")
    if (RB1==0){#load input datafile
      DEM<- projectRaster(getData('alt', country='NLD', mask=F),crs=crs)
    } else if (is.null(input$DEM)){
      return (NULL)
    }
      else if (RB1==1){
      DEM<-raster(input$DEM$datapath)}
 
    # Get other variables
    input$goButton
    breach.width<- isolate(as.numeric(input$breach.width))
    water.height<-isolate(as.numeric(input$water.height))
    breach.height<-0 #set to 0 for this moment
    breach.point<-isolate(matrix(c(input$coord.x, input$coord.y), nrow=1, ncol=2))
    dimnames(breach.point)<-list(colnames(breach.point), c('x','y'))
     
    # Calculate breach area
    breach.area<-calculate.breach.area(breach.point, breach.width)

    # Calculate flooded area
    flooded.area <- calculate.flooded.area(breach.area, breach.height, water.height, DEM)
     
     # ColorPallettes
     WATERPallette <- colorRampPalette(brewer.pal(9, "Blues"))(20)
     DEMPallette<-colorRampPalette(c("darkseagreen","darkgreen","darkolivegreen","darkkhaki","darkgoldenrod", "cornsilk","beige"))(20)
     
     plot function (adds a layer of the DEM under the result)
     spplot (flooded.area, col.regions = WATERPallette, 
            main='Flooded Area', sub='Waterheight [m]',
            xlab='Longitude',ylab='Latitude', 
            scales = list(draw = TRUE), 
            sp.layout=list(list('sp.polygons', breach.area, 
                                col='red', fill='red', first=FALSE)))+
     as.layer(spplot(DEM, col.regions=DEMPallette), under=T)
  })
})
