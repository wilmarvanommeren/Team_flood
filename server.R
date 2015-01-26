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
library(proj4)
library(rgdal)

# Load source scripts
source("./r/fill.up.R")
source("./r/calculate.breach.area.R")
source("./r/calculate.flooded.area.R")

# Plot flooded area with shiny
shinyServer(function(input, output){#Create plot from the inputvariables
  output$plot<-renderPlot({
    ##Create DEM's
    #Select radiobutton and create CRS object to reproject
    RB1 <- as.integer(input$RB1) 
    #load input datafile
    if (RB1==0){
        DEM<- getData('alt', country=input$country, mask=F)
        
    }             
    else if (is.null(input$DEM)){
       return (NULL)
     }
    else if (RB1==1){
       DEM<-raster(input$DEM$datapath)}

    #transform DEM if necessary
    proj<-"+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"
    if (projection(DEM)!=proj){
      DEM<-projectRaster(b, crs=CRS(proj))}

    ## Get other variables and isolate them so that the function only is executed if the button is clicked
    input$goButton
    breach.width<- isolate(as.numeric(input$breach.width))
    water.height<-isolate(as.numeric(input$water.height))
    breach.height<-0 #set to 0 for this moment
    
    # Get breach point(s)
    RB2 <- as.integer(input$RB2)
    if (RB2 == 0){
      breach.point<-isolate(matrix(c(input$coord.x, input$coord.y), nrow=1, ncol=2))
      dimnames(breach.point)<-list(colnames(breach.point), c('x','y')) 
    } else if (is.null(input$coords)){
      return (NULL)
    }
    else if (RB2==1){
      breach.point<-read.csv(input$coords$datapath)
    }
    
    ## Calculate the outputplotdata
    #Calculate breach area
    breach.area<-calculate.breach.area(breach.point, breach.width)
   
    # Calculate flooded area
    flooded.area<- isolate(calculate.flooded.area(breach.area, breach.height, water.height, DEM))

    # ColorPallettes
    WATERPallette <- colorRampPalette(brewer.pal(9, "Blues"))(20)
    DEMPallette<-colorRampPalette(c("darkseagreen","darkgreen","darkolivegreen","darkkhaki","darkgoldenrod", "cornsilk","beige"))(20)
    
    ## Plot if the button is clicked for the first time
    if (input$goButton==0){
      return(NULL)}
    else{
      spplot (flooded.area, col.regions = WATERPallette, 
              main='Flooded Area', sub='Waterheight [m]',
              xlab='Longitude',ylab='Latitude', 
              scales = list(draw = TRUE), 
              sp.layout=list(list('sp.polygons', breach.area, 
                                  col='red', fill='red', first=FALSE)))+
        as.layer(spplot(DEM, col.regions=DEMPallette), under=T)}
   })
})
