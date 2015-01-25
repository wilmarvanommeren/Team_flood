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
    #Get variables for functions
    if (is.null(input$DEM)){
      return(NULL)} #return NULL with no input data
    DEM <- input$DEM #select input data.frame
    DEM<-raster(DEM$datapath) #load input datafile
    
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
    DEMPallette<-colorRampPalette(c("darkseagreen","darkgreen","darkolivegreen","darkkhaki","darkgoldenrod"))(20)
    
    #plot function (adds a layer of the DEM under the result)
    spplot (flooded.area, col.regions = WATERPallette, 
            main='Flooded Area', sub='Waterheight [m]',
            xlab='Longitude',ylab='Latitude', 
            scales = list(draw = TRUE), 
            sp.layout=list(list('sp.polygons', breach.area, 
                                col='red', fill='red', first=FALSE)))+
      as.layer(spplot(DEM, col.regions=DEMPallette), under=T)
  })
  output$coord<- renderTable({
    if(is.null(input$click)){return(NULL)}
    data.frame(input$click)
  })
  
})
