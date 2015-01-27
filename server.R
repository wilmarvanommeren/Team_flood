# Authors: Rob Maas & Wilmar van Ommeren
# Date: January 2015

# Load & install packages if required
if (!require(shiny)){install.packages("shiny")}
if (!require(sp)){install.packages("sp")}
if (!require(igraph)){install.packages("igraph")}
if (!require(raster)){install.packages("raster")}
if (!require(rasterVis)){install.packages("rasterVis")}
if (!require(RColorBrewer)){install.packages("RColorBrewer")}
if (!require(proj4)){install.packages("proj4")}
if (!require(rgeos)){install.packages("rgeos")}
if (!require(rgdal)){install.packages("rgdal")}

# Load source scripts
source("./r/fill.up.R")
source("./r/calculate.breach.area.R")
source("./r/calculate.flooded.area.R")
source('./r/merge.breach.DEM.R')

# Plot flooded area with shiny
shinyServer(function(input, output){#Create plot from the inputvariables
  output$plot<-renderPlot({
    ## Create DEM
    # Select radiobutton and create CRS object to reproject
    RB1 <- as.integer(input$RB1) 
    # Load input datafile
    if (RB1==0){
      validate(need(input$country !="...", "Choose a country.\nIf a country is chosen the data will be downloaded to your harddisk!\nThe files can be deleted by pressing the remove button.\n\nScroll down for help."))
      DEM<- getData('alt', country=input$country, mask=F)}
    else if (is.null(input$DEM)){
      validate(need(input$DEM !=NULL, "Upload a DEM from a specific region.\nRemember that the file extention should be .tif or .grd.\n\nScroll down for help."))}
    else if (RB1==1){
      DEM<-raster(input$DEM$datapath)}
    
    # Transform DEM if necessary
    RijksDS<-"+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"
    if (projection(DEM)!=RijksDS){
      DEM<-projectRaster(DEM, crs=CRS(RijksDS))}
    
    ## Get other variables and isolate them so that the function only is executed if the button is clicked
    input$goButton
    breach.width<- isolate(as.numeric(input$breach.width))
    water.height<-isolate(as.numeric(input$water.height))
    breach.height<-0 # Set to 0 for this moment
    # Get breach point(s)
    RB2 <- as.integer(input$RB2)
    if (RB2 == 0){
      breach.point<-isolate(matrix(c(input$coord.x, input$coord.y), nrow=1, ncol=2))
      dimnames(breach.point)<-list(colnames(breach.point), c('x','y')) } 
    else if (is.null(input$coords)){
      validate(need(input$coords !=NULL, "Upload a .csv file with two columns representing the 'x' and 'y' coordinates.\nThe columns should be named 'x' and 'y'.\n\nScroll down for help."))}
    else if (RB2==1){
      breach.point<- isolate(read.csv(input$coords$datapath))}
    
    ## Calculate the outputplotdata
    # Calculate breach area
    breach.area<-NULL
    breach.area<- try(calculate.breach.area(breach.point, breach.width))
    
    # Include breach area in DEM
    DEM.withbreach <- withProgress(expr=try(merge.breach.DEM(breach.area, DEM)), 
                                   message = '(Re-)Calculation in progress',
                                   detail = 'Step 1: Merging breach with DEM...')
    
    # Calculate flooded area
    flooded.area<- withProgress(expr=try(isolate(calculate.flooded.area(breach.area, water.height, DEM, DEM.withbreach))), 
                                message = '(Re-)Calculation in progress',
                                detail = 'Step 2: Calculating the flooded area...')
    
    # ColorPallettes
    WATERPallette <- colorRampPalette(brewer.pal(9, "Blues"))(20)
    DEMPallette<-colorRampPalette(c("darkseagreen","darkgreen","darkolivegreen","darkkhaki","darkgoldenrod", "cornsilk","beige"))(20)
    
    ## Plot if the button is clicked for the first time
    if (input$goButton==0){
      return(NULL)}
    else if (is.null(breach.area)){
      withProgress(spplot(DEM, col.regions=DEMPallette, main='Digital Elevation Map\nHeight in meters',
                          xlab='Longitude',ylab='Latitude', scales = list(draw = TRUE)), 
                   message = '(Re-)Calculation in progress',
                   detail = 'Step 3: Plotting the map...')} 
    else {
      withProgress(spplot (flooded.area, col.regions = WATERPallette, main='Flooded Area\nWaterdepth in meters',
                           xlab='Longitude',ylab='Latitude', scales = list(draw = TRUE), 
                           sp.layout=list(list('sp.polygons', breach.area, 
                                               col='red', fill='red', first=FALSE)))+
                     as.layer(spplot(DEM, col.regions=DEMPallette), under=T), 
                   message = '(Re-)Calculation in progress',
                   detail = 'Step 3: Plotting the map...')}        
  })
  output$removed <- renderText({
    files <- paste(list.files(pattern='*alt.grd|*alt.gri|*alt.vrt', full.names=T),collapse=', ')
    input$remove
    if (files!=""){
      if (input$remove>0){
        file.remove(list.files(pattern='*alt.grd|*alt.gri|*alt.vrt', full.names=T))
        return(paste("Removed country DEM files:",files,"."))}}
  })
})