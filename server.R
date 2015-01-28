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

# Create plot from the inputvariables
shinyServer(function(input, output){
  DEM <- reactive({
    ## Create DEM
    # Select radiobutton 
    RB1 <- as.integer(input$RB1)
    
    # Create Datafile
    if (RB1==0){
      validate(need(input$country !="...", "Choose a country.\nIf a country is chosen the data will be downloaded to your harddisk!\nThe files can be deleted by pressing the remove button.\n\nScroll down for help (with examples)."))
      DEM<- getData('alt', country=input$country, mask=F)}
    else if (is.null(input$DEM)){
      validate(need(input$DEM !=NULL, "Upload a DEM from a specific region.\nRemember that the file extention should be .tif or .grd.\n\nScroll down for help (with examples)."))}
    else if (RB1==1){
      DEM<-raster(input$DEM$datapath)}
    
    # Transform DEM if necessary
    RijksDS<-"+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"
    if (projection(DEM)!=RijksDS){
      DEM<-withProgress(expr=projectRaster(DEM, crs=CRS(RijksDS)), 
                        message = '(Re-)Calculation in progress',
                        detail = 'Step 1: RE-projecting DEM...')}
    return (DEM)
  })
  
  breach.area<- reactive({
    ## Calculate the breach area
    
    
    # Get breach width if value is higher than 0
    validate(need(input$breach.width!=0, "Enter a breach width higher than 0.\nA breach of 0 can't cause a flood."))
    breach.width<- input$breach.width
    
    # Get breach point(s) if filled in or uploaded correctly
    RB2 <- as.integer(input$RB2)
    if (RB2 == 0){
      try(breach.point<-matrix(c(input$coord.x, input$coord.y), nrow=1, ncol=2))
      dimnames(breach.point)<-list(colnames(breach.point), c('x','y')) } 
    else if (is.null(input$coords)){
      validate(need(input$coords !=NULL, "Upload a .csv file with three columns representing the 'x' and 'y' coordinates and the breach width.\nThe first columns should be named 'x' and 'y'.\n\nScroll down for help (with examples)."))}
    else if (RB2==1){
      multiple.breach<- read.csv(input$coords$datapath)
      breach.point <- subset(multiple.breach, select=1:2)
      breach.width <- multiple.breach[3]}
    
    # Calculate breach area
    breach.area<-NULL # If NULL returned the plot function will only plot the DEM
    breach.area<- try(calculate.breach.area(breach.point, breach.width))
    
    return(breach.area)
  })
  water <- reactive({
    validate(need(input$water.height>0, "The waterlevel should be bigger than 0."))
    water.height<-input$water.height
  })
  
  flood <- reactive({
    ## Calculate the flooded area
    
    # Load inputvariables
    DEM <- DEM()
    breach.area<-breach.area()
    water.height<-water()
    
    # Include breach area in DEM
    DEM.withbreach <- withProgress(expr=try(merge.breach.DEM(breach.area, DEM)), 
                                   message = '(Re-)Calculation in progress',
                                   detail = 'Step 2: Merging breach with DEM...')
    
    # Calculate flooded area
    flooded.area<- withProgress(expr=try(calculate.flooded.area(breach.area, water.height, DEM, DEM.withbreach)), 
                                message = '(Re-)Calculation in progress',
                                detail = 'Step 3: Calculating the flooded area...')
    return(flooded.area)
  })
  
  output$plot<-renderPlot({
    ## Create plot (only after the goButton is clicked)
    input$goButton
    isolate({
    
    #Load inputvariables
    flooded.area<- flood()
    DEM <-DEM()
    breach.area<-breach.area()
    
    # Create colorPallettes
    WATERPallette <- colorRampPalette(brewer.pal(9, "Blues"))(20)
    DEMPallette<-colorRampPalette(c("darkseagreen","darkgreen","darkolivegreen","darkkhaki","darkgoldenrod", "cornsilk","beige"))(20)
    
    # Plot (only) if the button is clicked for the first time
    if (input$goButton==0){
      return(NULL)}
    else if (is.null(breach.area)){
      # If no breach area could be calculated only the DEM will be returned
      withProgress(spplot(DEM, col.regions=DEMPallette, main='Digital Elevation Map\nHeight in meters',
                          xlab='Longitude',ylab='Latitude', scales = list(draw = TRUE)), 
                   message = '(Re-)Calculation in progress',
                   detail = 'Step 4: Plotting the map...')} 
    else {
      # Plot if everything went correct
      withProgress(spplot (flooded.area, col.regions = WATERPallette, main='Flooded Area\nWaterdepth in meters',
                           xlab='Longitude',ylab='Latitude', scales = list(draw = TRUE), 
                           sp.layout=list(list('sp.polygons', breach.area, 
                                               col='red', fill='red', first=FALSE)))+
                     as.layer(spplot(DEM, col.regions=DEMPallette), under=T), 
                   message = '(Re-)Calculation in progress',
                   detail = 'Step 4: Plotting the map...')}})        
  })
  output$hist <- renderPlot({
    ## Histogram of frequencies with the total flooded area
    # Load data
    flooded.area<-flood()
    
    # Get cellsize
    resolutionX <-xres(flooded.area)
    resolutionY <-yres(flooded.area)
    
    # Calculate Frequency and total flooded area
    frequency <- freq(flooded.area, useNA='no')
    frequency[,2]<-(frequency[,2]*(resolutionX*resolutionY))/1000000
    total.area.km2 <- sum(frequency[,2])
    
    # Plot histogram of frequencies with the total flooded area
    df <- data.frame(frequency)#Needed for plot
    plot.title = 'Water Depth'
    plot.subtitle = paste("Total flooded area:", format(round(total.area.km2, 2), nsmall=2),"km2")
    qplot(df$value, df$count, geom="histogram", stat="identity", xlab="Meter", ylab="Area [km2]", fill=I("darkblue"), alpha=100)+
      ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), ""))))+
      scale_x_continuous(breaks=seq(min(df$value), max(df$value), 1)) + 
      theme(legend.position='none')
  })
  
  output$removed <- renderText({
    ## Outputtext that displays the files that are removed
    input$remove
    
    # Search for files
    files <- paste(list.files(pattern='*alt.grd|*alt.gri|*alt.vrt', full.names=T),collapse=', ')
    
    # Remove files
    if (files!=""){
      if (input$remove>0){
        file.remove(list.files(pattern='*alt.grd|*alt.gri|*alt.vrt', full.names=T))
        return(paste("Removed country DEM files:",files,"."))}}
  })
})