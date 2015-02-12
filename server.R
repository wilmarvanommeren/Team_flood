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
if (!require(GISTools)){install.packages("GISTools")}
if (!require(googleVis)){install.packages("googleVis")}
if (!require(xlsx)){install.packages("xlsx")}
if (!require(shiny)){install.packages("shiny")}

# Load source scripts
source('./r/fill.up.R')
source('./r/calculate.breach.area.R')
source('./r/calculate.flooded.area.R')
source('./r/merge.breach.DEM.R')
source('./r/create.openstreetmap.R')
source('./r/create.polygon.R')

# Create plot from the inputvariables
shinyServer(function(input, output){
  
  ############# 1. Create DEM and Basemap #############
  ## Create DEM
  DEM <- reactive({
    # Select radiobutton 
    RB1 <- as.integer(input$RB1)
    
    # Create Datafile
    if (RB1==0){#Check if country is chosen and get data
      validate(need(input$country !="...", "Choose a country.\nIf a country is chosen the data will be downloaded to your harddisk!\n\nTip: The files can be deleted by pressing the remove button.\n\nScroll down for help (with examples)."))
      DEM<- getData('alt', country=input$country, mask=F)}
    else if (is.null(input$DEM)){#Check coordinates are uploaded
      validate(need(input$DEM !=NULL, "Upload a DEM from a specific region.\n\nTip: Remember that the file extention should be .tif or .grd.\n\nScroll down for help (with examples)."))}
    else if (RB1==1){#If uploaded Dem is located in the datapath
      DEM<-raster(input$DEM$datapath)}
    
    # Transform DEM if necessary
    RijksDS<-"+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"
    if (grepl("+proj=stere*", projection(DEM))==FALSE){
      DEM<-withProgress(expr=projectRaster(DEM, crs=CRS(RijksDS)), 
                        message = '(Re-)Calculation in progress',
                        detail = 'RE-projecting DEM...')}
    return (DEM)
  })
  
  ## Create base map for final plot
  osm <- reactive({
    # Load DEM
    DEM <- DEM()
    
    # Create base map
    osm <- withProgress(create.openstreetmap(DEM), 
                        message = '(Re-)Calculation in progress',
                        detail = 'Creating base map...')
    return(osm)
  })
  
  ############# 2. Calculate variables and rasters #############
  ## Calculate the breach area 
  breach.area<- reactive({ 
    # Get breach point(s) if filled in or uploaded correctly 
    RB2 <- as.integer(input$RB2) 
    DEM <- DEM()
    
    # Extract breach width(S) and breach point(s)
    if (RB2 == 0){
      # Get cellsize for minimum breach width
      cellsize <-min(xres(DEM), yres(DEM))
      
      # Get single breach width and point if width is higher than 0
      validate(need(input$breach.width>=cellsize, paste("No breach is created.\n\nTip: Enter a breach width higher than the minimum cell resolution of your input DEM (the minimum cell resolution is ", round(cellsize)," meter).\n\nScroll down for help.", sep=""))) 
      breach.width<- input$breach.width 
      breach.point<-matrix(c(input$coord.x, input$coord.y), nrow=1, ncol=2) 
      dimnames(breach.point)<-list(colnames(breach.point), c('x','y')) }#Names needed for creating spatialpoints  
    else if (is.null(input$coords)){#Check if file is uploaded
      validate(need(input$coords !=NULL, "Upload a .csv|.xls|.xlsx file with three columns (or comma's per row) representing the 'x' and 'y' coordinates and the breach width.\n\nTip: The columns should be named 'x' and 'y'.\n\nScroll down for help."))
    }
    else if (RB2==1){#if uploaded read the file and create a subset of points and widths
      # Convert to data.frame
      multiple.file <- toString(input$coords$name)
      if (grepl("*.csv", multiple.file)){
        multiple.breach <- read.csv(input$coords$datapath)} 
      else {
        multiple.breach<- read.xlsx(input$coords$datapath, sheetIndex=1)}
      
      # Create subset
      breach.point <- subset(multiple.breach, select=1:2) 
      breach.width <- multiple.breach[3]} 
    
    # Calculate breach area 
    breach.area<- try(calculate.breach.area(breach.point, breach.width)) 
    
    return(breach.area) 
  })
  
  ## Include breach area in DEM
  DEM.withbreach <- reactive({
    # Get variables
    breach.area <- breach.area()
    DEM <- DEM()
    
    DEM.withbreach <- withProgress(expr=try(merge.breach.DEM(breach.area, DEM)), 
                                   message = '(Re-)Calculation in progress',
                                   detail = 'Merging breach with DEM...')
    return(DEM.withbreach)
  })
  
  ## Return waterheight
  water <- reactive({
    # Get minimum height
    DEM.withbreach <- DEM.withbreach()
    height <- DEM.withbreach[[2]]
    
    # Check if height is higher than minimum
    validate(need(input$water.height>height, paste("No area is flooded!\n\nTip: The waterlevel should be higher than the minimum height that is needed to flood all breaches.\nThe minimum waterlevel is: ", format(round(height, 2), nsmall=2), " meter.", sep="")))
    
    return(input$water.height)
  })
  
  ## Calculate the flooded area
  flood <- reactive({
    # Load inputvariables
    breach.area <- breach.area()
    water.height <- water()
    DEM.withbreach <- DEM.withbreach()
    
    # Calculate flooded area
    flooded.area <- withProgress(expr=try(calculate.flooded.area(breach.area, water.height, DEM.withbreach[[1]])), 
                                 message = '(Re-)Calculation in progress',
                                 detail = 'Calculating the flooded area...')
    return(flooded.area)
  })
  
  
  ## Frequencies of the total flooded area needed for histogram and total flooded area
  frequency <- reactive({
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
    names(df) <- c("Meter",'Area [km2]')
    
    return(df)
  })  

  ############# Create output #############
  ## Create baseplot
  output$plot<-renderPlot({
    # Load base map
    osm<-osm()# Start creating osm before the go button is clicked
    DEM <-DEM()
    flooded.area <- NULL
    

    # Create polygon and plot OSM
    SPS <- create.polygon(DEM)
    plot(SPS, col='white', border='white', axes=T, bty='n',xaxs='i', yaxs='i', 
         xlab='Longitude', ylab='Latitude',main="Waterdepth in the flooded area's [m]")
    box("plot", col="white")  
    withProgress(plotRGB(raster(osm, crs=CRS(projection(DEM))),add=T), 
                 message = '(Re-)Calculation in progress',
                 detail = 'Creating plot...')
    grid()#Add grid at tickmarks
    
    ## Plot flooded area after click and if the flooded.area is bigger then NULL
    input$goButton
    isolate({
      # Try looding remaining variables after click
      try(flooded.area <- flood())
      try(breach.area<- breach.area())
      
    if (!is.null(flooded.area)){
      # Create plot colors
      waterPallette <- colorRampPalette(brewer.pal(9, "Blues"))(20)
      
      # Plot flooded.area and breach area
      withProgress(plot(flooded.area, add=T, col=waterPallette), message = '(Re-)Calculation in progress',
                   detail = 'Plotting flooded area...')
      plot(breach.area, add=T, col='red', border='red')
      
      # Create and add a North arrow that takes the extent of the plot in account
      x.position<-extent(flooded.area)[1] + 0.1*(extent(flooded.area)[2]-extent(flooded.area)[1])
      y.position<-extent(flooded.area)[4] - 0.15*(extent(flooded.area)[4]-extent(flooded.area)[3])
      length <- (((extent(flooded.area)[4])-(extent(flooded.area)[3]))*0.035)
      north.arrow(xb=x.position, yb=y.position, len=length, lab="N")}
    })  
  })  
    
  ## Output histogram of the water depth distribution (only working in browser!)
  output$hist <- renderGvis({
    freq<-NULL
    try(freq<-frequency())
    if (!is.null(freq)){    
    gvisColumnChart(freq, options=list(
      title='Water Depth Distribution',
      axisTitlesPositon="out", 
      hAxis=paste("{title: 'Meter', titleTextStyle: {color: 'black'}, ticks:",paste("[",toString(unlist(frequency()[,1])),"]", sep=""),"}",sep=""), 
      vAxis="{title: 'Flooded Area [km2]', titleTextStyle: {color: 'black'}}",             
      legend="{position: 'none'}"))}
    else {
      return(NULL)
    }
  })
  
  ## Output text of the total flooded area
  output$total <- renderText({
    total.area.km2 <- sum(frequency()[,2])
    total <- paste("Total flooded area:", format(round(total.area.km2, 2), nsmall=2),"km2.")
    return(HTML(paste(total, "<font color='gray'>Tip: Press 'Plot!' to show the result in the map.</font>", sep="<br/><br/>")))
  })
  
  ############# Extra #############
  ## Outputtext that displays the files that are removed
  output$removed <- renderText({
    # Load variable
    input$remove
    
    # Search for files
    files <- paste(list.files(pattern='*alt.grd|*alt.gri|*alt.vrt', full.names=T),collapse=', ')
    
    # Remove files if present and show which files are removed
    if (files!=""){
      if (input$remove>0){
        file.remove(list.files(pattern='*alt.grd|*alt.gri|*alt.vrt', full.names=T))
        return(paste("Removed country DEM files:",files,"."))}}
  })
})