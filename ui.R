# Authors: Rob Maas & Wilmar van Ommeren
# Date: January 2015

# Install packages if required
if (!require(shiny)){install.packages("shiny")}
if (!require(raster)){install.packages("raster")}

# Load libraries
library(shiny)
library(raster)

#Set max upload size
options(shiny.maxRequestSize = 30*1024^2)

#Define list of possible countries
alldata<- getData("ISO3")
allcodes<-alldata[,1]
empty<-c("...")
allcountry<-alldata[,2]
names(allcodes)<-allcountry
allcodes<-append(empty, allcodes)

## Define UI for application that draws a histogram
shinyUI(# All interactive input variables
  fluidPage(
    titlePanel(title="The flooded area's after a single or multiple breach(es)",windowTitle="Flood Risk Area's"),
    sidebarLayout(
      sidebarPanel( 
        radioButtons('RB1', 'DEM options', c('Use country'=0, 'Upload region'=1), inline=T),
        selectInput("country", label="Select country", choices=allcodes),
        fileInput('DEM',label='Upload region', accept=c('.tif','.grd')), 
        numericInput("water.height", label= "Water level",
                    min=0, value=0),  
        numericInput("breach.width", label= "Breach width", 
                     min=0,max=1000,value=0),        
        radioButtons('RB2', 'Number of breaches', c('Single'=0, 'Multiple'=1), inline=T),
        h4("Single breach"),
        numericInput("coord.x", label= "X coordinate", 
                     value=46015),
        numericInput("coord.y", label= "Y coordinate", 
                     value=418028),
        h4("Multiple breaches"),
        fileInput('coords',label='Upload coordinates', accept=c('.csv')),
        actionButton("goButton","Go!")),
      mainPanel(# Plotoutput
        plotOutput("plot", clickId="click", height="750px"),
        br(),# Help
        br(),
        br(),
        br(),
        br(),
        br(),
        h2("Help"),
        h4("1.Instructions"),
        h5('1.1 Digital Elevation Map [DEM] options'),
        p("If option 'Use country' is used, the rasterfile of that country will be downloaded to your harddisk."),
        p("If option 'Upload region' is used, a rasterfile from your harddisk will be used in the calculation. Supported extensions are .tif and .grd. Digital elevation maps of the Netherlands are accesible"),
        p('DEMs of the Netherlands are downloadable', a('here.', href='http://www.arcgis.com/home/webmap/viewer.html?webmap=ac6b6ecd42424e33bd0e6fa09499c563', target="_blank")),
        h5("1.2 Waterheight"),
        p("The waterlevel should be above 0. This is because the default value for the groundlevel of the breach is 0. The sea level is the reference level."),
        h5("1.3 Breach settings"),
        p("If the coordinates are not known fill in 0 values for all possible options. A plot of the DEM will be returned from wich coordinates can be extracted."),
        p("If option 'single' is selected the coordinates of the single breach should be entered in the fields below the text 'Single breach."),
        p("If option 'multiple' is selected a file can be uploaded that only contains an x and y column. The supported extension is .csv."),
        h5("1.4 Start calculation"),
        p("Press 'Go!' to start the calculation. This may take some time!"),
        br(),
        h4("2.Unexpected outcome"),
        h5("2.1 DEM"),
        p("A DEM will be plotted if there is no flooded area present or if value's are not filled in correctly (e.g: 0 breach width, wrong coordinate units, wrong file extension"),
        h5("2.2 Error"),
        p("An error will be returned if there is not enough space available in the memory of the computer for the computation. This can be avoided by uploading or selecting a smaller file."),
        h5("2.3 Plot"),
        p("A White screen means the calculation is in progress, so there is no result yet. If the plot appears transparant this probably means you moved or resized the window of the browser. Somehow the calculation will start again.")
        ),
      fluid=T
    )
  )
)