library(shiny)
library(raster)

#Set max upload size
options(shiny.maxRequestSize = 30*1024^2)

#Define list of possible countries
alldata<- getData("ISO3")
allcodes<-alldata[,1]
allcountry<-alldata[,2]
names(allcodes)<-allcountry

# Define UI for application that draws a histogram
shinyUI(#All interactive input variables
  fluidPage(
    titlePanel("Flood Risk Area's"),
    sidebarLayout(
      sidebarPanel( 
        radioButtons('RB1', 'DEM options', c('Use country'=0, 'Upload region'=1), inline=T),
        selectInput("country", label="Select country", choices=allcodes),
        fileInput('DEM',label='Upload region (.tif)', accept=c('.tif')), #upload file
        sliderInput("water.height", label= "Water height",
                    min=0.1,max=20,value=0.1, step=0.1),
        
        h4(strong('Breach Settings')),
        numericInput("breach.width", label= "Width", 
                     min=0,max=1000,value=300),#set breach width        
        radioButtons('RB2', 'Number of breaches', c('single'=0, 'multiple'=1), inline=T),
        h4("Single breach"),
        numericInput("coord.x", label= "X coordinate", 
                     value=46015),#x.coordinate
        numericInput("coord.y", label= "Y coordinate", 
                     value=418028),#y.coordinate
        h4("Multiple breaches"),
        fileInput('coords',label='Upload coordinates', accept=c('.csv')),
        p('Upload a .csv format table with an x and y column'),
        actionButton("goButton","Go!")),
      mainPanel(
        plotOutput("plot", clickId="click", height="750px")
        ),
      fluid=T
    )
  )
)