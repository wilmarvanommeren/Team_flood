library(shiny)

# Define UI for application that draws a histogram
shinyUI(#All interactive input variables
  fluidPage(
    titlePanel("Flood Risk Area's"),
    sidebarLayout(
      sidebarPanel( 
        radioButtons('RB1', 'DEM options', c('Use country'=0, 'Upload region'=1), inline=T),
        fileInput('DEM',label='Upload New (.tif)', accept=c('.tif')), #upload file
        sliderInput("water.height", label= "Water height",
                    min=0.1,max=20,value=0.1, step=0.1),#set water height
        sliderInput("time.hour", label= "Time in hours",
                    min=1,max=24,value=1),#set time
        h4(strong('Breach Settings')),
        numericInput("breach.width", label= "Width", 
                     min=0,max=1000,value=200),#set breach width        
        radioButtons('RB2', 'Number of breaches', c('single'=0, 'multiple'=1), inline=T),
        h4("Single breach"),
        numericInput("coord.x", label= "X coordinate", 
                     value=46015.33),#x.coordinate
        numericInput("coord.y", label= "Y coordinate", 
                     value=418028),#y.coordinate
        h4("Multiple breaches"),
        fileInput('coords',label='Upload coordinates', accept=c('.csv')),
        p('Upload a .csv format table with an x and y column'),
        actionButton("goButton","Go!")),
      mainPanel(
        plotOutput("plot", clickId="click", height="600px")
        ),
      fluid=T
    )
  )
)