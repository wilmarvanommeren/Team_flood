library(shiny)

# Define UI for application that draws a histogram
shinyUI(#All interactive input variables
  fluidPage(
    titlePanel("Flood Risk Area's"),
    sidebarLayout(
      sidebarPanel( 
        fileInput('DEM',label='Input DEM', accept=c('.tif')), #upload file
        p('Example DEM-files are available', a('here', href='http://www.arcgis.com/home/webmap/viewer.html?webmap=ac6b6ecd42424e33bd0e6fa09499c563', target="_blank")),# add URL
        sliderInput("water.height", label= "Water height",
                    min=0.1,max=20,value=0.1, step=0.1),#set water height
        sliderInput("time.hour", label= "Time in hours",
                    min=1,max=24,value=1),#set time
        h4('Breach Settings:'),
        numericInput("breach.width", label= "Width", 
                     min=0,max=1000,value=200),#set breach width        
        numericInput("coord.x", label= "X coordinate", 
                     value=46015.33),#x.coordinate
        numericInput("coord.y", label= "Y coordinate", 
                     value=418028),#y.coordinate
        actionButton("goButton","Go!")),
      mainPanel(
        plotOutput("plot", clickId="click")
        ),
      fluid=T
    )
  )
)