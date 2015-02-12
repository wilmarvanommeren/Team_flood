# Authors: Rob Maas & Wilmar van Ommeren
# Date: January 2015

# Load & install packages if required
if (!require(raster)){install.packages("raster")}
if (!require(shiny)){install.packages("shiny")}

# Set max upload size
options(shiny.maxRequestSize = 30*1024^2)

# Define list of possible countries
alldata<- getData("ISO3")
allcodes<-alldata[,1]
empty<-c("...")
allcountry<-alldata[,2]
names(allcodes)<-allcountry
allcodes<-append(empty, allcodes)

# Define UI for application that calculates and plots the flooded area
shinyUI(
  fluidPage(
    # Title and tab-title in browser
    titlePanel(title="Simulate the flooded area's after a single or multiple breach(es)",windowTitle="Simulate Flood Risk Area's"),
    # Sidebar
    sidebarLayout(
      sidebarPanel( 
        # Every panel in the sidebar (h4 = heading 4, hr = horizontal line, p = paragraph)
        h4("Global Parameters"),
        # Upload a DEM or download a DEM
        radioButtons('RB1', 'Choose method', c('Use country'=0, 'Upload region'=1), inline=T),
        selectInput("country", label="Select country", choices=allcodes),
        fileInput('DEM',label='Upload region (.tif|.grd)', accept=c('.tif','.grd')), 
        # Input values for waterlevel and breach width
        numericInput("water.height", label= "Water level [m]",min=0, value=0),  
        # A single breach or multiple breaches
        radioButtons('RB2', 'Number of breaches', c('Single'=0, 'Multiple'=1), inline=T),
        hr(style="border-color:gray;"),
        # Enter coordinates for single breach
        h4("Single breach"),
        numericInput("breach.width", label= "Breach width [m]", min=0,max=1000,value=0),
        numericInput("coord.x", label= "X coordinate [m]", value=46015),
        numericInput("coord.y", label= "Y coordinate [m]", value=418028),
        p("Convert WGS84 to RD coordinates", a("here", href='http://www.regiolab-delft.nl/?q=node/36', target="_blank")),
        hr(style="border-color:gray;;"),
        # Upload coordinates for multiple breaches
        h4("Multiple breaches"),
        fileInput('coords',label='Upload coordinates (.csv|.xls|.xlsx)', accept=c('.csv','.xls','.xlsx')),
        # Press go to plot the output (calculations have already started)
        actionButton("goButton","Plot!"),
        br(),
        br(),
        br(),
        hr(style="border-color:gray;"),
        # Remove downloaded DEM's
        p("Removes downloaded country DEM files"),
        actionButton("remove", "Remove")),     
      mainPanel(
        # Plot & textoutput
        plotOutput("plot", height='750px'),
        hr(),
        htmlOutput("hist"),
        br(),
        htmlOutput("total"),
        br(),
        textOutput("removed"),
        br(),# Help
        hr(style="border-color:black;"),
        # Project background information
        p(strong("Authors:"), "Wilmar van Ommeren & Rob Maas"),
        p(strong("Instance:"), "Wageningen University, Wageningen"),
        p(strong("Course:"), "Geo Scripting", a("GRS-51806", href="https://ssc.wur.nl/Studiegids/Vak/GRS-51806", target="_blank")),
        p(strong("Lecturers:"), "dr.ir. J.P. verbesselt, ing. W.T. ten Haaf, ir. A.R. Bergsma, ir. H.J. Stuiver, dr.ir. S. de Bruin, dr.ir. R.J.A. van Lammeren & L.P. Dutrieux"),
        img(src='logo.png'),
        br(),
        hr(style="border-color:black;"),
        # Project help
        h2("Help"),
        h4("1.Instructions"),
        h5('1.1 Digital Elevation Map [DEM] options'),
        p("If option 'Use country' is used, the rasterfile of that country will be downloaded to your harddisk. The minimum breachsize is shown in the tip section."),
        p("If option 'Upload region' is used, a rasterfile from your harddisk will be used in the calculation. Supported extensions are .tif and .grd. Digital elevation maps of the Netherlands are accesible"),
        p("All input maps are converted to the Dutch",a('RD coordinate system.', href='http://en.wikipedia.org/wiki/Geography_of_the_Netherlands', target="_blank")),
        p('DEMs of the Netherlands are freely downloadable', a('here.', href='http://www.arcgis.com/home/item.html?id=917565db27be4283989da1d64cf437a4', target="_blank")),
        h5("1.2 Waterheight"),
        p("The sea level is the reference level. The minimum value of the breach is the minimum height value that is placed within the breach area. This minimum value is extracted from the DEM."),
        h5("1.3 Breach settings"),
        p("If option 'single' is selected the coordinates of the single breach should be entered in the fields below the text 'Single breach. Coordinate unit is meters, because this is the unit of the RD coordinate system."),
        p("If option 'multiple' is selected a file can be uploaded that contains an x, y and breach width column (example image below). The supported extensione are .csv, .xls and .xlsx."),
        img(src='CSVTableExample.png'),
        h5("1.4 Start calculation"),
        p("Press 'Go!' to start the calculation. This may take some time!"),
        br(),
        h4("2.Unexpected outcomes"),
        h5("2.1 DEM"),
        p("A DEM will be plotted if there is no flooded area present or if value's are not filled in correctly (e.g: 0 breach width, wrong coordinate units, wrong file extension)."),
        h5("2.2 Error"),
        p("An error will be returned if there is not enough space available in the memory of the computer for the computation (",span("Error: cannot allocate vector of size x MB", style= "color:red"),"). This can be avoided by uploading or selecting a smaller file. A better pc would also help."),
        p("If 'Multiple' breaches is selected and the file format or extension is wrong, another error will be returned (",span("Error: object 'x' not found or Error: invalid multibyte string 1", style= "color:red"),"). Remember that the extension should be .csv, .xls or .xlsx and the file should contain three columns with x and y coordinates and the breach width. The names of the first columns should be respectively 'x' and 'y'."),
        h5("2.3 Plot"),
        p("A White screen means the calculation is in progress, so there is no result yet. If the plot appears transparant this probably means you moved or resized the window of the browser. Somehow the calculation will start again."),
        p("If 'multiple breaches' is selected stripes can occur in the plot, because not all flooded area's are connected. We are currently looking for a fix for this problem."),
        br(),
        h4("3.Examples"),
        h5("2.1 Single breach"),
        p("*Step 1: Select as country 'Netherlands"),
        p("*Step 2: Set waterlevel to 1"),
        p("*Step 3: Set number of breaches to 'Single'"),
        p("*Step 4: Set breach width to 1000"),
        p("*Step 5: x and y coordinates respectively to 46015 and 418028"),
        p("*Step 6: Press 'Plot!'"),
        p("*Step 7: After plotting press 'Remove' to remove the DEM of the Netherlands from your pc"),
        h5("2.2 Multiple breaches"),
        p("*Step 1: Select as country 'Netherlands"),
        p("*Step 2: Set waterlevel to 1"),
        p("*Step 3: Set number of breaches to 'Multiple'"),
        p("*Step 4: Upload a file with the same data as the example image to the app"),
        p("*Step 5: Press 'Plot!'"),
        p("*Step 6: After plotting press 'Remove' to remove the DEM of the Netherlands from your pc")   
      )
    )
  )
)