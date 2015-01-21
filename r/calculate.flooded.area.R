calculate.flooded.area <- function(breach.area, breach.height, DEM){
  # Function to calculate the flooded area's
  # breach.area: The area of the breach
  # breach.height: The height of the bottom of the breach
  # DEM: The Digital Elevation Model of the area
  
  # extract values and select height of breach
  breach.mask <- mask(DEM, breach.area)
  breach.rast <- setValues(breach.mask, breach.height)#create empty raster
  breach.rast[is.na(breach.mask)] <- NA #set NA values for everything that is NA in the mask
  
  # combine DEM & breach
  DEMwithbreach <- merge(breach.rast, DEM)
  DEMwithbreach.filled <- fill.up(DEMwithbreach, waterheight)
  
  # Clump flooded areas
  clump.flood<-clump(DEMwithbreach.filled)
  clump.floodEMP <- setValues(raster(clump.flood), 1)#create empty raster
  clump.floodEMP[is.na(clump.flood)] <- NA #set NA values for everything that is NA in the clump
  clump.flood<-clump(clump.floodEMP) #clump flooded area's
  
  # Select clump connected to breach
  clump.intersect<-intersect(clump.flood, breach.area) #with this intersect all clumps within the breach area can be selected
  clump.connect<-unique(clump.intersect@data@values) #extract the unique codes of the clumps within the breach area
  flooded.area<-clump.connect[!is.na(clump.connect)] #remove NA values
  clump.flood[clump.flood!=flooded.area]<-NA #everything in the clumped flood areas that doesn't have the unique codes is NA
  
  # Set waterheight in flooded areas
  water.dept <- mask(DEMwithbreach.filled, clump.flood)
  
  return(water.dept)
}













