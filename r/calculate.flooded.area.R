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
  
  # Clump flooded areas select clumps connected to breach
  clump.flood<-clump(DEMwithbreach.filled)
  clump.value<-extract (clump.flood, breach.area) #extract clumps inside breaches
  clump.connect<-unique(unlist(clump.value)) #extract values of those clumps
  flooded.clump<-clump.connect[!is.na(clump.connect)] #remove NA values 
  clump.flood[clump.flood!=flooded.clump]<-NA #everything in the clumped flood areas that doesn't have the unique codes is NA
  
  # Set waterheight in flooded areas
  water.dept <- mask(DEMwithbreach.filled, clump.flood)
  
  return(water.dept)
}













