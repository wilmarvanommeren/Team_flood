merge.breach.DEM <- function(breach.area,DEM){
  ## Function to merge the DEM and breach.area, the valuefield is the lowest value of the DEM within the area
  # Breach.area: The area of the breach
  # DEM: The Digital Elevation Model of the area  
  
  ## Extract minimum value per polygon in breach.area and rasterize the polygons
  min.breach <- c()
  for (i in (1:length(breach.area))){
    sps= breach.area[i]
    
    # Extract the minimal value for each unique polygon
    breach.height = min(unlist(extract(DEM, sps)),na.rm=T)
    min.breach[[i]] = breach.height
    
    # Rasterize according to the unique breach.height and merge to DEM
    poly.rast <- rasterize(sps, DEM, field=min.breach[i])
    DEM<- merge(poly.rast, DEM)
  }
  return (DEM)
}