calculate.breach.area <- function(breach.point, breach.width){
  ## Function to calculate the breach area(s)
  # breach.point: The center location of the breach
  # breach.width: The width of the breach
  
  # Create spatial breach point
  breach.point<-data.frame(breach.point)
  coordinates(breach.point) <- ~x+y
  
  # Create spatial breach point buffer
  bufferlist = list() #Create empty list
  if (length(unlist(breach.width))>1){ #if multiple breaches go in the forloop
    for (i in (1:length(breach.width[,1]))){
      # Lists a buffer per breach, dependent on the breach.width
      buffer<-buffer(breach.point[i], width = breach.width[i,]) 
      bufferlist[[i]]<-buffer}
    
    # Create names for all buffers in the list (which will be used as ID's)
    set.seed(1)
    IDs <- sample(c(LETTERS, letters), length(unlist(breach.width)))
    names(bufferlist) <- IDs
    
    # Create a single SpatialPolygons object from a list of SpatialPolygons
    polygon <- SpatialPolygons(lapply(1:length(bufferlist), function(i) {
      # Select slot 'polygons' from individual list items
      Pol <- slot(bufferlist[[i]], "polygons")[[1]]
      # Create IDs from the created names
      slot(Pol, "ID") <- names(bufferlist)[i]
      # The function SpatialPolygons will be applied on the listitems with new IDs
      Pol}))
  } 
  else{ #If single breach create single buffer
    polygon<-buffer(breach.point, width = breach.width)}
  
  return(polygon)
}