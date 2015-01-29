fill.up <- function(DEM, waterheight){
  ## function that fills up a digital elevation model according to the given waterheight
  
  fill.rast<-calc(DEM, fun=function(x){
    x[x>waterheight]<-NA; # Cells with a height higher than the waterheight are set to NA
    x<-waterheight-x; # Set flood depth for remaining cells
    return (x)
  })
  
  return(fill.rast)
}