fill.up <- function(DEM, waterheight){
  # function that fills up a digital elevation model according to the given waterheight
  outputraster<-calc(DEM, fun=function(x){
    x[x>waterheight]<-NA;
    x<-waterheight-x;
    return (x)
    })
  return(outputraster)
}