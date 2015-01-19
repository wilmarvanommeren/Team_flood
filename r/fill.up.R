fill.up <- function(DEM, waterheight){
  outputraster<-calc(DEM, fun=function(x){
    x[x>waterheight]<-NA;
    x<-waterheight-x;
    return (x)
    })
  return(outputraster)
}
