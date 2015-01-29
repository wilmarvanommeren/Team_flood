create.polygon <- function(inputraster){
  ## Create Spatial Polygons from the extent of an inputraster
  
  # Create coordinates
  Poly.coord <- c(extent(inputraster)[1],extent(inputraster)[2],extent(inputraster)[3],extent(inputraster)[4])
  Poly.mat<-matrix(Poly.coord, ncol=2, dimnames=list(c('1','2'),c('x','y')))
  Poly.df<-data.frame(Poly.mat)
  coordinates(Poly.df) <- ~x+y
  
  # Create polygons and add projection
  Poly<-Polygon(Poly.df)
  Polys<-Polygons(list(Poly),1)
  SPS<-SpatialPolygons(list(Polys),proj4string=CRS(projection(inputraster)))
  return(SPS)
}