create.openstreetmap<-function(flooded.area){
  ### INSERTING OPEN STREET MAP ###
  # Extract the corners of the extent
  topleft <- cbind(extent(flooded.area)[1], extent(flooded.area)[4])   # top left coordinate of extent
  botright <- cbind(extent(flooded.area)[2], extent(flooded.area)[3])   # bottom right coordinate of extent
  
  # Make the extent-corners spatial and give it the same projection as the flooded area.
  extentcorners <- SpatialPoints(rbind(topleft, botright))
  proj4string(extentcorners) <- crs(flooded.area)
  
  # reproject extent to geographic coordinates to wgs84 for OpenStreetMap
  extent.wgs84<- spTransform(extentcorners, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  # retrieve basemap
  osm <- openmap (c(bbox(extent.wgs84)[2,2], bbox(extent.wgs84)[1,1]), c(bbox(extent.wgs84)[2,1], bbox(extent.wgs84)[1,2]))
  
  # reproject basemap back to flooded area projection
  osm.trans<-openproj(osm, projection= CRS(projection(flooded.area))) 
  return(osm.trans)
}
