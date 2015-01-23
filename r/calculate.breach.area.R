calculate.breach.area <- function(breach.point, breach.width){
  # Function to calculate the breach area(s)
  # breach.point: The center location of the breach
  # breach.width: The width of the breach
  
  # Create spatial breach point
  breach.point<-data.frame(breach.point)
  coordinates(breach.point) <- ~x+y
  
  # Create spatial breach point buffer
  breach.area<-buffer(breach.point, width = breach.width)
  
  return(breach.area)
}