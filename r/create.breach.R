create.breach <- function(breacharea){
  calc(breacharea, fun=function(x){
    x<-minValue(breacharea);
    return (x)
    })
}