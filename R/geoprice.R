geoprice <- function(x){
  geoprices <- NULL
  for(i in 1:ncol(x)){
    geoprices[i] <- exp(sum(log(x[,i]))/(nrow(x)))
  }
  return(geoprices)
}
