#' Returns a vector of the same length as the input vector, with its values "smoothed", to be closer to the vector mean. 
#' The parameter lambda defines the magnitude of smoothing.
#' If lambda is set to 1, every element of the returned vector will be equal to the vector mean.
#' The magnitude of smoothing is disproportional to the value of lambda.
#'
#'
#' @param vector input vector, lamdba
#' @return vector of the same length as the input
#'   
#' @export
#' @examples
#' smoothVector(myVector, lambda=3)
#' smoothVector(myVector)

smoothVector<-function(myVector, lambda=2)
{
  vectorMean<-mean(myVector)
  
  myVector<-sapply(myVector, function(x) x+((vectorMean-x)/lambda)) 
  
  return(myVector)
}