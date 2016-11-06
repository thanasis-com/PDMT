#' Returns a vector of the same size as the input vector.
#' Each value of the new vector is the difference between the current and previous value of the input vector.
#'
#' @param data input vector
#' @return vector of the same size as the input
#'   
#' @export
#' @examples
#' absDiff(myVector)

absDiff<-function(myVector)
{
  newVec<-c(myVector[1])
  for(i in 1:length(myVector))
  {
    if(i!=1)
    {
      newVec[i]<-myVector[i]-myVector[i-1]
    }
  }
 
  return(newVec)
}