#' Creates an extra column for every column in the input data that has missing values. 
#' The newly created column is a factor column (TRUE/FALSE) and represents the existance of NAs or empty strings ("") in the examined column.
#' The newly created columns are named "na"+name_of_original_column
#'
#' @param data input data
#' @return data frame with the original and the newly created columns
#'   
#' @export
#' @examples
#' naFeatures(myData)

naFeatures<-function(data)
{
  counter<-0
  for(i in 1:ncol(data))
  {
    if(sum(is.na(data[i]))>0 || sum(data[i]=="")>0)
    {
      counter<-counter+1
      varname<-paste0("na", names(data[i]))
      data[[varname]] <- with(data, as.factor(paste0((is.na(data[i]) | data[i]==""))))
    }
  }
  return(data)
}