% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smoothVector.R
\name{smoothVector}
\alias{smoothVector}
\title{Returns a vector of the same length as the input vector, with its values "smoothed", to be closer to the vector mean. 
The parameter lambda defines the magnitude of smoothing.
If lambda is set to 1, every element of the returned vector will be equal to the vector mean.
The magnitude of smoothing is disproportional to the value of lambda.}
\usage{
smoothVector(myVector, lambda = 2)
}
\arguments{
\item{vector}{input vector, lamdba}
}
\value{
vector of the same length as the input
}
\description{
Returns a vector of the same length as the input vector, with its values "smoothed", to be closer to the vector mean. 
The parameter lambda defines the magnitude of smoothing.
If lambda is set to 1, every element of the returned vector will be equal to the vector mean.
The magnitude of smoothing is disproportional to the value of lambda.
}
\examples{
smoothVector(myVector, lambda=3)
smoothVector(myVector)
}

