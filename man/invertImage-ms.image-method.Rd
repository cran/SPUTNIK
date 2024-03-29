% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ms.image_methods.R
\name{invertImage,ms.image-method}
\alias{invertImage,ms.image-method}
\alias{invertImage}
\title{Invert the colors of an MS image.}
\usage{
\S4method{invertImage}{ms.image}(object)
}
\arguments{
\item{object}{\link{ms.image-class} object. See \link{msImage}.}
}
\value{
\link{ms.image-class} object after inverting colors.
}
\description{
Invert the colors of an MS image.
}
\examples{
## Load package
library("SPUTNIK")

## Create ms.image-class object
msIm <- msImage(values = matrix(rnorm(200), 40, 50), name = "test", scale = TRUE)

## Invert the colors
msImInverted <- invertImage(msIm)
}
