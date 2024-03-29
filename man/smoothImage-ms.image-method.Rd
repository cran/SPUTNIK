% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ms.image_methods.R
\name{smoothImage,ms.image-method}
\alias{smoothImage,ms.image-method}
\alias{smoothImage}
\title{Apply Gaussian smoothing to an MS image.}
\usage{
\S4method{smoothImage}{ms.image}(object, sigma = 2)
}
\arguments{
\item{object}{\link{ms.image-class} object. See \link{msImage}.}

\item{sigma}{numeric (default = 2). Standard deviation of the smoothing
Gaussian kernel.}
}
\value{
\link{ms.image-class} smoothed msImage.
}
\description{
Apply Gaussian smoothing to an MS image.
}
\examples{
## Load package
library("SPUTNIK")

## Create ms.image-class object
msIm <- msImage(values = matrix(rnorm(200), 40, 50), name = "test", scale = TRUE)

## Smooth the image colors
msImSmoothed <- smoothImage(msIm, sigma = 5)
}
