% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct_funcs.R
\name{msImage}
\alias{msImage}
\title{Constructor for \link{ms.image-class} objects.}
\usage{
msImage(values, name = character(), scale = TRUE)
}
\arguments{
\item{values}{numeric matrix representing the pixels intensities. Rows and
columns represent the geometrical shape of the image.}

\item{name}{image name.}

\item{scale}{logical (default = TRUE). Whether the intensities should be
scaled in [0, 1].}
}
\value{
\link{ms.image-class} object.
}
\description{
Constructor for \link{ms.image-class} objects.
}
\examples{
## Load package
library("SPUTNIK")

## MS image
imShape <- c(40, 50)
matIm <- matrix(rnorm(200), imShape[1], imShape[2])
im <- msImage(values = matIm, name = "random", scale = TRUE)
}
\author{
Paolo Inglese \email{p.inglese14@imperial.ac.uk}
}
