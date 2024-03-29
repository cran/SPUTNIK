% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binary_refimg.R
\name{refImageBinaryKmeans}
\alias{refImageBinaryKmeans}
\alias{refImageOtsu}
\title{Calculate the binary reference image using k-means clustering. K-Means is run
on the first `npcs` principal components to speed up the calculations.}
\usage{
refImageBinaryKmeans(
  dataset,
  npcs = 10,
  alignTo = "detected",
  invertAligned = FALSE
)
}
\arguments{
\item{dataset}{\link{msi.dataset-class} object. See \link{msiDataset}.}

\item{npcs}{int (default = 10). Number of principal components to calculate.}

\item{alignTo}{string (default = "detected"). Sample reference image to align
the estimate binary image. It is expected to correlate with the sample location.}

\item{invertAligned}{boolean (default = FALSE). If TRUE, the binary image is
inverted after being aligned to the sample reference (\code{alignTo}).}
}
\value{
\link{ms.image-class} object with binary intensities.
}
\description{
Calculate the binary reference image using k-means clustering. K-Means is run
on the first `npcs` principal components to speed up the calculations.
}
