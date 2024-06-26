% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_split.R
\name{splitPeaksFilter}
\alias{splitPeaksFilter}
\title{Test for the presence of split peaks.}
\usage{
splitPeaksFilter(
  msiData,
  mzTolerance = 5,
  sharedPixelsRatio = 0,
  sparseness = "scatter.ratio",
  threshold = 0.5,
  returnDetails = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{msiData}{\link{msi.dataset-class} object. See \link{msiDataset}.}

\item{mzTolerance}{numeric (default = 5). Maximum distance in PPM between the
m/z values of two peaks to consider them for merging. See 'Details' section.}

\item{sharedPixelsRatio}{numeric (default = 0). Maximum fraction of common pixels
where the signal of two peaks is different from zero to consider them for
merging. See 'Details' section.}

\item{sparseness}{string (default = \code{"scatter.ratio"}). Method used to estimate
the 'scatteredness' of the peak image. See 'Details' section.}

\item{threshold}{numeric (default = 0.5). Threshold for scatteredness measure
to consider peaks for merging. At least one of the merging peaks should have
a measure associated with presence of structure.}

\item{returnDetails}{logical (default = \code{TRUE}). Add details on merged peaks
in the results.}

\item{verbose}{logical (default = \code{TRUE}). Additional output text.}
}
\value{
\code{peak.filter} object. See \link{applyPeaksFilter}.
}
\description{
\link{splitPeaksFilter} returns a list of estimated split peak indices. Each
element of the list contains an array of the original peak indices that can
be merged. The name of the list element is the new m/z value associated with
the merged peaks.
}
\details{
\link{splitPeaksFilter} determines whether close peaks represent the
same signal. This estimation is based on multiple conditions:

 \enumerate{

  \item peaks m/z values should be closer than \code{mzTolerance} PPM

  \item at least one of the peak images should be structured, accordingly to
  the \code{sparseness} measure. The \code{threshold} determines whether the
  pixel images are structured or not. The possible measures are:

  \itemize{
    \item \code{"scatter.ratio"}: ratio between the number of non-zero pixels
    and the image size after binarization using Otsu's thresholding. A value close
    to 0 is associated with a more structured image, whereas a value close to
    1 is associated with a less structured image. A suggested parameter of
    \code{threshold = 0.5} represents the maximum value for this measure for
    a structured image. Minimum possible value is 1 / ( # non-zero pixels ).

    \item \code{"spatial.chaos"}: similar to the scatter ratio taking into
    account of the color histogram. A value close to 1 represents a structured
    image, whereas a value close to 0 represents a more scattered image.
    A suggested parameter of \code{threshold = 0.8} represents the minimum value
    for this measure for a structured image. Maximum possible value is
    1 - 1 / ( # histogram bins ). Here, we use the default number of bins
    equal to 30.

    \item \code{"gini.index"}: Gini index measures the image sparsity. A value
    close to 1 is associated with a sparse image whereas a value close to 0
    is associated with a more uniform image. A suggested value of \code{threshold = 0.9}
    represents the maximum value of this measure for a structured image.
  }

  \item the merged peaks image should be more structured than the single
  peak images, accordingly to the selected \code{sparseness}.
}
}
\examples{
## Load package
library("SPUTNIK")

## Mass spectrometry intensity matrix
X <- matrix(rnorm(200), 20, 40)
X[X < 0] <- 0

## Print original dimensions
print(dim(X))

## m/z vector
mzVector <- seq(600, 601, by = (601 - 600) / 39)

## Read the image size
imSize <- c(5, 4)

## Construct the ms.dataset object
msiX <- msiDataset(X, mzVector, imSize[1], imSize[2])

## Determine split peaks
sp.filter <- splitPeaksFilter(
  msiData = msiX, mzTolerance = 50,
  sharedPixelsRatio = 0,
  sparseness = "spatial.chaos", threshold = 0.5
)
}
\references{
Palmer, A., Phapale, P., Chernyavsky, I., Lavigne, R., Fay, D.,
Tarasov, A., ... & Becker, M. (2017). FDR-controlled metabolite annotation for
high-resolution imaging mass spectrometry. Nature methods, 14(1), 57.

Hurley, N., & Rickard, S. (2009). Comparing measures of sparsity.
IEEE Transactions on Information Theory, 55(10), 4723-4741.
}
\author{
Paolo Inglese \email{p.inglese14@imperial.ac.uk}
}
