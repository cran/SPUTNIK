% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_csr.R
\name{CSRPeaksFilter}
\alias{CSRPeaksFilter}
\title{Performs the peak selection based on complete spatial randomness test.}
\usage{
CSRPeaksFilter(
  msiData,
  method = "ClarkEvans",
  covariateImage = NULL,
  covMethod = "sum",
  mzQueryCov = numeric(),
  mzTolerance = numeric(),
  useFullMZCov = TRUE,
  smoothCov = FALSE,
  smoothCovSigma = 2,
  invertCov = FALSE,
  adjMethod = "bonferroni",
  returnQvalues = TRUE,
  plotCovariate = FALSE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{msiData}{\link{msi.dataset-class} object. See \link{msiDataset}.}

\item{method}{string (default = \code{"ClarkEvans"}). CSR statistical test
applied to the peaks signal. Accepted values are:
\itemize{
   \item "ClarkEvans": performs a test based on the Clark and Evans aggregation
   R index. This test evaluates the compares of the nearest-neighbors distances
   to the case of purely random pattern.
   \item "KS": performs a test of goodness-of-fit between the signal pixels
   associated point process pattern and a spatial covariate using the
   Kolmogorov-Smirnov test. The covariate is defined by the reference image.
}}

\item{covariateImage}{\link{ms.image-class} object. An image used as covariate
(necessary for Kolmogorov-Smirnov test). If NULL, the covariate image is
calculated using the method defined by `covMethod`.}

\item{covMethod}{string (default = \code{"sum"}). Method used to calculate the
reference image. Read only when \code{method = "KS"}. Possible values
are described in \code{'refAndROIimages'}.}

\item{mzQueryCov}{numeric. Values of m/z used to calculate the reference image.
2 values are interpreted as interval, multiple or single values are searched
in the m/z vector. It should be left unset when using \code{useFullMZCov = TRUE}.
Read only when \code{method = "KS"}.}

\item{mzTolerance}{numeric. Tolerance in PPM to match the \code{mzQueryCov}
values in the m/z vector. It should be left unset when using
\code{useFullMZCov = TRUE}.Read only when \code{method = "KS"}.}

\item{useFullMZCov}{logical (default = \code{TRUE}). Whether all the peaks should be
used to calculate the covariate image. Read only when \code{method = "KS"}.}

\item{smoothCov}{logical (default = \code{FALSE}). Whether the covariate image
should be smoothed using a Gaussian kernel. Read only when \code{method = "KS"}.}

\item{smoothCovSigma}{numeric (default = 2). Standard deviation of the smoothing
Gaussian kernel. Read only when \code{method = "KS"}.}

\item{invertCov}{logical (default = \code{FALSE}). Whether the covariate image
colors should be inverted.]}

\item{adjMethod}{string (default = \code{"bonferroni"}). Multiple testing correction
method. Possible values coincide with those of the \code{stats::p.adjust} function.}

\item{returnQvalues}{logical (default = \code{TRUE}). Whether the computed
q-values should be returned together with the p-values.}

\item{plotCovariate}{logical (default = \code{FALSE}). Whether the covariate image
should be visualized. Read only when \code{method = "KS"}.}

\item{verbose}{logical (default = \code{TRUE}). Additional output texts are
generated.}

\item{...}{additional parameters compatible with the \code{statspat.core} functions.
See \link[spatstat.core]{cdf.test} for "KS" and \link[spatstat.core]{clarkevans.test}.
for "ClarkEvans"}
}
\description{
\code{CSRPeaksFilter} returns the significance for the null hypothesis that the
spatial distribution of the peak intensities follow a random pattern. A
significant p-value (q-values can be returned after applying multiple testing
correction) allows to reject the hypothesis that the spatial distribution of
a peak signal is random. The tests are performed using the functions available
in the \code{statspat} R package.
}
\examples{
## Load package
library("SPUTNIK")

## Mass spectrometry intensity matrix
X <- matrix(rnorm(16000), 400, 40)
X[X < 0] <- 0

## Print original dimensions
print(dim(X))

## m/z vector
mzVector <- seq(600, 900, by = (900 - 600) / 39)

## Read the image size
imSize <- c(20, 20)

## Construct the ms.dataset object
msiX <- msiDataset(X, mzVector, imSize[1], imSize[2])

## Calculate the p-values using the Clark Evans test, then apply Benjamini-
## Hochberg correction.
csr <- CSRPeaksFilter(
  msiData = msiX, method = "ClarkEvans",
  calculateCovariate = FALSE, adjMethod = "BH"
)

## Print selected peaks
print(csr$q.value)

## Create a new filter selecting corrected p-values < 0.001
selIdx <- which(csr$q.value < 0.001)
csrFilter <- createPeaksFilter(selIdx)
}
\references{
Baddeley, A., & Turner, R. (2005). Spatstat: an R package for
analyzing spatial point patterns. Journal of statistical software, 12(6), 1-42.

Clark, P.J. and Evans, F.C. (1954) Distance to nearest neighbour
as a measure of spatial relationships in populations. Ecology 35, 445–453.

Berman, M. (1986) Testing for spatial association between a point
process and another stochastic process. Applied Statistics 35, 54–62.
}
\author{
Paolo Inglese \email{p.inglese14@imperial.ac.uk}
}
