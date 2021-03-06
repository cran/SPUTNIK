% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_funcs.R
\name{refAndROIimages}
\alias{refAndROIimages}
\title{Compute the reference image and the ROI mask.}
\usage{
refAndROIimages(
  msiData,
  refMethod = "sum",
  roiMethod = "otsu",
  mzQueryRef = numeric(),
  mzTolerance = Inf,
  useFullMZRef = TRUE,
  smoothRef = FALSE,
  smoothSigma = 2,
  invertRef = FALSE,
  numClusters = 4,
  sizeKernel = 5,
  numCores = 1,
  verbose = TRUE
)
}
\arguments{
\item{msiData}{\link[SPUTNIK]{msiDataset} object..}

\item{refMethod}{string (default = "sum"). Method used to calculate the
reference image. Valid values are:
\itemize{
  \item "sum": peak intensities sum
  \item "mean": average peak intensities (without zeros)
  \item "median": median peak intensities (without zeros)
  \item "pca": first principal component scores.
}}

\item{roiMethod}{string (default = "otsu"). Method used to extract the ROI
binary mask. Valid values are:
\itemize{
  \item "otsu": the reference image is binarized using Otsu's thresholding
  \item "kmeans": msiData is partitioned in 2 clusters using k-means
  \item "kmeans2": k-means is applied with a user-defined number of clusters
  (see Details)
  \item "supervised": supervised segmentation based on user-defined areas
  corresponding to off-sample and sample regions.
}}

\item{mzQueryRef}{numeric. Values of m/z used to calculate the reference image.
2 values are interpreted as interval, multiple or single values are searched
in the m/z vector. It overrides the param \code{useFullMZRef}.}

\item{mzTolerance}{numeric (default = Inf). Tolerance in PPM to match the
\code{mzQueryRef} values in the m/z vector.}

\item{useFullMZRef}{logical (default = TRUE). Whether all the peaks should be
used to calculate the reference image.}

\item{smoothRef}{logical (default = FALSE). Whether the reference image
should be smoothed before binarizing. Only valid for \code{roiMethod = "otsu"}.}

\item{smoothSigma}{numeric (default = 2). Standard deviation of Gaussian
kernel.}

\item{invertRef}{logical (default = FALSE). Whether the reference image
colors should be inverted. This can be necessary when the signal is more
intense outside the ROI.}

\item{numClusters}{numeric (default = 4). Only for 'kmeans2' method. Number
of clusters.}

\item{sizeKernel}{4-D numeric array or numeric (default = 5). Only for 'kmeans2'.
Each element of the 4-D array represents the size of the corners square kernels
used to determine the off-tissue clusters. The element order is clockwise:
top-left, top-right, bottom-left, bottom-right. If negative, the corresponding
corner is skipped. If only a single value is passed, the same kernel size is
used for the 4 corners.}

\item{numCores}{numeric (default = 1). Only for 'kmeans2' method. Number of
CPU cores for parallel k-means. It must be smaller than the number of
available cores.}

\item{verbose}{logical (default = TRUE). Additional output text.}
}
\description{
\code{refAndROIimages} returns the reference image, calculated using the
\code{refMethod}, and the ROI binary mask, calculated using \code{roiMethod}.
These images represent the basic measures for the filters in SPUTNIK.
}
\details{
Function to extract the reference image from a \code{\link{msi.dataset-class}}
object. Two references images are returned, a continuous-valued and a binary-valued.
Multiple methods can be used to extract both the continuous and the binary
reference images, which afterwards can be used as argument for the \code{\link{globalPeaksFilter}}
filter. When 'kmeans2' is applied, the ROI is obtained by merging the sample-related
clusters. The user can set a larger number of cluster than 2 (like in 'kmeans'), in
such a way a finer segmentation of the sample-related area can be generated.
Currently, the off-sample clusters are identified by looking at the most frequent
(statistical mode) labels in the corners of the image.
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
mzVector <- seq(600, 900, by = (900 - 600) / 39)

## Read the image size
imSize <- c(5, 4)

## Construct the ms.dataset object
msiX <- msiDataset(X, mzVector, imSize[1], imSize[2])

## Calculate the reference and ROI images from the ms.dataset-class object msiX.
## The reference is calculated as the first principal component scores scaled
## in [0, 1]; the binary ROI is calculated applying k-means on the entire dataset.
## Use only m/z values in the range of [700, 900]. The interval extremal values
## are matched within a tolerance of 50 ppm.

ref.roi <- refAndROIimages(
  msiData = msiX, refMethod = "sum",
  roiMethod = "otsu", useFullMZRef = TRUE
)

## Plot the reference and region of interest ROI
## plot(ref.roi$Reference)
## plot(ref.roi$ROI)
}
\seealso{
msiDataset, binOtsu, binKmeans
}
\author{
Paolo Inglese \email{p.inglese14@imperial.ac.uk}
}
