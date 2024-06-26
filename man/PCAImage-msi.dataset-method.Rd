% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msi.dataset_methods.R
\name{PCAImage,msi.dataset-method}
\alias{PCAImage,msi.dataset-method}
\alias{PCAImage}
\title{Generates an RGB msImage representing the first 3 principal components. This
image can be used to qualitatively evaluate the spatial heterogeneity of the
sample.}
\usage{
\S4method{PCAImage}{msi.dataset}(object, alignToSample = TRUE, seed = NULL)
}
\arguments{
\item{object}{\link{msi.dataset-class} object.}

\item{alignToSample}{boolean (default = TRUE). If TRUE, the principal component
scores are aligned to the pixel mean intensity.}

\item{seed}{set the random seed (default = \code{NULL}).}
}
\value{
RGB raster representing the first 3 principal components
(see \link{msImage}).
}
\description{
Generates an RGB msImage representing the first 3 principal components. This
image can be used to qualitatively evaluate the spatial heterogeneity of the
sample.
}
