% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_funcs.R
\name{NMI}
\alias{NMI}
\title{Normalized mutual information (NMI).}
\usage{
NMI(x, y, numBins = 256)
}
\arguments{
\item{x}{numeric array. Image 1 color intensity array.}

\item{y}{numeric array. Image 2 (binary mask).}

\item{numBins}{numeric. Number of bins for discretizing the image colors.}
}
\value{
NMI value between 0 and 1.
}
\description{
\code{NMI} returns the normalized mutual information between two \code{ms.image}
objects. The normalized mutual information is calculated as the mutual information
divided by square-root of the product of the entropies. This function makes
use of the functions available in \code{infotheo} R package.
}
\references{
Meyer, P. E. (2009). Infotheo: information-theoretic measures.
R package. Version, 1(0).
}
\author{
Paolo Inglese \email{p.inglese14@imperial.ac.uk}
}
