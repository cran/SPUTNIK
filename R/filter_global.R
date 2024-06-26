#' Reference similarity based peak selection.
#'
#' \code{globalPeaksFilter} returns a list of peaks selected by their similarity
#' with a reference image.
#'
#' @param msiData \link{msi.dataset-class} object. See \link{msiDataset}.
#' @param referenceImage \link{ms.image-class} object. Reference image used
#' to calculate the similarity values.
#' @param method method used to calculate the similariry between the peak
#' intensities and the reference image. Accepted values are:
#' \itemize{
#'    \item \code{pearson}: Pearson's correlation
#'    \item \code{spearman}: Spearman's correlation
#'    \item \code{ssim}: structural similarity index measure
#'    \item \code{nmi}: normalized mutual information.
#' }
#' @param threshold numeric (default = 0, default = 0.001 (SSIM)). The threshold
#' applied to the similarity values between the peaks images and the reference
#' image. The default value of 0 guarantees that only the ions with a positive
#' similarity with the reference image (typically representing the spatial
#' distribution of the signal source) are retrieved. For consistency, the NMI are
#' scaled in [-1, 1] to match the same range of correlations.
#' @param cores integer (default = 1). Number of cores for parallel computing.
#' @param verbose logical (default = \code{TRUE}). Additional output text.
#'
#' @return \code{peak.filter} object. See \link{applyPeaksFilter}.
#'
#' @details A filter based on the similarity between the peak signals and a reference
#' signal. The reference signal, passed as an \code{\link{ms.image-class}} object.
#' Both continuous and binary references can be passed. The filter then calculates the similarity
#' between the peaks signal and the reference image and select those with a similarity
#' larger than \code{threshold}. Multiple measures are available, correlation,
#' structural similarity index measure (SSIM), and normalized mutual information (NMI).
#' Since correlation can assume values in [-1, 1], also NMI are scaled in [-1, 1].
#'
#' @author Paolo Inglese \email{p.inglese14@imperial.ac.uk}
#'
#' @references Wang, Z., Bovik, A. C., Sheikh, H. R., & Simoncelli, E. P. (2004).
#' Image quality assessment: from error visibility to structural similarity.
#' IEEE transactions on image processing, 13(4), 600-612.
#' @references Meyer, P. E. (2009). Infotheo: information-theoretic measures.
#' R package. Version, 1(0).
#' 
#' @import doSNOW foreach
#' @importFrom utils setTxtProgressBar txtProgressBar
#' 
#' @example R/examples/filter_global.R
#'
#' @seealso \code{\link{countPixelsFilter}} \code{\link{applyPeaksFilter-msi.dataset-method}}
#' 
#' @export
#'
globalPeaksFilter <- function(msiData,
                              referenceImage,
                              method = "pearson",
                              threshold = NULL,
                              cores = 1,
                              verbose = TRUE) {
  .stopIfNotValidMSIDataset(msiData)
  .stopIfNotValidMSImage(referenceImage)
  .stopIfNotValidGlobalMethod(method)
  
  if (.isBinary(referenceImage) && method == "pearson") {
    warning("For binary reference images, it is suggested to use the other available methods.\n")
  }

  if (is.null(threshold)) {
    # Default threshold for SSIM is slightly higher
    if (method == "ssim") {
      threshold <- 0.001
    } else {
      threshold <- 0
    }
  }
  
  if (threshold < -1 || threshold > 1) {
    stop("threshold must be in [-1, 1].")
  }

  # Calculate the similarity between the ion images and the reference image
  if (verbose) {
    cat("Calculating the similarity values...\n")
  }

  # Use NMI only if the reference image is binary
  if (method == "nmi" && !.isBinary(referenceImage)) {
    stop("globalPeaksFilter: 'nmi' can be only used with binary reference images.")
  }
  
  method.func <- switch(method,
                        "pearson" = function(x, y) { cor(x, y, method = "pearson") },
                        "spearman" = function(x, y) { cor(x, y, method = "spearman") },
                        "ssim" = function(x, y) { SSIM(x, y) },
                        "nmi" = function(x, y) { NMI(x, y) })

  niter <- ncol(msiData@matrix)
  pb <- txtProgressBar(max = niter, style = 3, width = 80)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  ref.values <- c(referenceImage@values)
  
  if (cores > 1) {

    cl <- makeCluster(cores)
    doSNOW::registerDoSNOW(cl)
    
    i <- NULL
    
    r <- foreach::foreach(i = 1:niter, .combine = c, .options.snow = opts) %dopar% {
      method.func(msiData@matrix[, i], ref.values)
    }
    
    stopCluster(cl)

  } else {
    r <- apply(msiData@matrix, 2, function(z) method.func(z, ref.values))
  }

  close(pb)
  
  if (verbose) {
    cat("Similarity measure quantiles (after removing NAs):\n")
    print(quantile(r, na.rm = TRUE))
  }

  if (verbose) {
    cat(paste0("Selecting peaks with similarity larger than ", threshold, "...\n"))
  }

  names(r) <- msiData@mz
  out <- list(sim.values = r, sel.peaks = which(r > threshold))
  attr(out, "peak.filter") <- TRUE
  attr(out, "filter") <- "globalPeaks"

  return(out)
}
