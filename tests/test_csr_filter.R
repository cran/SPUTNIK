# Test cmplete spatial randomness

library(testthat)
library(SPUTNIK)

test_that("CSR filter", {
  x <- bladderMALDIRompp2010(verbose = TRUE)
  mz <- attr(x, "mass")
  shape <- attr(x, "size")

  msX <- msiDataset(values = x, mz = mz, rsize = shape[1], csize = shape[2])
  msX <- normIntensity(msX, "PQN")
  msX <- varTransform(msX, "log2")
  
  ticImg <- totalIonCountMSI(msX)
  roi <- refImageBinaryOtsu(ticImg)
  
  # Reduce M/Z list for quick test
  mz.mask <- msX@mz >= 800
  msX@matrix <- msX@matrix[, mz.mask]
  msX@mz <- msX@mz[mz.mask]

  cat("Clark-Evans test...")
  csrCE <- CSRPeaksFilter(msX,
    method = "ClarkEvans",
    adjMethod = "bonferroni",
    returnQvalues = TRUE,
    plotCovariate = FALSE,
    verbose = TRUE
  )

  cat("Passing covariate as argument for Kolmogorov-Smirnov test...")
  set.seed(123)
  csrKS <- CSRPeaksFilter(msX,
    method = "KS",
    covariateImage = roi, # Calculate the covariate
    adjMethod = "bonferroni",
    returnQvalues = TRUE,
    plotCovariate = FALSE,
    verbose = TRUE
  )

  expect_is(csrCE, "list")
  expect_equal(attr(csrCE, "names"), c("p.value", "q.value"))
  expect_is(csrKS, "list")
  expect_equal(attr(csrKS, "names"), c("p.value", "q.value"))

  csrFiltCE <- createPeaksFilter(which(csrCE$q.value < 0.05))
  csrFiltKS <- createPeaksFilter(which(csrKS$q.value < 0.05))

  expect_equal(length(csrFiltCE$sel.peaks), 83)
  expect_equal(length(csrFiltKS$sel.peaks), 133)
})
