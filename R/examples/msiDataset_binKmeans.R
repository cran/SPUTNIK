## Load package
library("SPUTNIK")

## Create the msi.dataset-class object
sz <- c(5, 4)
x <- matrix(rnorm(sz[1] * sz[2]), sz[1], sz[2])
mz <- seq(100, 195, 5)
msiX <- msiDataset(x, mz, sz[1], sz[2])

## Binarize the reference image
refBin <- binKmeans(msiX)
