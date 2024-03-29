## Load package
library("SPUTNIK")

## Create ms.image-class object
msIm <- msImage(values = matrix(rnorm(200), 40, 50), name = "test", scale = TRUE)

## Generate binary image
msImBin <- refImageBinaryOtsu(msIm)

## Apply the morphological closing
msImClosed <- closeImage(msImBin, kern.size = 3)
