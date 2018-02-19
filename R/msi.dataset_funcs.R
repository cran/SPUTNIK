## .normIntensity
#' @importFrom stats median
.normIntensity <- function(x, method = "median")
{
  accept.method <- c("TIC", "median", "PQN")
  if (!any(method %in% accept.method))
  {
    stop("Valid methods are: ", paste0(accept.method, collapse = ", "), ".")
  }
  x[x == 0] <- NA
  x <- switch(method,

              "TIC" = {
                for (i in 1:nrow(x))
                {
                  x[i, ] <- x[i, ] / sum(x[i, ], na.rm = T)
                }
                x
              },

              "median" = {
                for (i in 1:nrow(x))
                {
                  x[i, ] <- x[i, ] / median(x[i, ], na.rm = T)
                }
                x
              },

              "PQN" = {

                for (i in 1:nrow(x))
                {
                  x[i, ] <- x[i, ] / sum(x[i, ], na.rm = T)
                }

                x[x == 0] <- NA

                ref.spectrum <- apply(x, 2, median, na.rm = T)
                quotients <- matrix(NA, nrow(x), ncol(x))
                for (i in 1:nrow(x))
                {
                  quotients[i, ] <- x[i, ] / ref.spectrum
                }

                quotients[quotients == 0] <- NA

                sc.factor <- apply(quotients, 2, median, na.rm = T)

                for (i in 1:nrow(x))
                {
                  x[i, ] <- x[i, ] / sc.factor
                }

                x[is.na(x)] <- 0

                x
              })
  x[is.na(x)] <- 0
  x
}

## .varTransf
.varTransf <- function(x, method = "log")
{
  accept.method <- c("log", "sqrt")
  if (!any(method %in% accept.method))
  {
    stop("Balid methods are:", paste0(accept.method, collapse = ", "), ".")
  }
  x <- switch(method,
              "log" = log(x + 1),
              "sqrt" = sqrt(x))
}
