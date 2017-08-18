get_samp_cov <- function(x, u, t) {
  x <- x[x > 0]
  f1 <- length(x[x == 1])
  f2 <- length(x[x == 2])
  if (f2 > 0) {
    1 - f1 / u * (t - 1) * f1 / ((t - 1) * f1 + 2 * f2)
  } else {
    1 - f1 / u * (t - 1) * (f1 - 1) / ((t - 1) * (f1 - 1) + 2)
  }
}
