est_f0 <- function(x, n) {
  x <- x[x > 0]
  f1 <- length(x[x == 1])
  f2 <- length(x[x == 2])
  if (f2 > 0) {
    ((n - 1) / n) * (f1 ^ 2 / (2 * f2))
  } else {
    ((n - 1) / n) * f1 * (f1 - 1) / 2
  }
}
