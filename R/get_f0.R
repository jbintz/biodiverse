get_f0 <- function(x, t) {
  x <- x[x > 0]
  f1 <- length(x[x == 1])
  f2 <- length(x[x == 2])
  if (f2 > 0) {
    ((t - 1) / t) * (f1 ^ 2 / (2 * f2))
  } else {
    ((t - 1) / t) * f1 * (f1 - 1) / 2
  }
}
