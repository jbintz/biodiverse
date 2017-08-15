est_cov <- function(n, U, f1, f2) {
  if (f2 > 0) {
    1 - f1 / U * (n - 1) * f1 / ((n - 1) * f1 + 2 * f2)
  } else {
    1 - f1 / U * (n - 1) * (f1 - 1) / ((n - 1) * (f1 - 1) + 2)
  }
}
