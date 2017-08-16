est_cov <- function(.x, .n) {
  f1 <- length(.x[.x == 1])
  f2 <- length(.x[.x == 2])
  U <- sum(.x)
  if (f2 > 0) {
    1 - f1 / U * (.n - 1) * f1 / ((.n - 1) * f1 + 2 * f2)
  } else {
    1 - f1 / U * (.n - 1) * (f1 - 1) / ((.n - 1) * (f1 - 1) + 2)
  }
}
