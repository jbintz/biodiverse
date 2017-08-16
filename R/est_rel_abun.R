est_rel_abun <- function(x, n, c, f0) {
  U <- sum(x)
  f0 <- ceiling(f0)
  lamb_est <- U / n * (1 - c) / sum((x / n) * (1 - x / n) ^ n)
  p1 <- (x / n) * (1 - lamb_est * (1 - x / n) ^ n)
  p2 <- rep(U / n * (1 - c) / f0, f0)
  c(p1, p2)
}
