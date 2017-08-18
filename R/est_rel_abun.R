est_rel_abun <- function(x, u, t, c, f0) {
  f0 <- ceiling(f0)
  lamb_est <- u / t * (1 - c) / sum((x / t) * (1 - x / t) ^ t)
  p1 <- (x / t) * (1 - lamb_est * (1 - x / t) ^ t)
  p2 <- rep(u / t * (1 - c) / f0, f0)
  c(p1, p2)
}
