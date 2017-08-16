get_cov <- function(x, n) {
  x <- x[x > 0]
  f1 <- length(x[x == 1])
  f2 <- length(x[x == 2])
  U <- sum(x)
  size <- floor(seq(1, n, length.out = min(n, 20)))
  C_int <- head(size,-1) %>%
    purrr::map_dbl(~ 1 - sum(x / U * choose(n - x, .) / choose(n - 1, .)))
  C_ext <- c(0, size) %>%
    purrr::map_dbl(
      ~ 1 - f1 / U * ((n - 1) * f1 / ((n - 1) * f1 + 2 * f2)) ^ (. + 1)
    )
  c(C_int, C_ext)
}
