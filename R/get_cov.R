get_cov <- function(x, u, t) {
  x <- x[x > 0]
  f1 <- length(x[x == 1])
  f2 <- length(x[x == 2])
  size <- floor(seq(1, t, length.out = min(t, 20)))
  C_int <- head(size,-1) %>%
    purrr::map_dbl(~ 1 - sum(x / u * choose(t - x, .) / choose(t - 1, .)))
  C_ext <- c(0, size) %>%
    purrr::map_dbl(
      ~ 1 - f1 / u * ((t - 1) * f1 / ((t - 1) * f1 + 2 * f2)) ^ (. + 1)
    )
  tibble::tibble(cov = 0:2 %>% purrr::map(~(c(C_int, C_ext))))
}
