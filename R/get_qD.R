get_qD <- function(x, n, q) {
  size <- floor(seq(1, n, length.out = min(n, 20)))
  if (q == 0) qD <- get_richness(x, n, size)
  if (q == 1) qD <- get_shannon(x, n, size)
  if (q == 2) qD <- get_simpson(x, n, size)
  qD
}

get_richness <- function(x, n, y) {
  f0 <- get_f0(x, n)
  f1 <- length(x[x == 1])
  S_obs <- length(x)
  q0_int <- y %>%
    purrr::map_dbl(~ S_obs - sum(choose(n - x, .) / choose(n, .)))
  q0_ext <- y %>%
    purrr::map_dbl(~ S_obs + f0 * (1 - (1 - f1 / (n * f0 + f1)) ^ .))
  c(q0_int, q0_ext)
}

get_shannon <- function(x, n, y) {
  U <- sum(x)
  H_inf <- get_H_inf(x, n)
  q1_int <- y %>%
    purrr::map_dbl(get_q1_int, x, n, U)
  q1_ext <- y %>%
    purrr::map_dbl(
      ~ exp(-n / (n + .) * sum(x / U * log(x / U)) + . / (n + .) * H_inf)
    )
  c(q1_int, q1_ext)
}

get_simpson <- function(x, n, y) {
  U <- sum(x)
  q2_int <- y %>%
    purrr::map_dbl(
      ~ . / (n / U + (. - 1) / U / (U - U / n) * sum(x * (x - 1)))
    )
  q2_ext <- y %>%
    purrr::map_dbl(
      ~ (n + .) / (n / U + (n + . - 1) / U / (U - U / n) * sum(x * (x - 1)))
    )
  c(q2_int, q2_ext)
}

get_H_inf <- function(x, n) {
  U <- sum(x)
  x_trunc <- x[x <= (n - 1)]
  f1 <- length(x[x == 1])
  f2 <- length(x[x == 2])
  if (f2 > 0) {
    A <- 2 * f2 / ((n - 1) * f1 + 2 * f2)
  } else if (f1 == 0) {
    A <- 1
  } else {
    A <- 2 / ((n - 1) * (f1 - 1) + 2)
  }
  temp_vec <- 1:(n - 1)
  temp_sum <- sum(1 / temp_vec * (1 - A) ^ temp_vec)
  log(U / n) + (n / U) *
    (
      sum(x_trunc / n * (digamma(n)-digamma(x_trunc))) +
      f1 / n * (1 - A) ^ (1 - n) * (-log(A) - temp_sum)
    )
}

get_q1_int <- function(m, x, n, U) {
  U_hat <- m * U / n  # this value equals .m for abundance data
  vec <- 1:max(x) %>%
    purrr::map_dbl(
      ~ (
          (1 / choose(n, m)) * sum(choose(x[x >= .], .) *
          choose(n - x[x >= .], (m - .))) * . / -U_hat * log(. / U_hat)
        )
    )
  exp(sum(vec))
}
