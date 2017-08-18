get_qD <- function(q, x, u, t) {
  x <- x[x > 0]
  size <- floor(seq(1, t, length.out = min(t, 20)))
  if (q == 0) qD <- get_richness(x, t, size)
  if (q == 1) qD <- get_shannon(x, u, t, size)
  if (q == 2) qD <- get_simpson(x, u, t, size)
  qD
}

get_richness <- function(x, t, y) {
  x <- x[x > 0]
  f0 <- get_f0(x, t)
  f1 <- length(x[x == 1])
  S_obs <- length(x)
  q0_int <- y %>%
    purrr::map_dbl(~ S_obs - sum(choose(t - x, .) / choose(t, .)))
  q0_ext <- y %>%
    purrr::map_dbl(~ S_obs + f0 * (1 - (1 - f1 / (t * f0 + f1)) ^ .))
  c(q0_int, q0_ext)
}

get_shannon <- function(x, u, t, y) {
  x <- x[x > 0]
  H_inf <- get_H_inf(x, u, t)
  q1_int <- y %>%
    purrr::map_dbl(get_q1_int, x, u, t)
  q1_ext <- y %>%
    purrr::map_dbl(
      ~ exp(-t / (t + .) * sum(x / u * log(x / u)) + . / (t + .) * H_inf)
    )
  c(q1_int, q1_ext)
}

get_H_inf <- function(x, u, t) {
  x_trunc <- x[x <= (t - 1)]
  f1 <- length(x[x == 1])
  f2 <- length(x[x == 2])
  if (f2 > 0) {
    A <- 2 * f2 / ((t - 1) * f1 + 2 * f2)
  } else if (f1 == 0) {
    A <- 1
  } else {
    A <- 2 / ((t - 1) * (f1 - 1) + 2)
  }
  temp_vec <- 1:(t - 1)
  temp_sum <- sum(1 / temp_vec * (1 - A) ^ temp_vec)
  log(u / t) + (t / u) *
    (
      sum(x_trunc / t * (digamma(t)-digamma(x_trunc))) +
        f1 / t * (1 - A) ^ (1 - t) * (-log(A) - temp_sum)
    )
}

get_q1_int <- function(m, x, u, t) {
  U_hat <- m * u / t  # this value equals m for abundance data
  vec <- 1:max(x) %>%
    purrr::map_dbl(
      ~ (
        (1 / choose(t, m)) * sum(choose(x[x >= .], .) *
                                   choose(t - x[x >= .], (m - .))) * . / -U_hat * log(. / U_hat)
      )
    )
  exp(sum(vec))
}

get_simpson <- function(x, u, t, y) {
  x <- x[x > 0]
  q2_int <- y %>%
    purrr::map_dbl(
      ~ . / (t / u + (. - 1) / u / (u - u / t) * sum(x * (x - 1)))
    )
  q2_ext <- y %>%
    purrr::map_dbl(
      ~ (t + .) / (t / u + (t + . - 1) / u / (u - u / t) * sum(x * (x - 1)))
    )
  c(q2_int, q2_ext)
}
