get_qD_err <- function(q, df, n) {
  size <- floor(seq(1, n, length.out = min(n, 20)))
  if (q == 0) qD_err <- get_richness_err(df, n, size)
  if (q == 1) qD_err <- get_shannon_err(df, n, size)
  if (q == 2) qD_err <- get_simpson_err(df, n, size)
  qD_err
}

get_richness_err <- function(df, n, size) {
  df %>%
    purrr::map_df(get_richness, n, size) %>%
    mutate(em = c(size, size + n)) %>%
    tidyr::gather(repnum, est, -em) %>%
    tidyr::spread(em, est) %>%
    dplyr::select(-repnum) %>%
    purrr::map_dbl(sd, na.rm = TRUE)
}

get_shannon_err <- function(df, n, size) {
  df %>%
    purrr::map_df(get_shannon, n, size) %>% # map to map_df
    mutate(em = c(size, size + n)) %>%
    tidyr::gather(repnum, est, -em) %>%
    tidyr::spread(em, est) %>%
    dplyr::select(-repnum) %>%
    purrr::map_dbl(sd, na.rm = TRUE)
}

get_simpson_err <- function(df, n, size) {
  df %>%
    purrr::map_df(get_simpson, n, size) %>%
    mutate(em = c(size, size + n)) %>%
    tidyr::gather(repnum, est, -em) %>%
    tidyr::spread(em, est) %>%
    dplyr::select(-repnum) %>%
    purrr::map_dbl(sd, na.rm = TRUE)
}
