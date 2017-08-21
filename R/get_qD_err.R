get_qD_err <- function(q, df, u, t) {
  size <- floor(seq(1, t, length.out = min(t, 20)))
  if (q == 0) qD_err <- get_richness_err(df, t, size)
  if (q == 1) qD_err <- get_shannon_err(df, u, t, size)
  if (q == 2) qD_err <- get_simpson_err(df, u, t, size)
  qD_err
}

get_richness_err <- function(df, t, size) {
  df %>%
    purrr::map_df(get_richness, t, size) %>%
    mutate(em = c(size, size + t)) %>%
    tidyr::gather(repnum, est, -em) %>%
    tidyr::spread(em, est) %>%
    dplyr::select(-repnum) %>%
    purrr::map_dbl(sd, na.rm = TRUE)
}

get_shannon_err <- function(df, u, t, size) {
  df %>%
    purrr::map_df(get_shannon, u, t, size) %>% # map to map_df
    mutate(em = c(size, size + t)) %>%
    tidyr::gather(repnum, est, -em) %>%
    tidyr::spread(em, est) %>%
    dplyr::select(-repnum) %>%
    purrr::map_dbl(sd, na.rm = TRUE)
}

get_simpson_err <- function(df, u, t, size) {
  df %>%
    purrr::map_df(get_simpson, u, t, size) %>%
    mutate(em = c(size, size + t)) %>%
    tidyr::gather(repnum, est, -em) %>%
    tidyr::spread(em, est) %>%
    dplyr::select(-repnum) %>%
    purrr::map_dbl(sd, na.rm = TRUE)
}
