get_cov_err <- function(df, n) { # this function not good
  size_vec <- floor(seq(1, n, length.out = min(n, 20)))
  df %>%
    purrr::map_df(get_cov, n) %>%
    mutate(em = c(size_vec, size_vec + n)) %>%
    tidyr::gather(repnum, est, -em) %>%
    tidyr::spread(em, est) %>%
    dplyr::select(-repnum) %>%
    purrr::map_dbl(sd, na.rm = TRUE)
}
