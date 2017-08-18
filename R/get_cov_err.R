get_cov_err <- function(df, u, t) { # this function not good
  size_vec <- floor(seq(1, t, length.out = min(t, 20)))
  df %>%
    purrr::map_df(get_cov, u, t) %>%
    mutate(em = c(size_vec, size_vec + t)) %>%
    tidyr::gather(repnum, est, -em) %>%
    tidyr::spread(em, est) %>%
    dplyr::select(-repnum) %>%
    purrr::map_dbl(sd, na.rm = TRUE)
}
