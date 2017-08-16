get_boot <- function(x, n, data_type) {
  if (data_type == "abundance") {
    rmultinom(200, n, x) %>%
      tibble::as_tibble()
  } else {
    replicate(200, x %>% purrr::map_dbl(~rbinom(1, n, prob = .))) %>%
      tibble::as_tibble()
  }
}
