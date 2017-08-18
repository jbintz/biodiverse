get_boot <- function(x, t, data_type) {
  if (data_type == "abundance") {
    rmultinom(200, t, x) %>%
      tibble::as_tibble()
  } else {
    replicate(200, x %>% purrr::map_dbl(~rbinom(1, t, prob = .))) %>%
      tibble::as_tibble()
  }
}
