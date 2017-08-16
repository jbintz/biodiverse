get_div <- function(x, n) {
  tibble::tibble(q = 0:2) %>%
    mutate(
      qD = 0:2 %>% purrr::map(get_qD, x, n)
    ) %>%
    tidyr::unnest()
}
