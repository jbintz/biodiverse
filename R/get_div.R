get_div <- function(x, n) {
  size <- floor(seq(1, n, length.out = min(n, 20)))
  tibble::tibble(q = 0:2) %>%
    mutate(
      qD = 0:2 %>% purrr::map(get_qD, x, n),
      m = 0:2 %>% purrr::map(~(c(size, size + n)))
    ) %>%
    tidyr::unnest()
}
