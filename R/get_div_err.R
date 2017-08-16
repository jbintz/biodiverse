get_div_err <- function(df, n) {
  tibble::tibble(q = 0:2) %>%
    mutate(
      qD_err = 0:2 %>% purrr::map(get_qD_err, df, n)
    ) %>%
    tidyr::unnest() %>%
    dplyr::select(qD_err)
}
