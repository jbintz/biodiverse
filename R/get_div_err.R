get_div_err <- function(df, u, t) {
  tibble::tibble(qD_err = 0:2 %>% purrr::map(get_qD_err, df, u, t))
}
