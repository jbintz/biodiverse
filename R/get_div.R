get_div <- function(x, n) {
  tibble::tibble(q = factor(0:2)) %>%
    dplyr::group_by(q) %>%
    mutate(
      qD = purrr::pmap(list(x, n, q), get_qD)
    )
}
