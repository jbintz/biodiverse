get_m <- function(t) {
  size <- floor(seq(1, t, length.out = min(t, 20)))
  tibble::tibble(m = 0:2 %>% purrr::map(~(c(size, size + t))))
}
