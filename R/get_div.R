# get_div <- function(x, u, t) {
#   x <- x[x > 0]
#   size <- floor(seq(1, t, length.out = min(t, 20)))
#   tibble::tibble(q = 0:2) %>%
#     mutate(
#       qD = 0:2 %>% purrr::map(get_qD, x, u, t),
#       m = 0:2 %>% purrr::map(~(c(size, size + t)))
#     )
# }
get_div <- function(x, u, t) {
  x <- x[x > 0]
  tibble::tibble(q = 0:2) %>%
    mutate(qD = 0:2 %>% purrr::map(get_qD, x, u, t))
}
