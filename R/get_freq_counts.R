get_freq_counts <- function(x) {
  table(x) %>%
    tibble::as_tibble() %>%
    dplyr::transmute(k = as.integer(.[[1]]), f_k = as.integer(.[[2]]))
}
