get_freq_vec <- function(df, data_type) {
  if (data_type == "incidence") {
    df <- df %>% dplyr::mutate_all(dplyr::funs(. > 0))
  }
  df %>%
    dplyr::transmute(freq = rowSums(.)) %>%
    .$freq %>%
    .[. > 0]
}
