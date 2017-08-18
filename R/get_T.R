get_T <- function(df, data_type, x) {
  if (data_type == "incidence") {
    ncol(df)
  } else if (data_type == "incidence2") {
    df %>%
      dplyr::transmute(freq = rowSums(.)) %>%
      .$freq %>%
      .[1]
  } else {
    sum(x)
  }
}
