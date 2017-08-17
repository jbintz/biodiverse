get_n <- function(df, data_type, x) {
  if (data_type == "incidence") {
    ncol(df)
  } else {
    sum(x)
  }
}
