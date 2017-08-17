get_R <- function(df, type, x) {
  if (type == "incidence") {
    ncol(df)
  } else {
    sum(x)
  }
}
