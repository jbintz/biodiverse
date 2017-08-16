get_n <- function(a, b, c) {
  if (b == "incidence") {
    ncol(a)
  } else {
    sum(c)
  }
}
