#' Enframe biodiversity data.
#'
#' \code{enframe_data} returns a data frame with one row per assemblage and a
#' list column of frequency vectors
#'
#' @param x a list of assemblage data frames
#'
#' @return a data frame with one row per assemblage
#'
#' @examples
#' l <- list(
#'   A = tibble::tibble(A1 = c(10,2,1,1)),
#'   B = tibble::tibble(B1 = c(7,2,1))
#' )
#' enframe_data(l)
#' # each assemblage data frame must be named
#' l <- list(
#'   A = tibble::tibble(A1 = c(10,2,1,1)),
#'   tibble::tibble(B1 = c(7,2,1))
#' )
#' enframe_data(l)
enframe_data <- function(x) {
  if (any(names(x) == "") | is.null(names(x))) {
    stop("Each assemblage data frame must be named.", call. = FALSE)
  }
  x %>%
    tibble::enframe(
      name = "assemblage",
      value = "data"
    ) %>%
    dplyr::mutate(assemblage = factor(assemblage))
}
