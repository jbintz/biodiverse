#' nest_data
#'
#' nest_data returns a nested data frame with one row per assemblage
#'
#' @param x a list of assemblage frequency vectors
#'
#' @return a nested data frame with one row per assemblage
#'
#' @examples
#' # abundance data
#' l <- list(A = c(10,2,1,1), B=c(7,2,1))
#' nest_data(l)
#' # if assemblage names are omitted, they will be assigned
#' l <- list(c(10,2,1,1), c(7,2,1))
#' nest_data(l)
nest_data <- function(x){
  x %>%
    purrr::map(~(tibble::tibble(freq = .))) %>%
    dplyr::bind_rows(.id = "assemblage") %>%
    dplyr::mutate(assemblage = factor(assemblage)) %>%
    dplyr::group_by(assemblage) %>%
    tidyr::nest()
}
