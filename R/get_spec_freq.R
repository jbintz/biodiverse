# @param df the data tibble for a single assemblage returned by `enframe_data`
# @param data_type is either "incidence" or "abundance"
# @return vector of species frequencies

get_spec_freq <- function(df, data_type) {
  if (data_type == "incidence") {
    df <- df %>% dplyr::mutate_all(dplyr::funs(. > 0))
  } else if (data_type == "incidence2") {
    df <- df %>% dplyr::transmute_all(dplyr::lead, n = 1L, default = 0)
  }
  df %>%
    dplyr::transmute(freq = rowSums(.)) %>%
    .$freq %>%
    .[. > 0]
}
