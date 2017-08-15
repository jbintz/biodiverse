get_diversity_estimates <- function(x, data_type = "abundance") {
  enframe_data(x) %>%
    dplyr::group_by(assemblage) %>%
    dplyr::mutate(summary_list = purrr::map2(data, data_type, get_sum_list)) # %>%
#    dplyr::mutate(div_ests = purrr::map_df(summary_list, get_div_ests))
}
