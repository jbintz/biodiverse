get_diversity_estimates <- function(x, data_type = "abundance") {
  enframe_data(x) %>%
    dplyr::group_by(assemblage) %>%
    dplyr::mutate(freq_vec = purrr::map2(data, data_type, get_freq_vec)) %>%
    dplyr::mutate(n = purrr::pmap_dbl(list(data,data_type,freq_vec), get_n)) %>%
    dplyr::mutate(cov_est = purrr::map2_dbl(freq_vec, n, est_cov)) %>%
    dplyr::mutate(f0_est = purrr::map2_dbl(freq_vec, n, est_f0)) %>%
    dplyr::mutate(rel_abun_est = purrr::pmap(
      list(freq_vec, n, cov_est, f0_est), est_rel_abun
    )) %>%
    dplyr::mutate(boot_df = purrr::pmap(
      list(rel_abun_est, n, data_type), get_boot_df
    ))
#    dplyr::mutate(div_ests = purrr::map_df(summary_list, get_div_ests))
}


# things to remember:
# U = sum(freq_vec)
# size_vec = floor(seq(1, n, length.out = min(n, 20)))
