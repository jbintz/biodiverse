get_diversity_estimates <- function(x, data_type = "abundance") {
  enframe_data(x) %>%
    dplyr::group_by(assemblage) %>%
    mutate(
      freq = purrr::map2(data, data_type, get_freq),
      n = purrr::pmap_dbl(list(data, data_type, freq), get_n),
      samp_cov = purrr::map2_dbl(freq, n, get_samp_cov),
      f0 = purrr::map2_dbl(freq, n, get_f0),
      rel_abun = purrr::pmap(list(freq, n, samp_cov, f0), est_rel_abun),
      boot = purrr::pmap(list(rel_abun, n, data_type), get_boot),
      cov = purrr::map2(freq, n, get_cov),
      cov_err = purrr::map2(boot, n, get_cov_err),
      div = purrr::map2(freq, n, get_div),
      div_err = purrr::map2(boot, n, get_div_err)
    )
  #    dplyr::mutate(div_ests = purrr::map_df(summary_list, get_div_ests))
}


# things to remember:
# U = sum(freq_vec)
# size_vec = floor(seq(1, n, length.out = min(n, 20)))
