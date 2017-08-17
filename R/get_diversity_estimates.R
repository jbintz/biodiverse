get_diversity_estimates <- function(x, data_type = "abundance") {
  enframe_data(x) %>%
    dplyr::group_by(assemblage) %>%
    mutate(
      spec_freq = purrr::map2(data, data_type, get_spec_freq),
      freq_counts = purrr::map(spec_freq, get_freq_counts),
      U = purrr::map_dbl(spec_freq, sum),
      R = purrr::pmap_dbl(list(data, data_type, spec_freq), get_R),
      samp_cov = purrr::map2_dbl(spec_freq, R, get_samp_cov),
      f0 = purrr::map2_dbl(spec_freq, R, get_f0),
      rel_abun = purrr::pmap(list(spec_freq, n, samp_cov, f0), est_rel_abun),
      boot = purrr::pmap(list(rel_abun, n, data_type), get_boot),
      cov = purrr::map2(spec_freq, n, get_cov),
      cov_err = purrr::map2(boot, n, get_cov_err),
      div = purrr::map2(spec_freq, n, get_div),
      div_err = purrr::map2(boot, n, get_div_err)
    )
  #    dplyr::mutate(div_ests = purrr::map_df(summary_list, get_div_ests))
}


# things to remember:
# U = sum(freq_vec)
# size_vec = floor(seq(1, n, length.out = min(n, 20)))
