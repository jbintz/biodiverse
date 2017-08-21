get_diversity_estimates <- function(x, data_type = "abundance") {
  enframe_data(x) %>%
    dplyr::group_by(assemblage) %>%
    mutate(
      spec_freq = purrr::map2(data, data_type, get_spec_freq),
      `T` = purrr::pmap_dbl(list(data, data_type, spec_freq), get_T),
      U = purrr::map_dbl(spec_freq, sum),
      freq_counts = purrr::map(spec_freq, get_freq_counts),
      samp_cov = purrr::pmap_dbl(list(spec_freq, U, `T`), get_samp_cov),
      f0 = purrr::map2_dbl(spec_freq, `T`, get_f0),
      rel_abun = purrr::pmap(list(spec_freq, U, `T`, samp_cov, f0), est_rel_abun),
      boot = purrr::pmap(list(rel_abun, `T`, data_type), get_boot),
      m = purrr::map(`T`, get_m),
      cov = purrr::pmap(list(spec_freq, U, `T`), get_cov),
      #cov_err = purrr::pmap(list(boot, U, `T`), get_cov_err),
      div = purrr::pmap(list(spec_freq, U, `T`), get_div),
      div_err = purrr::pmap(list(boot, U, `T`), get_div_err)
    ) %>%
    dplyr::select(assemblage, m, cov, div, div_err) %>%
    tidyr::unnest() %>%
    tidyr::unnest() %>%
    dplyr::ungroup() %>%
    mutate(q = factor(q))
}
