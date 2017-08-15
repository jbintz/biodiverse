get_sum_list <- function(df, data_type) {
  if (data_type == "incidence") {
    df <- df %>% dplyr::mutate_all(dplyr::funs(. > 0))
    n <- ncol(df)
  }
  freq_vec <- df %>%
    dplyr::transmute(freq = rowSums(.)) %>%
    .$freq %>%
    .[. > 0]
  U <- sum(freq_vec)
  if (data_type == "abundance") {
    n <- U
  }
  size <- if (n <= 20) n else 20
  size_vec <- floor(seq(1, n, length.out = size))
  m <- c(size_vec, size_vec + n)
  f0_est <- est_f0(freq_vec, n)
  f1 <- length(freq_vec[freq_vec == 1])
  f2 <- length(freq_vec[freq_vec == 2])
  cov_est <- est_cov(n, U, f1, f2)
  # need bs_rel_abun_vec
  # need boot_df
  list(
    freq_vec = freq_vec,
    n = n,
    U = U,
    size = size,
    size_vec = size_vec,
    m = m
  )
}
