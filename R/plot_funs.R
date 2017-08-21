plot_size_based_RE <- function(df) {
  df %>%
    ggplot(aes(m, qD, col = q,
               ymin = qD - 1.96 * qD_err,
               ymax = qD + 1.96 * qD_err)
    ) +
    geom_line(
      data = df %>% group_by(assemblage) %>% filter(m <= max(m) / 2)
    ) +
    geom_line(
      data = df %>% group_by(assemblage) %>% filter(m > max(m) / 2),
      linetype = "dashed") +
    geom_ribbon(
      alpha=0.2,
      aes(fill = q, col = NULL)
    ) +
    geom_point(
      data = df %>% group_by(assemblage) %>% filter(m == max(m) / 2)
    ) +
    labs(x = "Number of Individuals", y = "Diversity Estimate") +
    facet_wrap(~assemblage, scales = "free")
}

plot_size_based_RE <- function(df) {
  df %>%
    ggplot(aes(m, qD, col = assemblage,
               ymin = qD - 1.96 * qD_err,
               ymax = qD + 1.96 * qD_err)
    ) +
    geom_line(
      data = df %>% group_by(assemblage) %>% filter(m <= max(m) / 2)
    ) +
    geom_line(
      data = df %>% group_by(assemblage) %>% filter(m > max(m) / 2),
      linetype = "dashed") +
    geom_ribbon(
      alpha=0.2,
      aes(fill = assemblage, col = NULL)
    ) +
    geom_point(
      data = df %>% group_by(assemblage) %>% filter(m == max(m) / 2)
    ) +
    labs(x = "Number of Individuals", y = "Diversity Estimate") +
    facet_wrap(~q, scales = "fixed")
}

plot_cov_based_RE <- function(df) {
  df %>%
    ggplot(aes(cov, qD, col = q,
               ymin = qD - 1.96 * qD_err,
               ymax = qD + 1.96 * qD_err)
    ) +
    geom_line(
      data = df %>% group_by(assemblage) %>% filter(m <= max(m) / 2)
    ) +
    geom_line(
      data = df %>% group_by(assemblage) %>% filter(m > max(m) / 2),
      linetype = "dashed") +
    geom_ribbon(
      alpha=0.2,
      aes(fill = q, col = NULL)
    ) +
    geom_point(
      data = df %>% group_by(assemblage) %>% filter(m == max(m) / 2)
    ) +
    labs(x = "Percent Coverage", y = "Diversity Estimate") +
    facet_wrap(~assemblage, scales = "fixed")
}

plot_cov_based_RE <- function(df) {
  df %>%
    ggplot(aes(cov, qD, col = assemblage,
               ymin = qD - 1.96 * qD_err,
               ymax = qD + 1.96 * qD_err)
    ) +
    geom_line(
      data = df %>% group_by(assemblage) %>% filter(m <= max(m) / 2)
    ) +
    geom_line(
      data = df %>% group_by(assemblage) %>% filter(m > max(m) / 2),
      linetype = "dashed") +
    geom_ribbon(
      alpha=0.2,
      aes(fill = assemblage, col = NULL)
    ) +
    geom_point(
      data = df %>% group_by(assemblage) %>% filter(m == max(m) / 2)
    ) +
    labs(x = "Percent Coverage", y = "Diversity Estimate") +
    facet_wrap(~q, scales = "fixed")
}
