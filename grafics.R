# Plots


diag_caixes <- function(df, var) {
  ggplot(df, aes_string(x = "Mort", y = var, colour = "Estatines")) +
    geom_boxplot() +
    theme_bw()
}

diag_caixes_sev <- function(df, var) {
  ggplot(df, aes_string(x = "Severitat", y = var, colour = "Estatines")) +
    geom_boxplot() +
    theme_bw()
}

plot_corbes_surv <- function(df, grup, lims, conf_lev = 0.95) {
  z = qnorm((1 + conf_lev) / 2)
  
  df$n = NA
  for (lev in levels(df[, grup])) {
    df$n[df[, grup] == lev] = sum(df[, grup] == lev)
  }
  df$ymin = df$avg_pred - z * df$sd_pred / sqrt(df$n)
  df$ymax = df$avg_pred + z * df$sd_pred / sqrt(df$n)

  ggplot(df,
         aes_string(
           x = "temps",
           y = "avg_pred",
           colour = grup,
           fill = grup
         )) +
    geom_line(size = 1) +
    geom_ribbon(aes(
      ymin = ymin,
      ymax = ymax
    ),
    alpha = 0.3) +
    xlim(lims) +
    theme_bw()
}