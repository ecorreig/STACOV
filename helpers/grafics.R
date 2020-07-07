# Plots


stacked_bar_plot <- function(df, save=F) {
  # This is not a proper function, only works for the case in use, but I wanted to get
  # it out of the main script
  # TODO: generalize this
  
  ta = questionr::wtd.table(df$Estatines_ret, df$Mort, weights = df$weights)
  tdf1 = as.data.frame(ta)
  tdf2 = as.data.frame(tt)
  tdf = rbind.data.frame(tdf1, tdf2[tdf2$Var1 == "Sí", ])
  
  group_name = "Statins"
  event_name = "Death"
  names(tdf) = c(group_name, event_name, "Número")
  g1 = "No"
  g2 = "Withdrawn"
  g3 = "Continued"
  g4 = "Yes"
  tdf[, group_name] = factor(tdf[, group_name], 
                             levels = c("No", "Retirades", "No retirades", "Sí"),
                             labels = c(g1, g2, g3, g4))
  tdf[, event_name] = factor(tdf[, event_name],
                             levels = c("No", "Sí"), 
                             labels = c("No", "Yes"))
  
  tdf$Percentatge = 0
  
  noest = sum(tdf$`Número`[tdf[, group_name] == g1])
  tdf$Percentatge[tdf[, group_name] == g1] = tdf$`Número`[tdf[, group_name] == g1] / noest * 100
  ret = sum(tdf$`Número`[tdf[, group_name] == g2])
  tdf$Percentatge[tdf[, group_name] == g2] = tdf$`Número`[tdf[, group_name] == g2] / ret * 100
  noret = sum(tdf$`Número`[tdf[, group_name] == g3])
  tdf$Percentatge[tdf[, group_name] == g3] = tdf$`Número`[tdf[, group_name] == g3] / noret * 100
  est = sum(tdf$`Número`[tdf[, group_name] == g4])
  tdf$Percentatge[tdf[, group_name] == g4] = tdf$`Número`[tdf[, group_name] == g4] / est * 100
  
  tdf[, event_name] = relevel(tdf[, event_name], "Yes")
  
  pl <- ggplot(data = tdf, aes(x = Statins, y = Percentatge, fill = Death)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(
      aes(label = paste0(round(Percentatge, 2), "%")),
      position = position_stack(vjust = 0.5),
      color = "black",
      size = 5
    ) +
    ylab("") +
    ggtitle("Relation between statins and death") +
    #scale_fill_brewer(palette = "Paired") +
    scale_color_nejm() + 
    scale_fill_nejm() +
    theme_tufte() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "top")
  
  plot_saver(pl, "stacked_bar_plot", save = save)
}

plot_corbes_surv <- function(df, grup, lims, conf_lev = 0.95, title="", cuminc=F, save=F, name=NULL) {
  z = qnorm((1 + conf_lev) / 2)
  
  df$n = NA
  for (lev in levels(df[, grup])) {
    df$n[df[, grup] == lev] = sum(df[, grup] == lev)
  }
  df$ymin = df$avg_pred - z * df$sd_pred / sqrt(df$n)
  df$ymax = df$avg_pred + z * df$sd_pred / sqrt(df$n)
  
  yname = if (cuminc) "Predicted cumulative incidence" else "Predicted survival probability"

  pl <- ggplot(df,
         aes_string(
           x = "temps",
           y = "avg_pred",
           colour = grup,
           fill = grup
         )) +
    geom_line(aes_string(linetype = grup), size = 1) +
    geom_ribbon(aes_string(
      ymin = "ymin",
      ymax = "ymax",
      linetype = grup
    ),
    alpha = 0.3) +
    xlim(lims) +
    xlab("Days") + 
    ylab(yname) +
    ggtitle(title) +
    scale_color_nejm() + 
    scale_fill_nejm() +
    theme_tufte() +
    theme(axis.line = element_line(), 
          legend.position = "top")
    
  
  plot_saver(pl, name, save = save)
}

plot_saver <- function(pl, name, save) {
  plot_dir = "plots"
  type = "png"
  name = paste(name, type, sep = ".")
  
  path_ = file.path(plot_dir, name)
  if (save) {
    pl + ggsave(path_)
  }
  else pl
}