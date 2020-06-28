# Aquestes funcions preparen, calculen i dibuixen les cobres ROC, 
# aixi com la funció on es veu el millor llindar
# i també el gràfic que dibuixa els falços positius i negatius

require(ggplot2)

prepare_roc <- function(y_test, fit_test){
  # Funció: prepara les dades per les properes funcions
  # Arguments: el valor real en y_test, en format factor i fit_test, que són les probabilitats.
  # Retorn: dataframe amb les dues variables ben posades per les properes funcions.
  
  preroc = cbind(y_test,fit_test)
  preroc=as.data.frame(preroc)
  preroc[,1]=preroc[,1]-1
  preroc[,1]=as.factor(preroc[,1])
  colnames(preroc)=c('survived', 'pred')
  
  return(preroc)
}


calculate_roc <- function(df, cost_of_fp=1, cost_of_fn=1, n=100) {
  
  # Funció: Calcula les corbe ROC
  # Arguments: el retorn de la funció prepare_roc, i, si es vol, valors del cost dels
  #            falsos positius i negatius (per defecte = 1)
  # Retorn: un dataframe per poder dibuixar les corbes en les properes funcions
  
  tpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$survived == 1) / sum(df$survived == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$survived == 0) / sum(df$survived == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(df$pred >= threshold & df$survived == 0) * cost_of_fp + 
      sum(df$pred < threshold & df$survived == 1) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}


plot_roc_cost <- function(roc, threshold=0.5, cost_of_fp=1, cost_of_fn=1) {
  
  # Funció: dibuixa un gràfic per trobar el millor llindar
  # Arguments: la corba roc (retorn de la funció calculate_roc) i si volem el valor llindar i de FP i FN
  # Retorn: cap
  
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed") + 
    theme_bw()
  
  sub_title <- sprintf("Llindar a %.2f - cost de FP = %d, cost de FN = %d", threshold, cost_of_fp, cost_of_fn)
  
  p_cost
 # grid.arrange(p_roc, p_cost,nrow=2, sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
}

plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  
  # Funció: dibuixa la corba ROC
  # Arguments: la corba roc (retorn de la funció calculate_roc) i si volem el valor llindar i de FP i FN
  # Retorn: cap
  
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed") +
    theme_bw()

  
  sub_title <- sprintf("Llindar a %.2f - cost de FP = %d, cost de FN = %d", threshold, cost_of_fp, cost_of_fn)
  
  p_roc
  #grid.arrange(p_roc, p_cost,nrow=2, sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
}

plot_pred_type_distribution <- function(df, threshold=0.5) {
  
  # Funció: dibuixa un gràfic per veure la distribució de falsos positius i negatius.
  # Arguments: la corba roc (retorn de la funció calculate_roc) i si volem el valor llindar 
  # Retorn: cap
  
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$survived == 1, "VP", v)
  v <- ifelse(df$pred >= threshold & df$survived == 0, "FP", v)
  v <- ifelse(df$pred < threshold & df$survived == 1, "FN", v)
  v <- ifelse(df$pred < threshold & df$survived == 0, "VN", v)
  
  df$pred_type <- v
  
  ggplot(data=df, aes(x=survived, y=pred)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold)) + 
    theme_bw()
}

