# Helper functions

library(readxl)
library(lubridate)
library(kableExtra)
library(tibble)


treat_factors = function(x, all, correct) {
  x = as.character(x)
  for (a in all) {
    x[x == a] = correct
  }
  
  as.factor(as.character(x))
}

safe_numeric = function(x) {
  # Find weird separators
  x = gsub(",", ".", x)
  x = gsub("'", ".", x)

  # Remove all characters from data (units, typos, etc)
  x = gsub("[^0-9.-<]", "", x)
  
  # Check for values under the detecion limit and substitute for LDL/sqrt(2)
  # TODO: improve this
  x = ifelse(grepl("<", x), as.character(as.numeric(substr(x, 2, 100))/sqrt(2)), x)
  
  # Convert to numeric and return  
  as.numeric(as.character(x))
}


import_excel = function(file_name) {
  dir = "data"
  path = file.path(dir, file_name)
  
  # read file
  df = read_excel(path, sheet = 2, col_types = "text")
  
  # Remove unwanted columns
  df = df[, 1:89]
  
  # Remove unwanted rows (if they have no id nor outcome)
  cond_ = is.na(df[, 1]) & is.na(df[,ncol(df)])
  df = df[!cond_, ]
  
  df
  
}

missing_to_no = function(x) {
  x[is.na(x)] = "No"
  x
}

tranform_date = function(x) {
  if ("/" %in% x | "-" %in% x) {
    return(as.Date(as.character(x), 
                   tryFormats = c("%d/%m/%Y", "%d/%m/%y", "%d-%m-%Y", "%d-%m-%y")))
  }
  else {
    return(as.Date(safe_numeric(x), origin = "1900-01-01"))
  }
}

transform_dates = function(x) {

  temp = as_date(safe_numeric(x), origin = "1900-01-01")
  temp2 = dmy(x)
  
  ymd(ifelse(is.na(temp), as.character(temp2), as.character(temp)))
}

pred_adj_curves <- function(cox, df, ids, group, strata=NULL, cuminc=FALSE) {
  # We have a prediction for each person for each timepoint, each one 
  # with their confidence interval and we need to average all people in the same 
  # factor group and in the same timepoint
  
  orig_group = group
  
  pred = survfit(cox, df)
  
  if (!"weights" %in% names(df)) {
    df$weights = 1:nrow(df)
  }
  
  if (cuminc) {
    tt = cbind.data.frame(pred$time, 1 - pred$surv, pred$std.err)
  } else {
    tt = cbind.data.frame(pred$time, pred$surv, pred$std.err)
  }
  
  names(tt) = c("temps", "preds", "std.err")
  
  sh = data.table::shift(tt$temps, 1)
  first = (is.na(sh) | tt$temps < sh)
  pers = df$ids
  tt$ids = first
  tt$ids[tt$ids] = pers
  tt$ids[tt$ids == 0] = NA
  tt$ids = zoo::na.locf0(tt$ids)
  
  noms = c(ids, group, "weights", strata)
  ttt = merge(tt, df[, names(df) %in% noms], by = ids, all = T)
  
  if (!is.null(strata)) {
    ttt$group = as.factor(paste0(as.character(ttt[, strata]), as.character(ttt[, group])))
  }
  else {
    ttt$group = ttt[, group]
  }
  
  aa = ttt %>%
    group_by(temps, group) %>%
    summarise(
      avg_pred = weighted.mean(preds, weights),
      sd_pred = weighted.mean(std.err, weights)
    )
  
  names(aa)[names(aa) == "group"] = orig_group
  
  # Small correction (this is probably worth looking into (TODO))
  # This is due to the fact that we have stratified the model into sexes
  if (is.null(strata) | FALSE) {  # bad idea
    roll_curve(aa, group)
  }
  
  as.data.frame(aa)
}

roll_curve = function(aa, var) {
  fa = as.data.frame(matrix(nrow = 0, ncol = ncol(aa)))
  names(fa) = names(aa)
  
  aa = as.data.frame(aa)
  
  for (lev in levels(aa[, var])) {
    aest = aa[aa[, var] == lev, ]
    shf = data.table::shift(aest$avg_pred, -1)
    shf = zoo::na.locf(shf)
    aest$avg_pred = ifelse(aest$avg_pred < shf, shf, aest$avg_pred)
    fa = rbind.data.frame(fa, aest)
  }
  fa
  
}

nice_table <- function(df, dec = 3, decp = 5, rnames = F) {
  names(df)[ncol(df)] = "Pr(>|z|)"
  df %>%
    rownames_to_column('rnames') %>%
    mutate(`Pr(>|z|)` = cell_spec(`Pr(>|z|)`, "html", bold = `Pr(>|z|)` < 0.05)) %>%
    column_to_rownames('rnames') %>%
    kable(format = "html",
          escape = F,
          row.names = rnames) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = F
    )
}

cox_sum_table <- function(obj, dec = 3, decp = 5) {
  sm = summary(obj)
  smdf = as.data.frame(sm$coefficients)
  smdf$`robust se` = NULL
  smdf$`se(coef)` = NULL
  smdf[1:3] = round(smdf[1:3], dec)
  smdf$`Pr(>|z|)` = round(smdf$`Pr(>|z|)`, decp)
  
  nice_table(smdf, dec, decp, rnames = T)
  
}