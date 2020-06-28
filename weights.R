# weighted functions and table creation

wtd_mwt <- function(df, i, grp, weights) {
  x.name <- names(df)[i]
  g.name <- deparse(substitute(grp))
  w.name <- deparse(substitute(weights))
  vars <- c(x.name, g.name, w.name)
  
  weighted_mannwhitney_helper(df, vars)
}

weighted_mannwhitney_helper <- function(dat, vars) {
  # check if pkg survey is available
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("Package `survey` needed to for this function to work. Please install it.", call. = FALSE)
  }
  
  dat = dat[, vars]
  
  x.name <- colnames(dat)[1]
  group.name <- colnames(dat)[2]
  
  colnames(dat) <- c("x", "g", "w")
  
  design <- survey::svydesign(ids = ~0, data = dat, weights = ~w)
  mw <- survey::svyranktest(formula = x ~ g, design)
  
  attr(mw, "x.name") <- x.name
  attr(mw, "group.name") <- group.name
  class(mw) <- c("sj_wmwu", "list")
  
  if (dplyr::n_distinct(dat$g, na.rm = TRUE) > 2)
    m <- "Weighted Kruskal-Wallis test"
  else
    m <- "Weighted Mann-Whitney-U test"
  
  mw$method <- m
  
  mw
}

wtd_ttest <- function(i, grp, data, weights, mu = 0, paired = FALSE, ci.lvl = 0.95, alternative = c("two.sided", "less", "greater"), ...) {
  
  if (!missing(ci.lvl) & (length(ci.lvl) != 1 || !is.finite(ci.lvl) || ci.lvl < 0 || ci.lvl > 1))
    stop("'ci.lvl' must be a single number between 0 and 1")
  
  alternative <- match.arg(alternative)
  
  x <- data[, i]
  y <- data[, grp]
  w <- weights
  vars <- c(x, y, w)
  
  if (is.factor(y))
    grps <- levels(y)
  else
    grps <- na.omit(sort(unique(y)))
  
  if (length(grps) > 2)
    stop("Grouping factor has more than two levels.")
  
  xv <- x[y == grps[1]]
  yv <- x[y == grps[2]]
  wx <- w[y == grps[1]]
  wy <- w[y == grps[2]]
  
  mxv <- is.na(xv)
  xv <- xv[!mxv]
  wx <- wx[!mxv]
  
  myv <- is.na(yv)
  yv <- yv[!myv]
  wy <- wy[!myv]
  
  nx <- length(xv)
  ny <- length(yv)
  
  labs <- sjlabelled::get_labels(
    data[[vars[2]]],
    attr.only = FALSE,
    values = "p",
    drop.na = TRUE,
    drop.unused = TRUE
  )
  
  weighted_ttest_helper(xv, yv, wx, wy, nx, ny, mu, paired, alternative, ci.lvl, vars[1], vars[2], labs)
}

weighted_ttest_helper <- function(xv, yv, wx, wy, nx, ny, mu, paired, alternative, ci.lvl, x.name, y.name, group.name) {
  if (paired) {
    xv <- xv - yv
    yv <- NULL
  }
  
  mu.x.w <- stats::weighted.mean(xv, wx)
  var.x.w <- weighted_sd(xv, wx)^2
  se.x <- sqrt(var.x.w / nx)
  
  if (!is.null(yv)) {
    mu.y.w <- stats::weighted.mean(yv, wy)
    var.y.w <- weighted_sd(yv, wy)^2
    se.y <- sqrt(var.y.w / ny)
    
    se <- sqrt(se.x^2 + se.y^2)
    df <- se^4 / (se.x^4 / (nx - 1) + se.y^4 / (ny - 1))
    tstat <- (mu.x.w - mu.y.w - mu) / se
    
    estimate <- c(mu.x.w, mu.y.w)
    names(estimate) <- c("mean of x", "mean of y")
    method <- "Two-Sample t-test"
  } else {
    se <- se.x
    df <- nx - 1
    tstat <- (mu.x.w - mu) / se
    
    estimate <- stats::setNames(mu.x.w, if (paired)  "mean of the differences" else "mean of x")
    method <- if (paired) "Paired t-test" else "One Sample t-test"
  }
  
  
  
  if (alternative == "less") {
    pval <- stats::pt(tstat, df)
    cint <- c(-Inf, tstat + stats::qt(ci.lvl, df))
  } else if (alternative == "greater") {
    pval <- stats::pt(tstat, df, lower.tail = FALSE)
    cint <- c(tstat - stats::qt(ci.lvl, df), Inf)
  } else {
    pval <- 2 * stats::pt(-abs(tstat), df)
    alpha <- 1 - ci.lvl
    cint <- stats::qt(1 - alpha / 2, df)
    cint <- tstat + c(-cint, cint)
  }
  
  cint <- mu + cint * se
  
  names(tstat) <- "t"
  names(df) <- "df"
  names(mu) <- if (paired || !is.null(yv)) "difference in means" else "mean"
  
  
  tt <- structure(
    class = "sj_ttest",
    list(
      estimate = estimate,
      statistic = tstat,
      df = df,
      p.value = pval,
      ci = cint,
      alternative = alternative,
      method = method
    )
  )
  
  attr(tt, "x.name") <- x.name
  attr(tt, "y.name") <- y.name
  attr(tt, "group.name") <- group.name
  
  tt
}

wtd_desc_table <- function(df, group_name, weights=NULL, r_num=2, p_num=5) {
  library(reldist)
  library(sjstats)
  library(weights)
  
  if (is.null(weights)) {
    df$weights = rep(1, nrow(df))
  }
  
  group = df[, group_name]
  
  wtab = c()
  for (var in 1:ncol(df)) {
    name = names(df)[var]
    
    if (name %in% c(group_name, "weights")) {
      next
    }
    
    if (is.factor(df[, var])) {
      # All
      ta = questionr::wtd.table(df[, var], weights = weights)
      # Proportions
      propta = prop.table(ta)
      # Divided
      tn = questionr::wtd.table(df[, var], group, weights = weights)
      # Proportions
      proptn = prop.table(tn, margin = 2)
      # Hypothesis test
      tt = chisq.test(tn)
    
      vals = c()
      for (lev in levels(df[, var])) {
        all = paste0(round(ta[lev], r_num), " (", round(propta[lev] * 100, r_num), "%)")
        levs = paste0(round(tn[lev, ], r_num), " (", round(proptn[lev,] * 100, r_num), "%)")
        vals = rbind(vals, unname(c(name, lev, all, levs, round(tt$p.value, p_num))))
      }
      
    }
    else if (is.numeric(df[, var])) {
      name = names(df)[var]
      
      # Check if it's normaly distributed
      st = shapiro.test(df[, var])
      
      # Not normal
      if (st$p.value < 0.05) {
        # All
        q = round(Hmisc::wtd.quantile(df[, var], probs = c(0.25, 0.5, 0.75), weights = weights), r_num)
        
        all = paste0(q[2], " [", q[1], " ", q[3], "]")
        
        # By levels
        levs = c()
        for (lev in levels(group)) {
          obs = group == lev
          temp = df[obs, var]
          q = round(Hmisc::wtd.quantile(temp, probs = c(0.25, 0.5, 0.75), weights = weights[obs]), r_num)
          levs = c(levs, paste0(q[2], " [", q[1], " ", q[3], "]"))
        }
        # Hypothesis test
        tt = wtd_mwt(
          df = df,
          i = var,
          grp = Estatines,
          weights = weights
        )
        tp = round(tt$p.value, p_num)
        vals = c(name, "--", all, levs, tp)
      }
      # Normal
      else {
        # All
        m = wtd.mean(df[, var], weight = weights[obs])
        s = sqrt(weights[obs] * (df[, var] - m) ^ 2)
        all = paste0(round(m, r_num), " (", round(s, r_num), ")")
        
        # By levels
        levs = c()
        for (lev in levels(group)) {
          obs = group == lev
          temp = df[obs, var]
          m = wtd.mean(temp, weight = weights[obs])
          s = sqrt(weights[obs] * (temp - m) ^ 2)
          levs = c(levs, paste0(round(m, r_num), " (", round(s, r_num), ")"))
        }
        
        # Hypothesis test
        fmla = as.formula(paste0(names(df)[var], " ~ Estatines"))
        tt = wtd_ttest(
          i = var,
          grp = group_name,
          data = df,
          weights = weights
        )
        tp =  round(tt$p.value, 5)
        vals = c(name, "--", all, levs, tp)
      }
      
    }
    else {
      # Characters, dates, etc
      next
    }
    
    wtab = rbind(wtab, vals)
  }
  
  wtab = as.data.frame(wtab)
  wtab[, ncol(wtab)] = safe_numeric(wtab[, ncol(wtab)])
  wtab
}