# =========================================================
# R/metrics.R
# Weighted metric and residual-summary helpers
# =========================================================
# Purpose:
#   Provide small statistical helper functions used by the
#   benchmark workflow for model diagnostics, residual scaling,
#   and benchmark-gap summaries.
#
# Main helpers:
#   - weighted_metrics()
#   - safe_sd()
#   - weighted_residual_sd()
#   - weighted_sd_pop()
# =========================================================

weighted_metrics <- function(y, pred, w) {
  ok <- is.finite(y) & is.finite(pred) & is.finite(w)
  
  y <- y[ok]
  pred <- pred[ok]
  w <- w[ok]
  
  w_sum <- sum(w)
  resid <- y - pred
  ybar <- sum(w * y) / w_sum
  
  sse <- sum(w * resid^2)
  sst <- sum(w * (y - ybar)^2)
  
  tibble(
    wrmse = sqrt(sse / w_sum),
    wmae = sum(w * abs(resid)) / w_sum,
    wr2 = ifelse(sst > 0, 1 - sse / sst, NA_real_)
  )
}

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  
  if (length(x) <= 1) {
    return(0)
  }
  
  stats::sd(x)
}

weighted_residual_sd <- function(resid, w) {
  ok <- is.finite(resid) & is.finite(w)
  
  resid <- resid[ok]
  w <- w[ok]
  
  if (length(resid) == 0 || sum(w) <= 0) {
    return(NA_real_)
  }
  
  resid_mean <- weighted.mean(resid, w, na.rm = TRUE)
  resid_sd <- sqrt(weighted.mean((resid - resid_mean)^2, w, na.rm = TRUE))
  
  if (!is.finite(resid_sd) || resid_sd <= 0) {
    return(NA_real_)
  }
  
  resid_sd
}

weighted_sd_pop <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & (w > 0)
  
  x <- x[ok]
  w <- w[ok]
  
  if (length(x) <= 1 || sum(w) <= 0) {
    return(NA_real_)
  }
  
  mu <- weighted.mean(x, w)
  sqrt(sum(w * (x - mu)^2) / sum(w))
}
