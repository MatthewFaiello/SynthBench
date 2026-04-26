# =========================================================
# R/cv_helpers.R
# Cross-validation utility helpers
# =========================================================
# Purpose:
#   Provide small helpers for grouped cross-validation,
#   fold-count selection, and repeated-CV stability summaries.
#
# Main helpers:
#   - make_school_foldid()
#   - choose_nfolds()
#   - most_common()
#   - consistency_rate()
# =========================================================

make_school_foldid <- function(dat, seed, nfolds) {
  set.seed(seed)
  
  schools <- sort(unique(dat[[SETTINGS$school_id]]))
  k <- min(nfolds, length(schools))
  
  school_fold <- sample(rep(seq_len(k), length.out = length(schools)))
  names(school_fold) <- schools
  
  as.integer(school_fold[match(dat[[SETTINGS$school_id]], schools)])
}

choose_nfolds <- function(n_schools,
                          default_nfolds = SETTINGS$nfolds,
                          small_sample_cutoff = 60,
                          small_sample_nfolds = 5) {
  if (n_schools < small_sample_cutoff) {
    return(min(small_sample_nfolds, n_schools))
  }
  
  min(default_nfolds, n_schools)
}

most_common <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  names(sort(table(x), decreasing = TRUE))[1]
}

consistency_rate <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  max(prop.table(table(x)))
}