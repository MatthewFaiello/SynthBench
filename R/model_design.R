# =========================================================
# R/model_design.R
# Benchmark model-design helpers
# =========================================================
# Purpose:
#   Build formula labels, right-hand-side model terms, and
#   model matrices for benchmark model definitions.
#
# Main helpers:
#   - make_formula_label()
#   - make_rhs_terms()
#   - make_benchmark_design()
# =========================================================

make_formula_label <- function(selected_groups) {
  if (length(selected_groups) == 0) {
    return(SETTINGS$mandatory_group)
  }
  
  paste0(
    SETTINGS$mandatory_group,
    "(",
    paste(selected_groups, collapse = " + "),
    ")"
  )
}

make_rhs_terms <- function(predictor_groups, selected_groups) {
  year_cols <- predictor_groups[[SETTINGS$mandatory_group]]
  
  if (length(selected_groups) > 0) {
    selected_cols <- unlist(predictor_groups[selected_groups], use.names = FALSE)
  } else {
    selected_cols <- character(0)
  }
  
  rhs_terms <- year_cols
  
  if (length(selected_cols) > 0) {
    interaction_terms <- as.vector(
      outer(year_cols, selected_cols, paste, sep = ":")
    )
    
    rhs_terms <- c(
      rhs_terms,
      selected_cols,
      interaction_terms
    )
  }
  
  unique(rhs_terms)
}

make_benchmark_design <- function(df,
                                  predictor_groups,
                                  group_toggles) {
  selected_groups <- names(group_toggles)[group_toggles]
  
  formula_label <- make_formula_label(selected_groups)
  
  rhs_terms <- make_rhs_terms(
    predictor_groups = predictor_groups,
    selected_groups = selected_groups
  )
  
  # rhs_terms may include interaction terms such as:
  # SchoolYear.2025:LowIncome.LOWINC
  #
  # Those interaction terms are not raw columns in df. model.matrix()
  # creates them from their component columns. So we validate the
  # component columns, not the full interaction strings.
  raw_terms_needed <- unique(unlist(strsplit(rhs_terms, ":", fixed = TRUE)))
  
  missing_raw_terms <- setdiff(raw_terms_needed, names(df))
  
  if (length(missing_raw_terms) > 0) {
    stop(
      "Design matrix is missing expected raw columns: ",
      paste(missing_raw_terms, collapse = ", "),
      call. = FALSE
    )
  }
  
  formula_obj <- as.formula(
    paste(SETTINGS$outcome, "~", paste(rhs_terms, collapse = " + "))
  )
  
  X <- model.matrix(formula_obj, data = df)
  
  if ("(Intercept)" %in% colnames(X)) {
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  }
  
  keep_cols <- apply(X, 2, stats::sd, na.rm = TRUE) > 0
  keep_cols[is.na(keep_cols)] <- FALSE
  X <- X[, keep_cols, drop = FALSE]
  
  y <- df[[SETTINGS$outcome]]
  w <- df[[SETTINGS$weight_var]]
  
  if (!is.numeric(y)) {
    stop("Outcome column must be numeric: ", SETTINGS$outcome, call. = FALSE)
  }
  
  if (!is.numeric(w)) {
    stop("Weight column must be numeric: ", SETTINGS$weight_var, call. = FALSE)
  }
  
  if (any(!is.finite(y))) {
    stop("Outcome column contains non-finite values.", call. = FALSE)
  }
  
  if (any(!is.finite(w)) || any(w <= 0)) {
    stop("Weight column must contain positive finite values.", call. = FALSE)
  }
  
  list(
    selected_groups = selected_groups,
    formula_label = formula_label,
    rhs_terms = rhs_terms,
    formula_obj = formula_obj,
    X = X,
    y = y,
    w = w,
    x_term_stats = build_x_term_stats(X, w),
    term_meta = build_term_meta(colnames(X))
  )
}