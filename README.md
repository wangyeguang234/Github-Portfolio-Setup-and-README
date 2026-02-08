# ============================================================
# Data Preparation
# IS 6850 (Spring 2026)
# Purpose: reusable cleaning + feature engineering + aggregation
# ============================================================

library(tidyverse)

# ----------------------------
# 1) Basic cleaning functions
# ----------------------------

fix_days_employed <- function(df) {
  # DAYS_EMPLOYED == 365243 is a known placeholder value (anomaly)
  if ("DAYS_EMPLOYED" %in% names(df)) {
    df <- df %>%
      mutate(DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED))
  }
  df
}

add_basic_features <- function(df) {
  # Simple ratios that often help in credit risk
  # (Check columns exist to avoid errors)
  if (all(c("AMT_CREDIT", "AMT_INCOME_TOTAL") %in% names(df))) {
    df <- df %>% mutate(ratio_credit_income = AMT_CREDIT / AMT_INCOME_TOTAL)
  }
  if (all(c("AMT_ANNUITY", "AMT_INCOME_TOTAL") %in% names(df))) {
    df <- df %>% mutate(ratio_annuity_income = AMT_ANNUITY / AMT_INCOME_TOTAL)
  }
  if (all(c("AMT_GOODS_PRICE", "AMT_CREDIT") %in% names(df))) {
    df <- df %>% mutate(ratio_goods_credit = AMT_GOODS_PRICE / AMT_CREDIT)
  }

  # Convert day-based demographics into years (more interpretable)
  if ("DAYS_BIRTH" %in% names(df)) {
    df <- df %>% mutate(age_years = -DAYS_BIRTH / 365)
  }
  if ("DAYS_EMPLOYED" %in% names(df)) {
    df <- df %>% mutate(emp_years = -DAYS_EMPLOYED / 365)
  }

  df
}

add_missing_indicators <- function(df, cols) {
  # Add a 0/1 indicator for missingness (often predictive)
  # cols should be a character vector of column names (e.g., EXT_SOURCE_1..3)
  for (cname in cols) {
    if (cname %in% names(df)) {
      new_name <- paste0(cname, "_NA")
      df[[new_name]] <- ifelse(is.na(df[[cname]]), 1, 0)
    }
  }
  df
}

# -----------------------------------------
# 2) Train-only summaries for imputation
# -----------------------------------------

fit_imputer <- function(train_df, numeric_cols) {
  # Store medians computed from TRAIN ONLY (avoid leakage)
  medians <- map_dbl(numeric_cols, function(cname) {
    if (cname %in% names(train_df)) median(train_df[[cname]], na.rm = TRUE) else NA_real_
  })
  tibble(col = numeric_cols, median = medians)
}

apply_imputer <- function(df, imputer_tbl) {
  for (i in seq_len(nrow(imputer_tbl))) {
    cname <- imputer_tbl$col[i]
    med <- imputer_tbl$median[i]
    if (!is.na(med) && cname %in% names(df)) {
      df[[cname]] <- ifelse(is.na(df[[cname]]), med, df[[cname]])
    }
  }
  df
}

# -----------------------------------------
# 3) Aggregation of supplementary datasets
# -----------------------------------------

aggregate_bureau <- function(bureau_df) {
  # Example aggregation at SK_ID_CURR level
  bureau_df %>%
    group_by(SK_ID_CURR) %>%
    summarize(
      bureau_count = n(),
      bureau_amt_credit_sum = if ("AMT_CREDIT_SUM" %in% names(bureau_df)) sum(AMT_CREDIT_SUM, na.rm = TRUE) else NA_real_,
      bureau_amt_credit_mean = if ("AMT_CREDIT_SUM" %in% names(bureau_df)) mean(AMT_CREDIT_SUM, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
}

# You will add these later when you have the files:
# aggregate_previous_application <- function(prev_df) { ... }
# aggregate_installments_payments <- function(inst_df) { ... }

# -----------------------------------------
# 4) Master pipeline functions
# -----------------------------------------

prep_application_features <- function(df) {
  df %>%
    fix_days_employed() %>%
    add_basic_features() %>%
    add_missing_indicators(cols = c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3"))
}

# Build everything using TRAIN data
build_prep_objects <- function(app_train, bureau_df = NULL) {
  # 1) basic feature creation
  train_feat <- prep_application_features(app_train)

  # 2) choose numeric cols to impute (example: EXT_SOURCE + ratios)
  numeric_cols <- intersect(
    c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3",
      "ratio_credit_income", "ratio_annuity_income", "ratio_goods_credit",
      "age_years", "emp_years"),
    names(train_feat)
  )

  imputer_tbl <- fit_imputer(train_feat, numeric_cols)

  # 3) aggregate bureau if provided
  bureau_agg <- NULL
  if (!is.null(bureau_df)) {
    bureau_agg <- aggregate_bureau(bureau_df)
  }

  list(
    imputer = imputer_tbl,
    bureau_agg = bureau_agg,
    numeric_cols = numeric_cols
  )
}

# Apply to TRAIN or TEST using stored objects (no leakage)
apply_prep <- function(app_df, prep_obj) {
  out <- prep_application_features(app_df)
  out <- apply_imputer(out, prep_obj$imputer)

  # join bureau features if available
  if (!is.null(prep_obj$bureau_agg)) {
    out <- out %>% left_join(prep_obj$bureau_agg, by = "SK_ID_CURR")
  }

  out
}

# -----------------------------------------
# 5) Example usage (commented)
# -----------------------------------------
# app_train <- read_csv("application_train.csv")
# app_test  <- read_csv("application_test.csv")
# bureau <- read_csv("bureau.csv")
#
# prep_obj <- build_prep_objects(app_train, bureau_df = bureau)
# train_ready <- apply_prep(app_train, prep_obj)
# test_ready  <- apply_prep(app_test, prep_obj)
#
# # train_ready/test_ready now have consistent transformations

