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

  if ("DAYS_EMPLOYED" %in% names(df)) {
    df <- df %>%
      mutate(DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED))
  }
  df
}

add_basic_features <- function(df) {

  if (all(c("AMT_CREDIT", "AMT_INCOME_TOTAL") %in% names(df))) {
    df <- df %>% mutate(ratio_credit_income = AMT_CREDIT / AMT_INCOME_TOTAL)
  }
  if (all(c("AMT_ANNUITY", "AMT_INCOME_TOTAL") %in% names(df))) {
    df <- df %>% mutate(ratio_annuity_income = AMT_ANNUITY / AMT_INCOME_TOTAL)
  }
  if (all(c("AMT_GOODS_PRICE", "AMT_CREDIT") %in% names(df))) {
    df <- df %>% mutate(ratio_goods_credit = AMT_GOODS_PRICE / AMT_CREDIT)
  }

  if ("DAYS_BIRTH" %in% names(df)) {
    df <- df %>% mutate(age_years = -DAYS_BIRTH / 365)
  }
  if ("DAYS_EMPLOYED" %in% names(df)) {
    df <- df %>% mutate(emp_years = -DAYS_EMPLOYED / 365)
  }

  df
}

add_missing_indicators <- function(df, cols) {

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

  bureau_df %>%
    group_by(SK_ID_CURR) %>%
    summarize(
      bureau_count = n(),
      bureau_amt_credit_sum = if ("AMT_CREDIT_SUM" %in% names(bureau_df)) sum(AMT_CREDIT_SUM, na.rm = TRUE) else NA_real_,
      bureau_amt_credit_mean = if ("AMT_CREDIT_SUM" %in% names(bureau_df)) mean(AMT_CREDIT_SUM, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
}


# -----------------------------------------
# 4) Master pipeline functions
# -----------------------------------------

library(tidyverse)

# 1) Quick sanity check: basic numeric summaries
num_cols <- app_train %>% select(where(is.numeric))

summary_tbl <- num_cols %>%
  summarise(across(everything(),
                   list(min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# Show a small sample of min/max (full table is too wide to print)
summary_tbl %>% select(1:20)

app_train %>%
  summarise(
    n = n(),
    days_emp_365243 = sum(DAYS_EMPLOYED == 365243, na.rm = TRUE),
    pct_days_emp_365243 = mean(DAYS_EMPLOYED == 365243, na.rm = TRUE)
  )

app_train %>%
  summarise(
    min_days_birth = min(DAYS_BIRTH, na.rm = TRUE),
    max_days_birth = max(DAYS_BIRTH, na.rm = TRUE),
    min_days_employed = min(DAYS_EMPLOYED, na.rm = TRUE),
    max_days_employed = max(DAYS_EMPLOYED, na.rm = TRUE)
  )

app_train %>%
  summarise(
    neg_income = sum(AMT_INCOME_TOTAL < 0, na.rm = TRUE),
    neg_credit = sum(AMT_CREDIT < 0, na.rm = TRUE),
    neg_annuity = sum(AMT_ANNUITY < 0, na.rm = TRUE),
    neg_goods_price = sum(AMT_GOODS_PRICE < 0, na.rm = TRUE)
  )

app_train %>%
  select(SK_ID_CURR, AMT_INCOME_TOTAL, AMT_CREDIT, AMT_ANNUITY, AMT_GOODS_PRICE) %>%
  arrange(desc(AMT_INCOME_TOTAL)) %>%
  slice(1:10)

# variance of numeric columns (NA-safe)
var_tbl <- app_train %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~var(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "variance") %>%
  arrange(variance)

# show the lowest-variance variables
var_tbl %>% slice(1:20)

var_tbl %>%
  filter(variance == 0)


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

