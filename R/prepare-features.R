excluded_prefixes <- c("tanf_", "snap_", "ohp_")

excluded_columns <- c(
  "household_id",
  "person_id",
  "draw_lottery",
  "dt_retro_coverage",
  "dt_notify_lottery",
  "postn_death",
  "zip_msa_list"
)

prepare_analysis_data <- function(datasets) {
  datasets$emergency |>
    dplyr::left_join(datasets$descriptive, by = "person_id") |>
    dplyr::left_join(datasets$state_programs, by = "person_id") |>
    dplyr::left_join(datasets$survey0, by = "person_id") |>
    dplyr::mutate(medicaid = .data$ohp_all_ever_firstn_30sep2009) |>
    keep_pre_lottery_columns() |>
    keep_low_missing_columns() |>
    drop_excluded_features() |>
    dplyr::mutate(age_2009 = 2009 - .data$birthyear_list) |>
    dplyr::select(-"birthyear_list") |>
    tidyr::drop_na() |>
    normalize_model_variables()
}

keep_pre_lottery_columns <- function(df) {
  keep <- !stringr::str_detect(colnames(df), "_ed") |
    stringr::str_detect(colnames(df), "pre_ed") |
    stringr::str_detect(colnames(df), "num_visit_cens_ed")

  df[, keep]
}

keep_low_missing_columns <- function(df, max_missing = 0.05) {
  keep <- purrr::map_lgl(df, function(col) {
    mean(is.na(col)) < max_missing
  })

  df[, keep]
}

drop_excluded_features <- function(df) {
  df |>
    dplyr::select(-dplyr::any_of(excluded_columns)) |>
    dplyr::select(-dplyr::starts_with(excluded_prefixes))
}

normalize_model_variables <- function(df) {
  normalized <- df |>
    dplyr::mutate(
      numhh_list = dplyr::case_when(
        .data$numhh_list == "signed self up" ~ 0L,
        .data$numhh_list == "signed self up + 1 additional person" ~ 1L,
        .data$numhh_list == "signed self up + 2 additional people" ~ 2L
      ),
      treatment = dplyr::case_when(
        .data$treatment == "Selected" ~ 1L,
        .data$treatment == "Not selected" ~ 0L
      ),
      medicaid = dplyr::case_when(
        .data$medicaid == "Enrolled" ~ 1L,
        .data$medicaid == "NOT enrolled" ~ 0L
      )
    )

  stopifnot(
    all(!is.na(normalized$numhh_list)),
    all(!is.na(normalized$medicaid)),
    all(!is.na(normalized$treatment))
  )

  normalized
}

prepare_model_data <- function(df, train_proportion = 0.8, seed = 123) {
  set.seed(seed)
  train_idx <- sample(
    seq_len(nrow(df)),
    round(train_proportion * nrow(df)),
    replace = FALSE
  )

  train <- df[train_idx, ]
  test <- df[-train_idx, ]

  rec <- recipes::recipe(
    num_visit_cens_ed ~ .,
    data = dplyr::select(train, -medicaid, -treatment)
  ) |>
    recipes::step_dummy(recipes::all_nominal())

  prepped_rec <- recipes::prep(rec, train)
  x_train <- recipes::juice(prepped_rec)
  x_test <- recipes::bake(prepped_rec, new_data = test)

  x_train$num_visit_cens_ed <- NULL
  x_test$num_visit_cens_ed <- NULL

  list(
    train = train,
    test = test,
    x_train = x_train,
    x_test = x_test,
    recipe = prepped_rec
  )
}
