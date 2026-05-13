fit_causal_forest <- function(model_data, num_trees = 2000, seed = 123) {
  grf::instrumental_forest(
    X = as.matrix(model_data$x_train),
    Y = as.numeric(model_data$train$num_visit_cens_ed),
    W = as.numeric(model_data$train$medicaid),
    Z = as.numeric(model_data$train$treatment),
    num.trees = num_trees,
    seed = seed,
    honesty = TRUE
  )
}

predict_treatment_effects <- function(model, model_data) {
  predictions <- predict(
    model,
    newdata = as.matrix(model_data$x_test),
    estimate.variance = TRUE
  )

  tibble::as_tibble(predictions)
}

extract_feature_importance <- function(model, model_data) {
  tibble::tibble(importance = as.numeric(grf::variable_importance(model))) |>
    dplyr::mutate(feature = colnames(model_data$x_train)) |>
    dplyr::arrange(dplyr::desc(.data$importance))
}
