export_feature_importance <- function(feature_importance, output_dir = "figs") {
  fs::dir_create(output_dir)
  readr::write_csv(
    feature_importance,
    file.path(output_dir, "feat_importance.csv")
  )
}

export_model_summary <- function(predictions, output_dir = "artifacts/tables") {
  fs::dir_create(output_dir)

  summary <- tibble::tibble(
    metric = c("min", "first_quartile", "median", "mean", "third_quartile", "max"),
    value = c(
      min(predictions$predictions),
      unname(stats::quantile(predictions$predictions, 0.25)),
      stats::median(predictions$predictions),
      mean(predictions$predictions),
      unname(stats::quantile(predictions$predictions, 0.75)),
      max(predictions$predictions)
    )
  )

  readr::write_csv(summary, file.path(output_dir, "treatment-effect-summary.csv"))
}
