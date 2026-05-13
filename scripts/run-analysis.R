#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
})

source("R/load-data.R")
source("R/prepare-features.R")
source("R/fit-causal-forest.R")
source("R/export-figures.R")
source("R/export-tables.R")

parse_args <- function(args) {
  values <- list(
    seed = 123L,
    trees = 2000L,
    train_proportion = 0.8,
    output_dir = "figs"
  )

  for (arg in args) {
    if (startsWith(arg, "--seed=")) {
      values$seed <- as.integer(sub("--seed=", "", arg))
    } else if (startsWith(arg, "--trees=")) {
      values$trees <- as.integer(sub("--trees=", "", arg))
    } else if (startsWith(arg, "--train-proportion=")) {
      values$train_proportion <- as.numeric(sub("--train-proportion=", "", arg))
    } else if (startsWith(arg, "--output-dir=")) {
      values$output_dir <- sub("--output-dir=", "", arg)
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
  }

  values
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))

  datasets <- load_datasets()
  analysis_data <- prepare_analysis_data(datasets)
  model_data <- prepare_model_data(
    analysis_data,
    train_proportion = args$train_proportion,
    seed = args$seed
  )

  model <- fit_causal_forest(
    model_data,
    num_trees = args$trees,
    seed = args$seed
  )

  predictions <- predict_treatment_effects(model, model_data)
  feature_importance <- extract_feature_importance(model, model_data)

  export_analysis_figures(predictions, model_data, args$output_dir, args$seed)
  export_feature_importance(feature_importance, args$output_dir)
  export_model_summary(predictions)
}

main()
