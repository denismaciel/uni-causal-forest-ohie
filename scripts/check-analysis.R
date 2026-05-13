#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
})

source("R/load-data.R")
source("R/prepare-features.R")

required_outputs <- c(
  "figs/entropy.png",
  "figs/ete_p_rank.png",
  "figs/ete_p_rank_interval.png",
  "figs/ete_histogram.png",
  "figs/feat_importance.csv",
  "artifacts/tables/treatment-effect-summary.csv"
)

datasets <- load_datasets()
analysis_data <- load_analysis_data()
model_data <- prepare_model_data(analysis_data, seed = 123)

stopifnot(
  identical(analysis_data, prepare_analysis_data(datasets)),
  nrow(analysis_data) > 0,
  nrow(model_data$train) > 0,
  nrow(model_data$test) > 0,
  ncol(model_data$x_train) == ncol(model_data$x_test),
  !anyNA(model_data$x_train),
  !anyNA(model_data$x_test),
  all(file.exists(required_outputs))
)

message("analysis checks passed")
