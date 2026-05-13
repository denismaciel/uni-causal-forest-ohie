#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
})

source("R/load-data.R")
source("R/prepare-features.R")
source("R/convert-data.R")

convert_stata_to_parquet()
build_analysis_dataset()
