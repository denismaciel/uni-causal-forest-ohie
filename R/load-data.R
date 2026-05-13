load_datasets <- function(data_dir = "data/interim/parquet") {
  read_dataset <- function(name) {
    arrow::read_parquet(file.path(data_dir, paste0(name, ".parquet")))
  }

  list(
    descriptive = read_dataset("descriptive"),
    emergency = read_dataset("emergency"),
    state_programs = read_dataset("state_programs"),
    survey0 = read_dataset("survey0"),
    survey12 = read_dataset("survey12")
  )
}

load_analysis_data <- function(data_path = "data/analysis/model-data.parquet") {
  arrow::read_parquet(data_path)
}
