load_datasets <- function() {
  descriptive <<- arrow::read_parquet(here::here("data", "interim", "parquet", "descriptive.parquet"))
  survey12 <<- arrow::read_parquet(here::here("data", "interim", "parquet", "survey12.parquet"))
  survey0 <<- arrow::read_parquet(here::here("data", "interim", "parquet", "survey0.parquet"))
  emergency <<- arrow::read_parquet(here::here("data", "interim", "parquet", "emergency.parquet"))
  state_programs <<- arrow::read_parquet(here::here("data", "interim", "parquet", "state_programs.parquet"))
}

load_datasets()
