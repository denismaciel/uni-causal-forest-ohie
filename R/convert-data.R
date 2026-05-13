raw_stata_files <- c(
  descriptive = "oregonhie_descriptive_vars.dta",
  emergency = "oregonhie_ed_vars.dta",
  inperson = "oregonhie_inperson_vars.dta",
  patterns = "oregonhie_patterns_vars.dta",
  state_programs = "oregonhie_stateprograms_vars.dta",
  survey0 = "oregonhie_survey0m_vars.dta",
  survey6 = "oregonhie_survey6m_vars.dta",
  survey12 = "oregonhie_survey12m_vars.dta"
)

read_stata_table <- function(path) {
  haven::read_dta(path) |>
    haven::as_factor()
}

convert_stata_to_parquet <- function(
    raw_dir = "data/OHIE_Public_Use_Files/OHIE_Data",
    output_dir = "data/interim/parquet") {
  fs::dir_create(output_dir)

  purrr::iwalk(raw_stata_files, function(file_name, table_name) {
    table <- read_stata_table(file.path(raw_dir, file_name))
    arrow::write_parquet(table, file.path(output_dir, paste0(table_name, ".parquet")))
  })
}

build_analysis_dataset <- function(interim_dir = "data/interim/parquet", output_dir = "data/analysis") {
  fs::dir_create(output_dir)

  datasets <- load_datasets(interim_dir)
  analysis_data <- prepare_analysis_data(datasets)

  arrow::write_parquet(analysis_data, file.path(output_dir, "model-data.parquet"))
  invisible(analysis_data)
}
