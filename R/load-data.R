load_datasets <- function(data_dir = "data/transformed") {
  read_dataset <- function(name) {
    readr::read_rds(file.path(data_dir, paste0(name, ".rds")))
  }

  list(
    descriptive = read_dataset("descriptive"),
    emergency = read_dataset("emergency"),
    state_programs = read_dataset("state_programs"),
    survey0 = read_dataset("survey0"),
    survey12 = read_dataset("survey12")
  )
}
