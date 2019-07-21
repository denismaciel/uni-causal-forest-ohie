load_datasets <- function() {
  descriptive <<- readr::read_rds(here::here("data", "transformed", "descriptive.rds"))
  survey12 <<- readr::read_rds(here::here("data", "transformed", "survey12.rds"))
  survey0 <<- readr::read_rds(here::here("data", "transformed", "survey0.rds"))
  emergency <<- readr::read_rds(here::here("data", "transformed", "emergency.rds"))
  state_programs <<- readr::read_rds(here::here("data", "transformed", "state_programs.rds"))
}

load_datasets()
