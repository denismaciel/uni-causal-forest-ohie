library(magrittr)

read_stata_dataset <- function(file_path) {
  file_path %>%
    here::here() %>%
    haven::read_dta(file_path) %>%
    haven::as_factor()
}

descriptive <- read_stata_dataset("data/OHIE_Public_Use_Files/OHIE_Data/oregonhie_descriptive_vars.dta")
state_programs <- read_stata_dataset("data/OHIE_Public_Use_Files/OHIE_Data/oregonhie_stateprograms_vars.dta")
survey12 <- read_stata_dataset("data/OHIE_Public_Use_Files/OHIE_Data/oregonhie_survey12m_vars.dta")
survey0 <- read_stata_dataset("data/OHIE_Public_Use_Files/OHIE_Data/oregonhie_survey0m_vars.dta")
emergency <- read_stata_dataset("data/OHIE_Public_Use_Files/OHIE_Data/oregonhie_ed_vars.dta")

readr::write_rds(descriptive, here::here("data", "transformed", "descriptive.rds"))
readr::write_rds(state_programs, here::here("data", "transformed", "state_programs.rds"))
readr::write_rds(survey12, here::here("data", "transformed", "survey12.rds"))
readr::write_rds(survey0, here::here("data", "transformed", "survey0.rds"))
readr::write_rds(emergency, here::here("data", "transformed", "emergency.rds"))
