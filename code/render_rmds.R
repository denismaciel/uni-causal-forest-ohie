library(magrittr)

rmd_files <- fs::dir_ls(
  here::here("code"), regexp =  ".Rmd$"
)

DONT_COMPILE <- c("check_randomization","analysis")

ind <- mapply(DONT_COMPILE, FUN = function(x) stringr::str_detect(rmd_files, x)) %>%
  rowSums() %>%
  as.logical()

rmd_files <- rmd_files[!ind]

get_target_dirs <- function(file_names) {
  file_names %>%
    stringr::str_replace("(?<=/)code(?=/)", "compiled_analysis") %>%
    stringr::str_remove("\\.Rmd")
}

target_dirs <- get_target_dirs(rmd_files)

l <- tibble::tibble(rmd_files, target_dirs)

fs::dir_create(target_dirs)
fs::file_create(paste0(target_dirs, "/README.md"))


for (i in 1:nrow(l)) {
  print(i)
  rmarkdown::render(
    input = l[i, ]$rmd_files,
    output_dir = l[i, ]$target_dirs,
    output_file = "README.md"
  )
}

## Exclude absolute path for images ====
remove_absolute_path <- function(target_dir) {
  print(paste("Removing absolute path from", target_dir))
  file_name <- paste0(target_dir, "/", "README.md")
  readLines(file_name) %>%
    stringr::str_remove(paste0(target_dir, "/")) %>%
    paste(collapse = "\n") %>%
    write(file_name)
}

purrr::walk(target_dirs, remove_absolute_path)


