f <- fs::dir_ls("data/OHIE_Public_Use_Files/OHIE_Documentation/", regexp = "\\.pdf$")

l <- map(f, pdftools::pdf_text)

# fs::dir_create("data/OHIE_Public_Use_Files/OHIE_Documentation/txt")

f_txt <- stringr::str_replace(f, "\\.pdf$", "\\.txt")

map2(l, f_txt, function(.x, .y) {
  write(.x, file = .y)
})
