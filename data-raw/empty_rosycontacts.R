## code to prepare `empty_rosycontacts` dataset goes here
empty_rosycontacts <- list(
  pkg_date = pkg_date,
  pkg_version = pkg_version,
  pkg_version = pkg_version,
  original = list(),
  optional_edit_wide = list(),
  final = list()
)

x <- pdftools::pdf_text("data-raw/vcf_doc.pdf") %>% paste0(collapse = "")
x <- gsub("\n"," ",x)
while (grepl("  ",x) %>% any()) {
  x<- gsub("  "," ",x)
  print("1")
}
matches <- stringr::str_extract_all(x, "(?<=\\s)\\w+(?=\\sType name:)")
matches <-matches %>%  append(stringr::str_extract_all(x, "(?<=\\s)\\w+(?=\\sType Name:)"))
result <- unlist(matches) %>% unique()
result
VCF_names <- c(
  "FN","N","PHOTO","BDAY","ADR",
  "LABEL","TEL","EMAIL","MAILER","TZ","GEO",
  "TITLE","ROLE","LOGO","AGENT","ORG","CATEGORIES",
  "NOTE","PRODID","REV","SOUND","UID","URL",
  "VERSION","CLASS","KEY"
)

does_not_repeat <- c("FN","N","PRODID","BDAY","NOTE","PHOTO")

usethis::use_data(empty_rosycontacts, does_not_repeat, overwrite = TRUE)
