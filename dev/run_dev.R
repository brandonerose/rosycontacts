#refresh -----------------------

rm(list=ls(all.names = T))
cat("\014")
if(!is.null(dev.list())) dev.off()
graphics.off()
devtools::document()
attachment::att_amend_desc()
golem::detach_all_attached()
# devtools::unload()
devtools::load_all()
# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

#load -----------------------

your_folder <- "/Users/brandon/Desktop/rosycontacts"
file_path <- "/Users/brandon/Desktop/rosycontacts/origin.vcf"
file_path2 <- "/Users/brandon/Desktop/rosycontacts/all_contacts.xlsx"
file_path3 <- "/Users/brandon/Desktop/rosycontacts/final/contacts_wide_optional_edit.xlsx"
# file_path <- "/Users/brandonrose/R/messages/contacts3.vcf"
#run shiny app -----------------------
# source("/Users/brandonrose/R/brose/drive_location.R")

rosycontacts <- file_path %>% load_vcf()

rosycontacts<-wide_import(file_path3) %>% wide_to_long() %>% long_to_final(rosycontacts)
# rosycontacts <- rosycontacts %>% original_to_final()

rosycontacts %>% drop_rosycontacts(your_folder)


contacts_long <- rosycontacts$original$long




rosycontacts$original %>% add_list_to_global()
# Run the application
run_app()
