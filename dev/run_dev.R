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

# Run the application
run_app()
