# START ####
package_name<-"rosycontacts"
golem::fill_desc(
  pkg_name = package_name, # The Name of the package containing the App
  pkg_title = "Rosy Contacts", # The Title of the package containing the App
  pkg_description = "Make Better Contacts", # The Description of the package containing the App
  author_first_name = "Brandon", # Your First Name
  author_last_name = "Rose", # Your Last Name
  author_email = "thecodingdocs@gmail.com", # Your Email
  repo_url = paste0("https://github.com/brandonerose/",package_name) # The URL of the GitHub Repo (optional)
)
golem::set_golem_options()
usethis::use_mit_license("Brandon Rose") # You can set another license here
usethis::use_readme_rmd(open = FALSE)
usethis::use_code_of_conduct(contact = "Brandon Rose")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)
golem::remove_favicon() # Uncomment to remove the default favicon
golem::use_favicon("inst/app/www/logo.png") # path = "path/to/ico". Can be an online file.

# DEV ####
# golem::add_fct("helpers", with_test = TRUE)
# golem::add_utils("helpers", with_test = TRUE)
# golem::add_css_file("custom")
## Add internal datasets
## If you have data in your package
# usethis::use_pipe()
usethis::use_data_raw(name = "my_dataset", open = FALSE)
usethis::use_test("app")

usethis::use_vignette(package_name)
devtools::build_vignettes()
usethis::use_coverage()## Set the code coverage service ("codecov" or "coveralls")
# covrpage::covrpage()# Create a summary readme for the testthat subdirectory

# DOCUMENT ####
rm(list=ls(all.names = T))
cat("\014")
if(!is.null(dev.list())) dev.off()
graphics.off()
devtools::document()
attachment::att_amend_desc()
golem::detach_all_attached()
# devtools::unload()
devtools::load_all()
run_app()
#major.minor.patch.dev
#update version
attachment::att_amend_desc()

# DEPLOY ####
devtools::check()
devtools::build()
usethis::use_version()
devtools::build_readme()
usethis::use_git()
usethis::use_github(private=T)
usethis::use_github_action() # See https://usethis.r-lib.org/reference/use_github_actions.html

usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()

usethis::use_github_action_pr_commands()

#UNUSED ####

# golem::use_utils_ui(with_test = TRUE)
# golem::use_utils_server(with_test = TRUE)
## Add modules
## Create a module infrastructure in R/
# golem::add_module(name = "name_of_module1", with_test = TRUE) # Name of the module
# golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module
# usethis::use_travis()
# usethis::use_travis_badge()
# usethis::use_appveyor()
# usethis::use_appveyor_badge()
# usethis::use_circleci()
# usethis::use_circleci_badge()
# usethis::use_jenkins()
# usethis::use_gitlab_ci()
# ## Add helper functions
# ## Creates fct_* and utils_*
# ## RStudio
# ## If you want to deploy on RStudio related platforms
# golem::add_rstudioconnect_file()
# golem::add_shinyappsio_file()
# golem::add_shinyserver_file()
# ## Docker
# golem::add_dockerfile_with_renv()## If you want to deploy via a generic Dockerfile
# golem::add_dockerfile_with_renv_shinyproxy()## If you want to deploy to ShinyProxy

# BROSE_DEV ######
