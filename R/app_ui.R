#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboardPlus::dashboardPage(
      options = list(
        sidebarExpandOnHover = F
      ),
      header = shinydashboardPlus::dashboardHeader(
        # fixed = F,
        title = tagList(
          span(class = "logo-lg", .packageName),
          tags$a(
            href="https://thecodingdocs.com",
            target="_blank",
            tags$img(src = "www/logo.png", width="100%")
          )
        )
      ),
      sidebar = shinydashboardPlus::dashboardSidebar(
        minified = F,
        collapsed = T,

        TCD_SBH(),
        TCD_SBF()
      ),
      body = shinydashboard::dashboardBody(
        lapply(1:20, box, width = 12, title = "box")
        # TCD_SF()
      ),
      controlbar = shinydashboardPlus::dashboardControlbar(
        TCD_SBH(),
        div(style="text-align:center",p(paste0('Version: ',pkg_version))),
        div(style="text-align:center",p(paste0('Last Updated: ',pkg_date))),
        TCD_SBF(),
        fluidRow(
          column(
            12,
            p("This app is still in development."),
            p("Consider donating for more."),
            p("Contact with issues."),
            p("Consider using R package."),
            align="center"
          )
        )
      ),
      title = "DashboardPage",
      footer = TCD_NF()
    )
  )
}

#' Add external Resources to the Application
#' This function is internally used to add external
#' resources inside the Shiny application.
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = .packageName
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
