library(shiny)
# Load config
library(nasbMicrotaskViewerConfig)
devtools::load_all('./components/datain/')
source('./components/chdir/R/ui.R', local=TRUE)
source('./components/paea/R/ui.R', local=TRUE)

#' Data analysis tab
#'
analyze_panel <- shiny::tabPanel(
    title='Analyze',
    shiny::tabsetPanel(
        id='workflow_panel',
        app.datain::datain_tab,
        app.chdir::chdir_tab,
        paea_tab
    )
)

#' About tab
#'
about_panel <- shiny::tabPanel(
    title = 'About',
    shiny::fluidRow(shiny::column(width=12,
        shiny::includeMarkdown('about.md'),
        shiny::tags$dl(
            shiny::tags$dt('Last update:'),
            shiny::tags$dd(shiny::textOutput('last_modified'))
        )
    ))
)


#' Complete UI
#'
shiny::shinyUI(
    shiny::navbarPage(
        theme='css/bootstrap.min.css',
        title=config$app_name,
        header=shiny::singleton(shiny::tags$head(
            shiny::includeCSS('www/css/tourist.css'),
            shiny::includeCSS('www/css/custom.css'),
            shiny::tags$script(src='js/underscore-min.js'),
            shiny::tags$script(src='js/backbone-min.js'),
            shiny::tags$script(src='js/button-status-handler.js'),
            shiny::includeScript('www/js/tourist.min.js'),
            shiny::includeScript('www/js/analyze-tour.js')
        )),
        footer=shiny::column(width=12),
        analyze_panel,
        about_panel
    )
)
