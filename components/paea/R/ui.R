library(nasbMicrotaskViewerConfig)

#' Principle Angle Enrichment Analysis startegy input
#'
paea_strategy_radio <- shiny::radioButtons('paea_strategy', 'View:', c(
    'in↑ ∩ lib↑' = '+up_up',
    'in↓ ∩ lib↓' = '+down_down',
    'in↑ ∩ lib↓' = '+up_down',
    'in↓ ∩ lib↑' = '+down_up', 
    'in↑ ∩ lib↑ + in↓ ∩ lib↓' = '+up_up+down_down',
    'in↑ ∩ lib↓ + in↓ ∩ lib↑' = '+up_down+down_up',
    'in↑ ∩ lib↑ + in↓ ∩ lib↓ - in↑ ∩ lib↓ - in↓ ∩ lib↑' = '+up_up+down_down-up_down-down_up',
    'in↑ ∩ lib↓ + in↓ ∩ lib↑ - in↑ ∩ lib↑ - in↓ ∩ lib↓' = '+up_down+down_up-up_up-down_down'
))


#' paea tab - input parameters
#'
paea_input_parameters <- shiny::wellPanel(
    shiny::h3('PAEA parameters', id='paea_parameters'),
    shiny::checkboxInput('paea_casesensitive', 'Case Sensitive', FALSE),
    shiny::helpText('Check if you want gene lables comparisons to be case sensitive. Not recommended.'),
    shiny::uiOutput('background_dataset_container'),
    shiny::actionButton(inputId = 'run_paea', label = 'Run Principle Angle Enrichment', icon = NULL),
    shiny::helpText(shiny::textOutput('run_paea_message'))
)


#' paea tab - downloads
#'
paea_downloads <- shiny::wellPanel(
    shiny::h3('Downloads', id='paea_downloads'),
    shiny::downloadButton('download_paea', 'Download current view'),
    shiny::helpText(shiny::textOutput('paea_downloads_message'))
)


#' paea tab - paea summary
#'
paea_summary <- shiny::tabPanel(
    'Summary', shiny::column(
        width=12,
        shiny::p(shiny::textOutput('paea_message')),
        shiny::uiOutput('paea_run_summary')
    )
)


#' paea tab - output
#'
paea_output <- shiny::div(
    shiny::h3('PAEA results', id='pae_results'),
    
    shiny::tabsetPanel(
        paea_summary,
        shiny::tabPanel(
            'Enriched sets',
            shiny::p(),
            shiny::wellPanel(
                paea_strategy_radio
            ),
            shiny::dataTableOutput('paea_results_table')
        )
    )
)


#' Principle Angle Enrichment Analysis ouput tab
#'
paea_tab <- shiny::tabPanel(
    'Principle Angle Enrichment Analysis',
    shiny::fluidRow(
        shiny::column(width=12, shiny::p('')),
        shiny::column(width=6, paea_input_parameters),
        shiny::column(width=6, paea_downloads),
        shiny::column(width=12, paea_output)
    )
)


