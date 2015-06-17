#' Disappearing plot help 
#' https://groups.google.com/forum/#!topic/ggvis/kQQsdn1RYaE
#'
ggvis_bug_message <- paste(
    'If you see this message but the plot is invisible please try to resize it',
    'using small grey triangle in the bottom right corner. Unfortunately it seems to be a known bug',
    'in ggvis/shiny so we\'ll have to wait for a fix.'
)


#' Characteristic Direction Analysis input summary
#'
chdir_input_summary <- shiny::wellPanel(
    shiny::h3('Input summary', id='chdir_input_summary'),
    shiny::tags$dl(
        shiny::tags$dt('#genes:'),
        shiny::tags$dd(shiny::textOutput('ngenes')),
        shiny::tags$dt('#probes:'),
        shiny::tags$dd(shiny::textOutput('nprobes')),
        shiny::tags$dt('Control samples:'),
        shiny::tags$dd(shiny::textOutput('control_samples')),
        shiny::tags$dt('Treatment samples:'),
        shiny::tags$dd(shiny::textOutput('treatment_samples'))
    )
)


#' Characteristic Direction Analysis parameters
#' 
chdir_parameters <- shiny::wellPanel(
    shiny::h3('CHDIR parameters', id='chdir_parameters'),
    shiny::sliderInput('chdir_gamma', 'Gamma', 1.0, min = 0, max = 1, step = 0.05),
    shiny::sliderInput('chdir_nnull', 'Nnull', 10, min = 2, max = 100, step = 1, round=TRUE),
    shiny::uiOutput('random_seed_container'),
    shiny::checkboxInput('set_random_seed', "Set RNG seed manually", FALSE),
    shiny::helpText(paste(
        'Significance test is using random number generator.',
        'If you want to obtain reproducible results you can set',
        'seed value.'
    )),
    shiny::actionButton(inputId = 'run_chdir', label = 'Run Characteristic Direction Analysis', icon = NULL),
    shiny::helpText(shiny::textOutput('run_chdir_help'))
)


#' Characteristic Direction Analysis results
#' 
chdir_results <- shiny::div(
    shiny::h3('CHDIR results', id='chdir_results_header'),
    shiny::tabsetPanel(
        id='chdir_results',
        shiny::tabPanel('Summary', shiny::p(shiny::textOutput('chdir_message')), shiny::uiOutput('chdir_run_summary')),
        shiny::tabPanel('Plots',
                        shiny::conditionalPanel(
                            condition = 'output.show_chdir_results === true',
                            shiny::helpText(ggvis_bug_message),
                            ggvis::ggvisOutput('chdir_ggvis_plot')
                        ) 
        ),
        shiny::tabPanel(
            'Upregulated genes',
            DT::dataTableOutput('chdir_up_genes_table')
        ),
        shiny::tabPanel(
            'Downregulated genes',
            DT::dataTableOutput('chdir_down_genes_table')
        )
    )
)


#' Characteristic Direction Analysis downloads
#' 
chdir_downloads <- shiny::wellPanel(
    shiny::h3('Downloads', id='chdir_downloads'),
    shiny::uiOutput('ngenes_tokeep_contatiner'),
    shiny::tags$dl(
        shiny::tags$dt('#{significant upregulated genes}:'),
        shiny::tags$dd(shiny::textOutput('n_sig_up_genes')),
        shiny::tags$dt('#{significant downregulated genes}:'),
        shiny::tags$dd(shiny::textOutput('n_sig_down_genes'))
    ),
    shiny::downloadButton('download_chdir', 'Download chdir'),
    shiny::downloadButton('download_chdir_up', 'Download up genes'),
    shiny::downloadButton('download_chdir_down', 'Download down genes'),
    shiny::uiOutput('chdir_downloads_message')
)


#' Characteristic Direction Analysis Enrichr
#'
chdir_enrichr <- shiny::wellPanel(
    shiny::h3('Analyze with Enrichr'),
    shiny::radioButtons('enrichr_subset', 'Subset', c('Upregulated'='up', 'Downregulated'='down')),
    
    shiny::tags$form(
        target='_blank', method='post', enctype='multipart/form-data',
        action='http://amp.pharm.mssm.edu/Enrichr/enrich',
        shiny::uiOutput('enrichr_form'),
        shiny::tags$button('Analyze with Enrichr', class='btn btn-default', id='send_to_enrichr')
    )
)


#' Characteristic Direction Analysis ouput tab
#'
chdir_tab <- shiny::tabPanel(
    'Characteristic Direction Analysis',
    shiny::fluidRow(
        shiny::column(width=12, shiny::p('')), 
        shiny::column(width=4, chdir_input_summary),
        shiny::column(width=4, chdir_parameters),
        shiny::column(
            width=4, 
            chdir_downloads,
            chdir_enrichr
        ),
        shiny::column(width=12, chdir_results)
    )
)


