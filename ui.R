library(shiny)

#' Disappearing plot help 
#' https://groups.google.com/forum/#!topic/ggvis/kQQsdn1RYaE
#'
ggvis_bug_message <- paste(
    'If you see this message but the plot is invisible please try to resize it',
    'using small grey triangle in the bottom right corner. Unfortunately it seems to be a known bug',
    'in ggvis/shiny so we\'ll have to wait for a fix.'
)

#' Input separator choice
#'
datain <- shiny::radioButtons(
    'sep', 'Separator',
    c(Comma=',', Semicolon=';', Tab='\t'),
)


datain_input_choice <- shiny::wellPanel(
    shiny::radioButtons(
        'datain_type', 'Input type',
        c('Custom expression data'='upload', 'Disease signature'='disease')
    )
)


datain_start_tour <- shiny::wellPanel(
    shiny::helpText(
        shiny::p('Not sure where to start?', shiny::a('Begin a guided tutorial tour', href='#', id='start_tour'))
    )
)

datain_upload <-shiny::wellPanel(
    h3('Expression data'),
    shiny::uiOutput('datain_container'),
    datain        
)


datain_sampleclass <- shiny::wellPanel(
    h3('Control samples', id='control_samples'),
    shiny::uiOutput('sampleclass_container'),
    shiny::helpText(shiny::textOutput('sampleclass_message'))
)


datain_preprocessing <- shiny::wellPanel(
    shiny::h3('Preprocessing', id='datain_preprocessing'),
    shiny::checkboxInput(inputId='log2_transform', label='log2 transform' , value = FALSE),
    shiny::checkboxInput(inputId='quantile_normalize', label='Quantile normalize', value = FALSE),
    shiny::checkboxInput(inputId='enable_id_filter', label='Enable id filter', value = TRUE)
)


datain_disease_input <- shiny::wellPanel(
    shiny::selectizeInput(
        'disease_sigs_choices', 'Choose disease signature', 
        choices = NULL, options = list(placeholder = 'type disease name')
    ),
    shiny::actionButton('fetch_disease_sig', 'Fetch signature')
)


datain_preview <- shiny::div(
    shiny::h3('Input preview', id='datain_preview_header'),
    shiny::tabsetPanel(
        id='datain_preview',
        shiny::tabPanel(
            'Input data',
            shiny::p(shiny::textOutput('upload_message')),
            shiny::dataTableOutput('contents')
        ),
        shiny::tabPanel(
            "Plots",
            shiny::conditionalPanel(
                condition = 'output.show_datain_results === true',
                shiny::helpText(ggvis_bug_message),
                ggvis::ggvisOutput('datain_density_ggvis')
            )
        )
    )
)


#' Dataset download 
#'
datain_tab <- shiny::tabPanel(
    'Upload dataset',
    shiny::fluidRow(
        shiny::column(width=12, p('')),
        shiny::column(
            width=3,
            datain_input_choice,
            datain_start_tour
        ),
        shiny::conditionalPanel(
            'input.datain_type === "upload"',
            shiny::column(width=3, datain_upload),
            shiny::column(width=3, datain_sampleclass)
        ),
        shiny::conditionalPanel(
            'input.datain_type === "disease"',
            shiny::column(width=6, datain_disease_input)
        ),
        shiny::column(width=3, datain_preprocessing),
        shiny::column(width=12, datain_preview)
    )
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
            shiny::dataTableOutput('chdir_up_genes_table')
        ),
        shiny::tabPanel(
            'Downregulated genes',
            shiny::dataTableOutput('chdir_down_genes_table')
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
    shiny::uiOutput('paea_downloads_container')
)


#' paea tab - paea summary
#'
paea_summary <- shiny::tabPanel(
    'Summary', shiny::column(width=12, shiny::p(shiny::textOutput('paea_message')), shiny::uiOutput('paea_run_summary'))
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


#' Data analysis tab
#'
analyze_panel <- shiny::tabPanel(
    title='Analyze',
    shiny::tabsetPanel(
        id='workflow_panel',
        datain_tab,
        chdir_tab,
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
        title='NASB Microtask Viewer',
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
