library(shiny)
library(ggvis)
library(markdown)

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
seperator_input <- radioButtons(
    'sep', 'Separator',
    c(Comma=',', Semicolon=';', Tab='\t'),
)


datain_input_choice <- column(3, wellPanel(
    radioButtons(
        'datain_type', 'Input type',
        c('Custom expression data'='upload', 'Disease signature'='disease')
    )
))


datain_upload <- column(3, wellPanel(
    h3('Expression data'),
    uiOutput('datain_container'),
    seperator_input        
))

datain_sampleclass <- column(3, wellPanel(
    h3('Control samples', id='control_samples'),
    uiOutput('sampleclass_container')
))

datain_preprocessing <- column(3, wellPanel(
    h3('Preprocessing', id='datain_preprocessing'),
    checkboxInput(inputId='log2_transform', label='log2 transform' , value = FALSE),
    checkboxInput(inputId='quantile_normalize', label='Quantile normalize', value = FALSE),
    checkboxInput(inputId='enable_id_filter', label='Enable id filter', value = TRUE)
))

disease_input <- column(width=6, wellPanel(
    selectizeInput(
        'disease_sigs_choices', 'Choose disease signature', 
        choices = NULL, options = list(placeholder = 'type disease name')
    ),
    uiOutput('fetch_disease_sig_container')
))


datain_preview <- column(
    width=12, 
    h3('Input preview', id='datain_preview_header'),
    tabsetPanel(
        id='datain_preview',
        tabPanel(
            'Input data',
            p(textOutput('upload_message')),
            dataTableOutput('contents')
        ),
        tabPanel(
            "Plots",
            conditionalPanel(
                condition = 'output.show_datain_results === true',
                helpText(ggvis_bug_message),
                ggvisOutput('datain_density_ggvis')
            )
        )
    )
)


#' Dataset download 
#'
upload_tab <- tabPanel(
    'Upload dataset',
    fluidRow(
        column(12, p('')),
        datain_input_choice,
        conditionalPanel(
            'input.datain_type === "upload"',
            datain_upload,
            datain_sampleclass
        ),
        conditionalPanel(
            'input.datain_type === "disease"',
            disease_input
        ),
        datain_preprocessing,
        datain_preview
    )
)

#' Characteristic Direction Analysis ouput tab
#'
chdir_tab <- tabPanel(
    'Characteristic Direction Analysis',
    fluidRow(
        column(12, p('')), 
        column(4, wellPanel(
            h3('Input summary', id='chdir_input_summary'),
            tags$dl(
                tags$dt('#genes:'),
                tags$dd(textOutput('ngenes')),
                tags$dt('#probes:'),
                tags$dd(textOutput('nprobes')),
                tags$dt('Control samples:'),
                tags$dd(textOutput('control_samples')),
                tags$dt('Treatment samples:'),
                tags$dd(textOutput('treatment_samples'))
            )
        )),
   
        column(4, wellPanel(
            h3('CHDIR parameters', id='chdir_parameters'),
            sliderInput('chdir_gamma', 'Gamma', 1.0, min = 0, max = 1, step = 0.05),
            sliderInput('chdir_nnull', 'Nnull', 10, min = 2, max = 100, step = 1, round=TRUE),
            uiOutput('random_seed_container'),
            checkboxInput('set_random_seed', "Set RNG seed manually", FALSE),
            helpText(paste(
                'Significance test is using random number generator.',
                'If you want to obtain reproducible results you can set',
                'seed value.'
            )),
            uiOutput('run_chdir_container')
        )),
        
        column(4,
            wellPanel(
                h3('Downloads', id='chdir_downloads'),
                uiOutput('ngenes_tokeep_contatiner'),
                tags$dl(
                    tags$dt('#{significant upregulated genes}:'),
                    tags$dd(textOutput('n_sig_up_genes')),
                    tags$dt('#{significant downregulated genes}:'),
                    tags$dd(textOutput('n_sig_down_genes'))
                ),
                uiOutput('chdir_downloads_container')
            ),
            wellPanel(
                h3('Analyze with Enrichr'),
                radioButtons('enrichr_subset', 'Subset', c('Upregulated'='up', 'Downregulated'='down')),
                uiOutput('enrichr_form')
            )
        ),
        column(12,
            h3('CHDIR results', id='chdir_results_header'),
            tabsetPanel(
                id='chdir_results',
                tabPanel('Summary'),
                tabPanel('Plots',
                    p(textOutput('chdir_message')),
                    conditionalPanel(
                        condition = 'output.show_chdir_results === true',
                        helpText(ggvis_bug_message),
                        ggvisOutput('chdir_ggvis_plot')
                    ) 
                ),
                tabPanel(
                    'Upregulated genes',
                    dataTableOutput('chdir_up_genes_table')
                ),
                tabPanel(
                    'Downregulated genes',
                    dataTableOutput('chdir_down_genes_table')
                )
            )
        )
    )
)


#' Principle Angle Enrichment Analysis startegy input
#'
paea_strategy_radio <- radioButtons('paea_strategy', 'View:', c(
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
paea_input_parameters <-column(6, wellPanel(
    h3('PAEA parameters', id='paea_parameters'),
    checkboxInput('paea_casesensitive', 'Case Sensitive', FALSE),
    helpText('Check if you want gene lables comparisons to be case sensitive. Not recommended.'),
    uiOutput('background_dataset_container'),
    uiOutput('run_paea_container')
))

#' paea tab - downloads
#'
paea_downloads <- column(width=6, wellPanel(
    h3('Downloads', id='paea_downloads'),
    uiOutput('paea_downloads_container')
))

#' paea tab - paea summary
#'
paea_summary <- tabPanel(
    'Summary', column(12, p(textOutput('paea_message')))
)

#' paea tab - output
#'
paea_output <- column(
    width=12,
    h3('PAEA results', id='pae_results'),
    
    tabsetPanel(
        tabPanel(
            'Enriched sets',
            column(
                width=12,
                p(),
                column(
                    width=12,
                    wellPanel(
                        paea_strategy_radio,
                        imageOutput('paea_strategy_chart',  width = '100%', height='100%')
                    )
                )
            ),
            dataTableOutput('paea_results_table')
        )
    )
)


#' Principle Angle Enrichment Analysis ouput tab
#'
paea_tab <- tabPanel(
    'Principle Angle Enrichment Analysis',
    fluidRow(
        column(12, p('')),
        paea_input_parameters,
        paea_downloads,
        paea_output
    )
)


#' Data analysis tab
#'
analyze_panel <- tabPanel(
    title='Analyze',
    tabsetPanel(
        id='workflow_panel',
        upload_tab,
        chdir_tab,
        paea_tab
    )
)

#' About tab
#'
about_panel <- tabPanel(
    title = 'About',
    fluidRow(column(12,
        includeMarkdown('about.md'),
        tags$dl(
            tags$dt('Last update:'),
            tags$dd(textOutput('last_modified'))
        )
    ))
)


#' Complete UI
#'
shinyUI(
    navbarPage(
        title='NASB Microtask Viewer',
        footer=column(12),
        analyze_panel,
        about_panel,
        
        tags$head(
            includeCSS('www/css/tourist.css'),
            includeCSS('www/css/custom.css'),
            tags$script(src='js/underscore-min.js'),
            tags$script(src='js/backbone-min.js'),
            includeScript('www/js/tourist.min.js'),
            includeScript('www/js/analyze-tour.js')
        )
    )
)
