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
            DT::dataTableOutput('contents')
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

