library(shiny)
library(magrittr)
# Load config
library(nasbMicrotaskViewerConfig)
library(nasbMicrotaskViewerHelpers)


source('components/handlers/R/downloads.R', local=TRUE)
source('components/handlers/R/input.R', local=TRUE)


logging::basicConfig()

last_modified <- sort(sapply(list.files(), function(x) strftime(file.info(x)$mtime)), decreasing=TRUE)[1]

options(shiny.maxRequestSize=config$maxRequestSize) 

perturbations_data <- lapply(
    config$datasets,
    function(dataset) {
        force(dataset)
        shiny::reactive({ nasbMicrotaskViewerHelpers::preprocess(dataset$path, drop_duplicates=config$drop_duplicates) })
    }
)

disease_sigs <- data.table::fread(config$sigs_list_path)
disease_sigs_choices <- setNames(
    disease_sigs$file_name,
    paste(disease_sigs$disease, disease_sigs$cell_type, disease_sigs$geo_id, sep = ' | ')
)

shiny::shinyServer(function(input, output, session) {
    
    output$last_modified <- shiny::renderText({ last_modified })
    
    values <- shiny::reactiveValues(
        # Not required. Just to remind myself what is stored inside
        last_error = NULL,
        input_name = NULL,
    
        # Required
        manual_upload = TRUE,
        disease_sig_fetch_running = FALSE
    )
    
    error_handler <- shiny_error_handler(values)
    
    #' Render last error
    #'
    output$last_error <- shiny::renderText({
        values$last_error$message
    })
    
    
    source('./components/datain/R/server.R', local=TRUE)
    source('./components/chdir/R/server.R', local=TRUE)
    source('./components/paea/R/server.R', local=TRUE)
    
})
