logging::basicConfig()
options(shiny.maxRequestSize=config$maxRequestSize) 

# Required libraries
library(magrittr)

# Load config and helpers
library(nasbMicrotaskViewerConfig)
library(nasbMicrotaskViewerHelpers)

# Load components
source('components/handlers/R/downloads.R', local=TRUE)
source('components/handlers/R/input.R', local=TRUE)
source('components/handlers/R/dataset_loaders.R', local=TRUE)

# Load data
last_modified <- get_last_modified()
perturbations_data <- load_perturbations_data(config$datasets)
disease_sigs <- load_disease_sigs(config$sigs_list_path)

shiny::shinyServer(function(input, output, session) {
    
    output$last_modified <- shiny::renderText({ last_modified })
    
    #' Init reactive values
    values <- shiny::reactiveValues(
        # Not required. Just to remind myself what is stored inside
        last_error = NULL,
        input_name = NULL,
    
        # Required
        manual_upload = TRUE,
        disease_sig_fetch_running = FALSE
    )
    
    #' Setup error handler
    error_handler <- shiny_error_handler(values)
    
    #' Render last error
    #'
    output$last_error <- shiny::renderText({
        values$last_error$message
    })
    
    #' Load components   
    source('./components/datain/R/server.R', local=TRUE)
    source('./components/chdir/R/server.R', local=TRUE)
    source('./components/paea/R/server.R', local=TRUE)
    
})
