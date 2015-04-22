#' datain panel - ugly hack to be able to clear upload widget
#' 
output$datain_container <- shiny::renderUI({
    shiny::fileInput(
        'datain', 'Choose file to upload',
        accept = c(
            'text/csv', 'text/comma-separated-values',
            'text/tab-separated-values', 'text/plain',
            '.csv', '.tsv'
        )
    )
})


#'  datain panel - upload observer
#'
shiny::observe({
    if (is.null(input$datain)) return()
    
    datain <- input$datain
    
    values$manual_upload <- TRUE
    values$datapath <- datain$datapath
    
    values$input_name <- datain$name
})


#' datain panel - disease signature choice
#'
shiny::updateSelectizeInput(
    session, 'disease_sigs_choices',
    choices = disease_sigs_choices, server = TRUE
)


#' datain panel - disease sig observe
#'
shiny::observe({
    if(is.null(input$fetch_disease_sig) || input$fetch_disease_sig == 0) { return() }
    if(input$disease_sigs_choices == '') return()
    
    disableButton(id='#fetch_disease_sig', session=session)
    
    tryCatch({

            values$input_name <- shiny::isolate(input$disease_sigs_choices)
            values$datapath <- get_path(config$sigs_path, isolate(values$input_name))
            
            values$manual_upload <- FALSE
            
        },
        error = error_handler,
        finally = enableButton(id='#fetch_disease_sig', session=session)
    )
})


#' Read input data
#'
datain <- shiny::reactive({
    if(!is.null(values$datapath) && file.exists(values$datapath) && !file.info(values$datapath)$isdir) {
        tryCatch(
            if(values$manual_upload) {
                data.table::fread(values$datapath, sep=input$sep) 
            } else {
                data.table::fread(values$datapath) %>% dplyr::select(-ID_REF)
            },
            error = error_handler
        )
    }
})


#' Is input valid?
#'
datain_valid <- shiny::reactive({ datain_is_valid(datain()) })


id_filter <- reactive({
    if(input$enable_id_filter) { config$id_filter }
})


#' Apply preprocessing steps to datain
#'
#'
datain_preprocessed <- shiny::reactive({
    if(datain_valid()) {
        
        datain_preprocess(
            datain=datain(),
            log2_transform=input$log2_transform,
            quantile_normalize=input$quantile_normalize,
            id_filter=id_filter()
        )
        
    } else {
        datain()
    }
})

#' datain - input data preview
#'
output$contents <- shiny::renderDataTable({
    datain_preprocessed()
})


#' datain panel - control/treatment samples checkboxes
#'
output$sampleclass_container <- shiny::renderUI({
    if (values$manual_upload && datain_valid()) {
       shiny::checkboxGroupInput(
            'sampleclass',
            'Choose control samples',
            colnames(datain())[-1]
        )
    }
})


#' datain panel - control/treatment samples help message
#'
output$sampleclass_message <- shiny::renderText({
    if (!values$manual_upload) {
       'You can choose samples only for manually uploaded data sets.'
    }  else if (!datain_valid()) {
        #' Manual upload and invalid data
        attributes(datain_valid())$message
    }
})


#' Update lists of control/treatment samples
#'
samples <- shiny::reactive({
    if(values$manual_upload && datain_valid()) {
        list(
            control=input$sampleclass,
            treatment = setdiff(colnames(datain())[-1], input$sampleclass)
        )
    } else if (!values$manual_upload && !is.null(values$input_name)) {
        sig <- disease_sigs %>% dplyr::filter(file_name == values$input_name)
        list(
            control=unlist(stringi::stri_split_fixed(sig$ctrl_ids, ',')),
            treatment=unlist(stringi::stri_split_fixed(sig$pert_ids, ','))
        )
    } 
})


#' datain  panel - status message
#'
output$upload_message <- shiny::renderText({
    if(is.null(datain())) {
        'preview not available...'
    }
})


#' datain tab - set plots visibility
#'
output$show_datain_results <- shiny::reactive({ datain_valid() })
shiny::outputOptions(output, 'show_datain_results', suspendWhenHidden = FALSE)


#' datain tab - density plot
#'
shiny::observe({
    if(!datain_valid()) { return() }
    
    datain_preprocessed() %>% 
        prepare_density_plot_input() %>%
        plot_density() %>% 
        ggvis::bind_shiny('datain_density_ggvis')
})

