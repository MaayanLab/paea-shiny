#' paea panel - run button
#'
output$run_paea_message <- shiny::renderText({
    if(is.null(chdir())) {
        'Before you can run PAEA you have to execute CHDIR analysis.'
    }
})


#' Choose background dataset
#'
output$background_dataset_container <- shiny::renderUI({
    datasets <- names(perturbations_data)
    shiny::radioButtons(
        'background_dataset', 'Background',
        setNames(datasets, stringi::stri_trans_totitle(datasets))
    )
})


#' Trigger execution of paea
#'
paea_trigger <- shiny::reactive({
     if(input$run_paea == 0 || is.null(shiny::isolate(chdir()))) { return() }
     return(input$run_paea)
})


#' Prepare Principle Angle Enrichment Analysis parameters
#'
paea_params <- shiny::reactive({
    if(is.null(paea_trigger())) { return() }
    shiny::isolate({
        append(
            list(
                casesensitive=input$paea_casesensitive,
                background_dataset=input$background_dataset
            ),
            chdir_params()
        )
    })
})


#' Run Principle Angle Enrichment Analysis
#'
paea <- shiny::reactive({
    paea_params <- paea_params()
    
    if(is.null(paea_params)) { return() }
    
    disableButton('#run_paea', session)
    
    tryCatch(
        shiny::withProgress(message = 'Running PAEA', value = 0, {        
            lapply(paea_analysis_dispatch(
                chdirresults=shiny::isolate(chdir())$chdirprops,
                gmtfile=prepare_gene_sets(perturbations_data[[paea_params$background_dataset]]()$genes),
                casesensitive=paea_params$casesensitive,
                with_progress=TRUE
            ), paea_to_df)
            
        }),
        error = error_handler,
        finally = enableButton('#run_paea', session)
    )
})


#' PAEA results
#' 
paea_results <- shiny::reactive({
    if(!is.null(paea())) {
        description <- perturbations_data[[paea_params()$background_dataset]]()$description
        prepare_paea_results(combine_results(paea(), input$paea_strategy), description)
    }
})


#' PAEA output
#'
output$paea_results_table <- shiny::renderDataTable({
    if(!is.null(paea_results())) {
        paea_results()
    }
})

#' paea panel - control download button status
#'
shiny::observe({
    if(is.null(paea())) {
        disableButton('#download_paea', session)
    } else {
        enableButton('#download_paea', session)
    }
})


#' paea panel - download message
#'
output$paea_downloads_message <- shiny::renderText({
    if (is.null(paea())) {
        'No data available. Did you run PAEA analysis?'
    }
})


#' paea panel - downloads handler
#'
output$download_paea <- shiny::downloadHandler(
    filename = 'paea_results.tsv',
    content = paea_download_handler(paea_results())
)


#' paea panel - status message
#'
output$paea_message <- shiny::renderText({
    if(is.null(paea())) {
        'results not available...'
    }
})


#' paea panel - run summary
#'
output$paea_run_summary <- shiny::renderUI({
    if(!is.null(paea())) {
        list_to_defs(paea_params())
    }
})
