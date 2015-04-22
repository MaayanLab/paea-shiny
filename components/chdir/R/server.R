#' chdir panel - is input valid
#'
chdir_input_valid <- shiny::reactive({
    datain_valid() && length(samples()$control) > 1 && length(samples()$treatment) > 1
})


#' chdir panel - enable/disable run button
#'
shiny::observe({
    if(!chdir_input_valid()) {
        disableButton(id='#run_chdir', session=session)
    } else {
        enableButton(id='#run_chdir', session=session)
    }
})


#' chdir panel - run button help message
#'
output$run_chdir_help <- shiny::renderText({
    if (is.null(datain())) {
        'Upload your dataset and select control samples first.'
    } else if ( length(samples()$control) < 2 | length(samples()$treatment) < 2) {
        'You need at least two control and two treatment samples to run chdir.'
    }
})


#' chdir panel - random number generator seed 
#'
output$random_seed_container <- shiny::renderUI({
    rng_seed_input <- shiny::numericInput(
        inputId='random_seed',
        label='Random  number generator seed',
        value=as.integer(Sys.time())
    ) 
    if(!input$set_random_seed){
        disabledNumericInput(rng_seed_input)
    } else {
        rng_seed_input
    }
})

#' chdir panel - number of probes
#'
output$nprobes <- shiny::renderText({
    if(datain_valid()) {
        nrow(datain())
    }
})


#' chdir panel - number of genes
#' 
output$ngenes <- shiny::renderText({
    if(datain_valid()) {
        datain() %>% extract2(1) %>% unique() %>% length() 
    }
})


#' chdir panel - control samples
#'
output$control_samples <- shiny::renderText({
    paste(samples()$control, collapse = ', ')
})


#' chdir panel - treatment samples
#'
output$treatment_samples <- shiny::renderText({
    paste(samples()$treatment, collapse = ', ')
})


#' Characteristic Direction Analysis trigger
#'
chdir_trigger <- shiny::reactive({
    if(is.null(input$run_chdir) || input$run_chdir == 0) { return() }
    input$run_chdir
})
 

#' Prepare Characteristic Direction Analysis params
#'
chdir_params <- shiny::reactive({
    if(is.null(chdir_trigger())) { return() }
    
    shiny::isolate(
        list(
            manual_upload=values$manual_upload,
            input_name=values$input_name,
            control_samples=samples()$control,
            treatment_samples=samples()$treatment,
            log2_transform=input$log2_transform,
            quantile_normalize=input$quantile_normalize,
            enable_id_filter=input$enable_id_filter,
            gamma=input$chdir_gamma,
            nnull=min(as.integer(input$chdir_nnull), 1000),
            seed=input$random_seed
        )
    )
})


#' Run Characteristic Direction Analysis
#'

chdir <- shiny::reactive({
    params <- chdir_params()
    
    if(is.null(params)) { return() }
    
    datain <- shiny::isolate(datain_preprocessed())
    sampleclass <- factor(ifelse(colnames(datain) %>% extract(-1) %>% is_in(params$control), '1', '2'))
    set.seed(params$seed)
    
    disableButton('#run_chdir', session)
    tryCatch(
        chdir_analysis_wrapper(preprocess_chdir_input(datain), sampleclass, params$gamma, params$nnull),
        error = error_handler,
        finally = enableButton('#run_chdir', session)
    )  
})


#' chdir tab - set plots visibility
#'
output$show_chdir_results <- shiny::reactive({ !is.null(chdir()) })
shiny::outputOptions(output, 'show_chdir_results', suspendWhenHidden = FALSE)


#' Plot top genes from Characteristic Direction Analysis
#'
shiny::observe({
    # Not as reactive as it should be
    # https://groups.google.com/forum/#!topic/ggvis/kQQsdn1RYaE
    
    chdir <- chdir()
    
    if(!is.null(chdir)) {
        chdir$results %>%
            extract2(1) %>%
            prepare_results() %>%
            plot_top_genes() %>%
            ggvis::bind_shiny('chdir_ggvis_plot')
    }
})

#' chdir panel number of significant genes to keep
#' 
output$ngenes_tokeep_contatiner <- shiny::renderUI({
    limit <- if(!is.null(chdir())) {
        #' Number of genes in chdir output
        chdir() %>%
            chdir_results() %>%
            length() %>%
            #' All or fraction
            multiply_by(c(1, config$max_fgenes_tokeep)) %>%
            c(config$max_ngenes_tokeep) %>% 
            #' Overall min
            min()
    } else {
        config$max_ngenes_tokeep
    }

    shiny::sliderInput(
        'ngenes_tokeep', label='Limit number of genes to return',
        min=1, max=limit, step=1, value=ceiling(limit / 2), round=TRUE
    )
})



#' chdir panel - download block
#'
output$chdir_downloads_message <- shiny::renderText({
    if (is.null(chdir())) {
        'No data available. Did you run CHDIR analysis?'
    }
})


#' chdir panel - number of significant upregulated genes
#'
output$n_sig_up_genes <- shiny::renderText({
    if(!is.null(chdir())) {
        nrow(chdir_up_genes())
    }
})

#' chdir panel - number of significant downregulated genes
#'
output$n_sig_down_genes <- shiny::renderText({
    if(!is.null(chdir())) {
        nrow(chdir_down_genes())
    }
})


#' chdir panel - chdir download
#'
output$download_chdir <- shiny::downloadHandler(
    filename = 'chdir.tsv',
    content = chdir_download_handler(prepare_results(chdir() %>% chdir_props() %>% extract(, 1)))
)


#' chdir panel - prepare down genes
#'
chdir_up_genes <- shiny::reactive({
    if(!is.null(chdir())) {
        head(prepare_up_genes(chdir() %>% chdir_results()), input$ngenes_tokeep)
    }
})


#' chdir panel - prepare up genes
#'
chdir_down_genes <- shiny::reactive({
    if(!is.null(chdir())) {
        head(prepare_down_genes(chdir() %>% chdir_results()), input$ngenes_tokeep)
    }
})


#' chdir panel - chdir download
#'
output$download_chdir_up <- shiny::downloadHandler(
    filename = 'chdir_up_genes.tsv',
    content = chdir_download_handler(chdir_up_genes())

)

#' chdir panel - chdir download
#'
output$download_chdir_down <- shiny::downloadHandler(
    filename = 'chdir_up_genes.tsv',
    content = chdir_download_handler(chdir_down_genes())
)

#' chdir panel - up genes table
#'
output$chdir_up_genes_table <- shiny::renderDataTable({
    if(!is.null(chdir())) {
        chdir_up_genes() %>% dplyr::rename(Gene = g, 'Characteristic Direction Coefficient' = v)
    }
})


#' chdir panel - down genes table
#'
output$chdir_down_genes_table <- shiny::renderDataTable({
    if(!is.null(chdir())) {
        chdir_down_genes() %>% dplyr::rename(Gene = g, 'Characteristic Direction Coefficient' = v)
    }
})

    
#' chdir panel - status message
#'
output$chdir_message <- shiny::renderText({
    if(is.null(chdir())) {
        'results not available...'
    }
})


#' Prepare lists of genes in a format expected by the Enrichr
#'
enrichr_input <- shiny::reactive({
    if(is.null(chdir())) {
        list(up='', down='')
    } else {
        list(up=chdir_up_genes(), down=chdir_down_genes()) %>% lapply(prepare_enrichr_input)
    }
})


#' chdir panel - Enrichr submit form
#'
output$enrichr_form <- shiny::renderUI({
    #' Prepare description string for Enrichr
    description <- paste(values$input_name, ' (', input$enrichr_subset, ')', sep='')
    
    list(
        shiny::tags$input(name='list', type='hidden', value=enrichr_input()[[input$enrichr_subset]]),
        shiny::tags$input(name='description', type='hidden', value=description)
    )
})


#' chdir panel - run summary
#'

output$chdir_run_summary <- shiny::renderUI({
    if(!is.null(chdir())) {
        list_to_defs(chdir_params())
    }
})


shiny::observe({
    chdir_consumers <- c(
        '#run_paea', '#send_to_enrichr', '#download_chdir',
        '#download_chdir_up', '#download_chdir_down'
    )

    if (is.null(chdir()))  {
        lapply(chdir_consumers, disableButton, session=session)
    } else {
        lapply(chdir_consumers, enableButton, session=session)
    }

})
