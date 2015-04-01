library(shiny)
library(magrittr)
library(nasbMicrotaskViewerHelpers)

source('downloads_handlers.R', local=TRUE)
source('config.R', local=TRUE)

logging::basicConfig()

last_modified <- sort(sapply(list.files(), function(x) strftime(file.info(x)$mtime)), decreasing=TRUE)[1]

options(shiny.maxRequestSize=120*1024^2) 

perturbations_data <- lapply(
    config$data_paths,
    function(data_path) {
        force(data_path)
        shiny::reactive({ nasbMicrotaskViewerHelpers::preprocess(data_path, drop_duplicates=config$drop_duplicates) })
    }
)

disease_sigs <- data.table::fread(config$sigs_list_path)
disease_sigs_choices <- setNames(
    disease_sigs$file_name,
    paste(disease_sigs$disease, disease_sigs$cell_type, disease_sigs$geo_id, sep = ' | ')
)

shiny::shinyServer(function(input, output, session) {
    
    disableButton <- function(id, session) {
        session$sendCustomMessage('button_status_message', list(id=id, disable=TRUE))
    }
    
    
    enableButton <- function(id, session) {
        session$sendCustomMessage('button_status_message', list(id=id, disable=FALSE))
    }
    
    
    output$last_modified <- shiny::renderText({ last_modified })
    
    values <- shiny::reactiveValues(
        # Not required. Just to remind myself what is stored inside
        chdir = NULL,
        control_samples = NULL,
        treatment_samples = NULL,
        last_error = NULL,
        chdir_params = NULL,
        input_name = NULL,
    
        # Required
        paea_running = FALSE,
        manual_upload = TRUE,
        disease_sig_fetch_running = FALSE
    )
    
    error_handler <- shiny_error_handler(values)
    
    #' Render last error
    #'
    output$last_error <- shiny::renderText({
        values$last_error$message
    })
    
    
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
    
    
    #' datain panel - control/treatment samples checboxes
    #'
    output$sampleclass_container <- shiny::renderUI({
        datain_valid <- datain_valid()
        
        if (!values$manual_upload) {
            shiny::helpText('You can choose samples only for manually uploaded data sets.')
        } else if (datain_valid) {
           shiny::checkboxGroupInput(
                'sampleclass',
                'Choose control samples',
                colnames(datain())[-1]
            )
        } else {
            #' Manual upload and invalid data
            shiny::helpText(attributes(datain_valid)$message)
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
        
        plot_density(datain_preprocessed()) %>% 
            ggvis::bind_shiny('datain_density_ggvis')
    })
    
    
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
            length(unique(datain()[[1]]))
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
        sampleclass <- factor(ifelse(colnames(datain)[-1] %in% params$control, '1', '2'))
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
            results <- prepare_results(chdir$results[[1]])
            plot_top_genes(results) %>% ggvis::bind_shiny('chdir_ggvis_plot')
        }
    })
    
    #' chdir panel number of significant genes to keep
    #' 
    output$ngenes_tokeep_contatiner <- shiny::renderUI({
        slider <- shiny::sliderInput(
            'ngenes_tokeep', label='Limit number of genes to return',
            min=1, max=config$max_ngenes_tokeep, step=1, value=100, round=TRUE
        )
        
        chdir <- chdir()
        
        if(!is.null(chdir)) {
            ngenes <- length(chdir$results[[1]])
            limit <- min(config$max_fgenes_tokeep * ngenes, min(config$max_ngenes_tokeep, ngenes))
            slider$children[[2]]$attribs['data-max'] <- limit
            slider$children[[2]]$attribs['data-from'] <- ceiling(limit / 2)
        }
        slider
    })
    
    
    #' chdir panel - download block
    #'
    output$chdir_downloads_container <- shiny::renderUI({
        buttons <- list(
            shiny::downloadButton('download_chdir', 'Download chdir'),
            shiny::downloadButton('download_chdir_up', 'Download up genes'),
            shiny::downloadButton('download_chdir_down', 'Download down genes')
        ) 
        if (is.null(chdir())) {
            append(
                lapply(buttons, disabledActionButton),
                list(shiny::helpText('No data available. Did you run CHDIR analysis?'))
            )
        } else {
            buttons
        }
    })
    
    #' See coment for run_chdir_container
    #'
    shiny::outputOptions(output, 'chdir_downloads_container', suspendWhenHidden = FALSE)
    

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
        content = chdir_download_handler(prepare_results(chdir()$chdirprops$chdir[[1]][, 1]))
    )
    
    
    #' chdir panel - prepare down genes
    #'
    chdir_up_genes <- shiny::reactive({
        if(!is.null(chdir())) {
            head(prepare_up_genes(chdir()$results[[1]]), input$ngenes_tokeep)
        }
    })
    
    
    #' chdir panel - prepare up genes
    #'
    chdir_down_genes <- shiny::reactive({
        if(!is.null(chdir())) {
            head(prepare_down_genes(chdir()$results[[1]]), input$ngenes_tokeep)
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
    
    
    #' chdir panel - Enrichr submit form
    #'
    output$enrichr_form <- shiny::renderUI({
        button <-shiny::tags$button('Analyze with Enrichr', class='btn btn-default')
        chdir_diff_genes <- list(up=chdir_up_genes, down=chdir_down_genes)
        value <- if(is.null(chdir())) { '' } else {
            chdir_diff_genes[[input$enrichr_subset]]() %>% prepare_enrichr_input() 
        }
        
        #' Prepare description string for Enrichr
        description <- paste(values$input_name, ' (', input$enrichr_subset, ')', sep='')
        
        shiny::tags$form(
            target='_blank', method='post', enctype='multipart/form-data',
            action='http://amp.pharm.mssm.edu/Enrichr/enrich',
            shiny::tags$input(name='list', type='hidden', value=value),
            shiny::tags$input(name='description', type='hidden', value=description),
            if(is.null(chdir())) { disabledActionButton(button) } else { button }
        )
    })
    
    
    #' chdir panel - run summary
    #'
    
    output$chdir_run_summary <- shiny::renderUI({
        if(!is.null(chdir())) {
            list_to_defs(chdir_params())
        }
    })
    
    #' paea panel - run button
    #'
    output$run_paea_container <- shiny::renderUI({
        button <- shiny::actionButton(inputId = 'run_paea', label = 'Run Principle Angle Enrichment', icon = NULL)
        if (values$paea_running) {
           list(disabledActionButton(button))
        } else if(is.null(chdir())) {
            list(
                list(disabledActionButton(button)),
                shiny::helpText('Before you can run PAEA you have to execute CHDIR analysis.')
            )
        } else {
            list(button)
        }
    })
    
    
    #' See coment for run_chdir_container
    #'
    shiny::outputOptions(output, 'run_paea_container', suspendWhenHidden = FALSE)
    
    
    #' Choose background dataset
    #'
    output$background_dataset_container <- shiny::renderUI({
        datasets <- names(perturbations_data)
        shiny::radioButtons(
            'background_dataset', 'Background',
            setNames(datasets, stringi::stri_trans_totitle(datasets))
        )
    })
    
    #' paea panel - workflow flowchart
    #'
    output$paea_strategy_chart <- shiny::renderImage(
        list(src=file.path('www/img', paste(input$paea_strategy, 'png', sep='.')), contentType='image/png'),
        deleteFile=FALSE
    )

        
    #' Prepare Principle Angle Enrichment Analysis parameters
    #'
    shiny::observe({
        if(is.null(input$run_paea) || input$run_paea == 0 || is.null(shiny::isolate(chdir()))) { return() }
        
        values$paea_params <- append(
            list(
                casesensitive=shiny::isolate(input$paea_casesensitive),
                background_dataset=shiny::isolate(input$background_dataset)
            ),
            chdir_params()
        )
    })
    
    
    #' Run Principle Angle Enrichment Analysis
    #'
    shiny::observe({
        paea_params <- values$paea_params 
        if(is.null(paea_params)) { return() }
        values$paea_running <- TRUE
        
        values$paea <- tryCatch(
            shiny::withProgress(message = 'Running PAEA', value = 0, {        
                lapply(paea_analysis_dispatch(
                    chdirresults=shiny::isolate(chdir())$chdirprops,
                    gmtfile=prepare_gene_sets(perturbations_data[[paea_params$background_dataset]]()$genes),
                    casesensitive=paea_params$casesensitive,
                    with_progress=TRUE
                ), paea_to_df)
                
            }),
            error = error_handler,
            finally = { values$paea_running <- FALSE }
        )
    })
    
    
    #' PAEA results
    #' 
    paea_results <- shiny::reactive({
        if(!is.null(values$paea)) {
            description <- perturbations_data[[values$paea_params$background_dataset]]()$description
            prepare_paea_results(combine_results(values$paea, input$paea_strategy), description)
        }
    })
    
    
    #' PAEA output
    #'
    output$paea_results_table <- shiny::renderDataTable({
        if(!is.null(paea_results())) {
            paea_results()
        }
    })
    
    
    #' paea panel - download block
    #'
    output$paea_downloads_container <- shiny::renderUI({
        button <- shiny::downloadButton('download_paea', 'Download current view')
        
        if (is.null(values$paea)) {
            list(
                disabledActionButton(button),
                shiny::helpText('No data available. Did you run PAEA analysis?')
            )
        } else {
            list(button)
        }
    })
    
    
    #' See coment for run_chdir_container
    #'
    shiny::outputOptions(output, 'paea_downloads_container', suspendWhenHidden = FALSE)
    
    
    #' paea panel - downloads handler
    #'
    output$download_paea <- shiny::downloadHandler(
        filename = 'paea_results.tsv',
        content = paea_download_handler(paea_results())
    )
    
    
    #' paea panel - status message
    #'
    output$paea_message <- shiny::renderText({
        if(is.null(values$paea)) {
            'results not available...'
        }
    })
    
    
    #' chdir panel - run summary
    #'
    output$paea_run_summary <- shiny::renderUI({
        if(!is.null(values$paea)) {
            list_to_defs(values$paea_params)
        }
    })
    
})
