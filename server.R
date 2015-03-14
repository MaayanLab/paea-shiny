library(shiny)
library(magrittr)
library(nasbMicrotaskViewerHelpers)

source('downloads_handlers.R', local=TRUE)
source('utils.R', local=TRUE)
source('config.R', local=TRUE)

last_modified <- sort(sapply(list.files(), function(x) strftime(file.info(x)$mtime)), decreasing=TRUE)[1]

options(shiny.maxRequestSize=120*1024^2) 

perturbations_data <- lapply(
    config$data_paths,
    nasbMicrotaskViewerHelpers::preprocess,
    drop_duplicates=config$drop_duplicates
)

disease_sigs <- data.table::fread('data/disease_signatures.csv')
disease_sigs_choices <- setNames(
    disease_sigs$file_name,
    paste(disease_sigs$disease, disease_sigs$cell_type, disease_sigs$geo_id, sep = ' | ')
)

shiny::shinyServer(function(input, output, session) {
    
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
        chdir_running = FALSE,
        paea_running = FALSE,
        manual_upload = TRUE,
        disease_sig_fetch_running = FALSE
    )
    
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
    
    output$fetch_disease_sig_container <- shiny::renderUI({
        button <- shiny::actionButton('fetch_disease_sig', 'Fetch signature')
        if(input$disease_sigs_choices == '' || values$disease_sig_fetch_running) {
            list(disabledActionButton(button))
        } else {
            list(button)
        }
    })
    
    
    #' datain panel - disease sig observe
    #'
    shiny::observe({
        if(is.null(input$fetch_disease_sig)) { return() } else if(input$fetch_disease_sig == 0) { return() }
        
        values$disease_sig_fetch_running <- TRUE
        
        tryCatch({
            
                choice <- shiny::isolate(input$disease_sigs_choices)
                values$input_name <- choice
                
                values$datapath <- if(stringi::stri_startswith_fixed(config$sigs_path, 'http')) {
                    download.file(file.path(config$sigs_path, choice), file.path(tempdir(), choice))
                    file.path(tempdir(), choice)
                } else {
                    file.path(config$sigs_path, choice)
                }
                
                
                sig <- disease_sigs %>% dplyr::filter(file_name == values$input_name)
                    
                values$control_samples <- unlist(stringi::stri_split_fixed(sig$ctrl_ids, ','))
                values$treatment_samples <- unlist(stringi::stri_split_fixed(sig$pert_ids, ','))
                
                values$manual_upload <- FALSE
                
            },
            error = function(e) {
                values$last_error <- e
                NULL
            }
        )
        
        values$disease_sig_fetch_running <- FALSE
    })
    
    
    #' Read input data
    #'
    datain <- shiny::reactive({
        if(!is.null(values$datapath)) {
            tryCatch(
                if(values$manual_upload) {
                    data.table::fread(values$datapath, sep=input$sep) 
                } else {
                    data.table::fread(values$datapath) %>% dplyr::select(-ID_REF)
                },
                error = function(e) {
                    values$last_error <- e
                    NULL
                }
            )
        }
    })
    
    
    #' Is input valid?
    #'
    datain_valid <- shiny::reactive({ datain_is_valid(datain())$valid })
    
    
    #' Apply preprocessing steps to datain
    #'
    #'
    datain_preprocessed <- shiny::reactive({
        if(datain_valid()) {
            id_filter <- if(input$enable_id_filter) {
                config$id_filter
            } else { NULL }
            
            datain_preprocess(
                datain=datain(),
                log2_transform=input$log2_transform,
                quantile_normalize=input$quantile_normalize,
                id_filter=id_filter
            ) %>% na.omit()
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
        if (datain_valid() && values$manual_upload) {
           shiny::checkboxGroupInput(
                'sampleclass',
                'Choose control samples',
                colnames(datain())[-1]
            )
        } else if (is.null(datain())) {
            shiny::helpText('To select samples you have to upload your dataset.')
        } else if (ncol(datain()) == 1) {
           shiny:: helpText('No experimental data detected. Please check separator and/or uploaded file')
        } else if (ncol(datain()) < 5) {
            shiny::helpText('You need at least four samples to run Characteristic Direction Analysis')
        } else if (!values$manual_upload) {
            shiny::helpText('You can choose samples only for manually uploaded data sets.')
        } else {
            shiny::helpText('It looks like your dataset is invalid')
        } 
    })
    
    
    #' Update lists of control/treatment samples
    #'
    shiny::observe({
        if(values$manual_upload) {
            if(datain_valid()) {
                datain <- datain()
                samples_mask <- colnames(datain)[-1] %in%  input$sampleclass
                values$control_samples <- colnames(datain)[-1][samples_mask]
                values$treatment_samples <- colnames(datain)[-1][!samples_mask]
            } else {
                values$control_samples <- NULL
                values$treatment_samples <- NULL
            }
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
    output$show_datain_results <- shiny::reactive({ datain_is_valid(datain())$valid })
    shiny::outputOptions(output, 'show_datain_results', suspendWhenHidden = FALSE)
    
    #' datain tab - density plot
    #'
    shiny::observe({
        if(datain_valid()) {
            plot_density(datain_preprocessed()) %>% ggvis::bind_shiny('datain_density_ggvis')
        }
    })
    
    
    #' chdir panel - run button
    #'
    #'
    output$run_chdir_container <- shiny::renderUI({
        button <- shiny::actionButton(inputId = 'run_chdir', label = 'Run Characteristic Direction Analysis', icon = NULL)
        if(!datain_valid() | length(values$control_samples) < 2 | length(values$treatment_samples) < 2 | values$chdir_running) {
             list(
                disabledActionButton(button),
                if (is.null(datain())) {
                    shiny::helpText('Upload your dataset and select control samples first.')
                } else {
                    shiny::helpText('You need at least two control and two treatment samples to run chdir.')
                }
             )
            
        } else {
            button
        }
    })
    
    #' Not the best solution, but we want to render buttons even if we switch tabs using tourist
    #'
    shiny::outputOptions(output, 'run_chdir_container', suspendWhenHidden = FALSE)
    
    
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
        paste(values$control_samples, collapse = ', ')
    })
    
    
    #' chdir panel - treatment samples
    #'
    output$treatment_samples <- shiny::renderText({
        paste(values$treatment_samples, collapse = ', ')
    })
    
 
    #' Run Characteristic Direction Analysis
    #'
    shiny::observe({
        if(is.null(input$run_chdir)) { return() } else if(input$run_chdir == 0) { return() }
        datain <- shiny::isolate(datain_preprocessed())
        nnull <- min(as.integer(shiny::isolate(input$chdir_nnull)), 1000)
        gamma <- shiny::isolate(input$chdir_gamma)
        seed <- shiny::isolate(input$random_seed)
        
        sampleclass <- factor(ifelse(colnames(datain)[-1] %in% values$control_samples, '1', '2'))
        
        set.seed(seed)
        
        values$chdir_running <- TRUE
        
        #' Store parameters.
        #'
        values$chdir_params <- list(
            manual_upload=values$manual_upload,
            input_name=values$input_name,
            control_samples=values$control_samples,
            treatment_samples=values$treatment_samples,
            log2_transform=input$log2_transform,
            quantile_normalize=input$quantile_normalize,
            enable_id_filter=input$enable_id_filter,
            gamma=gamma,
            nnull=nnull,
            seed=seed
        )
        
        values$chdir <- tryCatch(
            chdir_analysis_wrapper(preprocess_chdir_input(datain), sampleclass, gamma, nnull),
            error = function(e) {
                values$last_error <- e
                NULL
            }
        )
        
        values$chdir_running <- FALSE
 
    })
    
    
    #' chdir tab - set plots visibility
    #'
    output$show_chdir_results <- shiny::reactive({ !is.null(values$chdir) })
    shiny::outputOptions(output, 'show_chdir_results', suspendWhenHidden = FALSE)

    
    #' Plot top genes from Characteristic Direction Analysis
    #'
    shiny::observe({
        # Not as reactive as it should be
        # https://groups.google.com/forum/#!topic/ggvis/kQQsdn1RYaE
        if(!is.null(values$chdir)) {
            results <- prepare_results(values$chdir$results[[1]])
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
        
        if(!is.null(values$chdir)) {
            ngenes <- length(values$chdir$results[[1]])
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
        if (is.null(values$chdir)) {
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
        if(!is.null(values$chdir)) {
            nrow(chdir_up_genes())
        }
    })
    
    #' chdir panel - number of significant downregulated genes
    #'
    output$n_sig_down_genes <- shiny::renderText({
        if(!is.null(values$chdir)) {
            nrow(chdir_down_genes())
        }
    })

    
    #' chdir panel - chdir download
    #'
    output$download_chdir <- shiny::downloadHandler(
        filename = 'chdir.tsv',
        content = chdir_download_handler(prepare_results(values$chdir$chdirprops$chdir[[1]][, 1]))
    )
    
    
    #' chdir panel - prepare down genes
    #'
    chdir_up_genes <- shiny::reactive({
        if(!is.null(values$chdir)) {
            head(prepare_up_genes(values$chdir$results[[1]]), input$ngenes_tokeep)
        }
    })
    
    
    #' chdir panel - prepare up genes
    #'
    chdir_down_genes <- shiny::reactive({
        if(!is.null(values$chdir)) {
            head(prepare_down_genes(values$chdir$results[[1]]), input$ngenes_tokeep)
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
        if(!is.null(values$chdir)) {
            chdir_up_genes() %>% dplyr::rename(Gene = g, 'Characteristic Direction Coefficient' = v)
        }
    })
    
    
    #' chdir panel - down genes table
    #'
    output$chdir_down_genes_table <- shiny::renderDataTable({
        if(!is.null(values$chdir)) {
            chdir_down_genes() %>% dplyr::rename(Gene = g, 'Characteristic Direction Coefficient' = v)
        }
    })
    
        
    #' chdir panel - status message
    #'
    output$chdir_message <- shiny::renderText({
        if(is.null(values$chdir)) {
            'results not available...'
        }
    })
    
    
    #' chdir panel - Enrichr submit form
    #'
    output$enrichr_form <- shiny::renderUI({
        button <-shiny::tags$button('Analyze with Enrichr', class='btn btn-default')
        chdir_diff_genes <- list(up=chdir_up_genes, down=chdir_down_genes)
        value <- if(is.null(values$chdir)) { '' } else {
            chdir_diff_genes[[input$enrichr_subset]]() %>% prepare_enrichr_input() 
        }
        
        shiny::tags$form(
            target='_blank', method='post', enctype='multipart/form-data',
            action='http://amp.pharm.mssm.edu/Enrichr/enrich',
            shiny::tags$input(name='list', type='hidden', value=value),
            if(is.null(values$chdir)) { disabledActionButton(button) } else { button }
        )
    })
    
    
    #' chdir panel - run summary
    #'
    
    output$chdir_run_summary <- shiny::renderUI({
        if(!is.null(values$chdir)) {
            list_to_defs(values$chdir_params)
        }
    })
    
    #' paea panel - run button
    #'
    output$run_paea_container <- shiny::renderUI({
        button <- shiny::actionButton(inputId = 'run_paea', label = 'Run Principle Angle Enrichment', icon = NULL)
        if (values$paea_running) {
           list(disabledActionButton(button))
        } else if(is.null(values$chdir)) {
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

        
    #' Run Principle Angle Enrichment Analysis
    #'
    shiny::observe({
        if(is.null(input$run_paea)) { return() } else if(input$run_paea == 0) { return() }
        chdir <- shiny::isolate(values$chdir)
        casesensitive <- shiny::isolate(input$paea_casesensitive)
        background_dataset <- shiny::isolate(input$background_dataset)
        
        if(!(is.null(chdir))) {
            values$paea_running <- TRUE
            
            values$paea_params <- append(
                list(
                    casesensitive=casesensitive,
                    background_dataset=background_dataset
                ),
                values$chdir_params
            )
            
            values$paea <- tryCatch(
                shiny::withProgress(message = '', value = 0, {
                     
                    lapply(paea_analysis_dispatch(
                        chdirresults=chdir$chdirprops,
                        gmtfile=prepare_gene_sets(perturbations_data[[background_dataset]]$genes),
                        casesensitive=casesensitive,
                        with_progress=TRUE
                    ), paea_to_df)

                }),
                error = function(e) {
                    values$last_error <- e
                    NULL
                }
            )
            

            
            values$paea_running <- FALSE
        }
    })
    
    
    #' PAEA results
    #' 
    paea_results <- shiny::reactive({
        if(!is.null(values$paea)) {
            description <- perturbations_data[[values$paea_params$background_dataset]]$description
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
            render_params(values$paea_params)
        }
    })
    
})
