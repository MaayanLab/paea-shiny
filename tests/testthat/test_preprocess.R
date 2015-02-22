testthat::test_that('Test preprocess_data', {
    testthat::expect_equal(dim(preprocess_data(example_microtask_data)), dim(example_microtask_data))
    testthat::expect_equal(
        preprocess_data(example_microtask_data)$geo_id,
        factor(unlist(stringi::stri_replace_all_fixed(stringi::stri_trim_both(example_microtask_data$geo_id), '[ACCN]', '')))
    )
    testthat::expect_equal(preprocess_data(example_microtask_data)$id, example_microtask_data$id)
    testthat::expect_false(any(grepl('&Acirc;&nbsp;', preprocess_data(example_microtask_data)$control)))
})


testthat::test_that('Test extract_samples', {
    example_microtask_data <- preprocess_data(example_microtask_data)
    samples_table <- extract_samples(example_microtask_data)
    
    control <- unlist(stringi::stri_split_fixed(example_microtask_data$control, ',', omit_empty = TRUE))
    treatment <- unlist(stringi::stri_split_fixed(example_microtask_data$treatment, ',', omit_empty = TRUE))
    
    testthat::expect_equal(as.character(samples_table$sample), c(control, treatment))
    testthat::expect_equal(
        as.character(dplyr::filter(samples_table, id == 24 & group == 'control')$sample),
        c('GSM463015', 'GSM463016', 'GSM463017', 'GSM463018')
    )
    testthat::expect_equal(
        as.character(dplyr::filter(samples_table, id == 35 & group == 'treatment')$sample),
        c('GSM1133119', 'GSM1133120', 'GSM1133121', 'GSM1133122', 'GSM1133123')
    )  
})


testthat::test_that('Test extract_genes', {
    example_microtask_data <- preprocess_data(example_microtask_data)
    genes_table <- extract_genes(example_microtask_data)
    upregulated <- unlist(stringi::stri_split_fixed(example_microtask_data$upregulated, '\n'))
    downregulated <- unlist(stringi::stri_split_fixed(example_microtask_data$downregulated, '\n'))
    
    testthat::expect_equal(length(unique(genes_table$id)), length(example_microtask_data$id))
    testthat::expect_equal(dim(dplyr::filter(genes_table, id == 24, category == 'upregulated'))[1], 140)
    testthat::expect_equal(nrow(dplyr::filter(genes_table, category == 'upregulated')), length(upregulated))
    testthat::expect_equal(nrow(dplyr::filter(genes_table, category == 'downregulated')), length(downregulated))
})


testthat::test_that('Test extract_description', {
    example_microtask_data <- preprocess_data(example_microtask_data)
    testthat::expect_equal(nrow(extract_description(example_microtask_data)), nrow(example_microtask_data))
    testthat::expect_equal(extract_description(example_microtask_data)$gene, example_microtask_data$gene)
    testthat::expect_equal(extract_description(example_microtask_data)$id, example_microtask_data$id)
    testthat::expect_equal(extract_description(example_microtask_data)$geo_id, example_microtask_data$geo_id)
})


testthat::test_that('Test prepare_gene_sets ', {
    example_microtask_data <- preprocess_data(example_microtask_data)
    genes_table <- extract_genes(example_microtask_data)
    gene_sets <- prepare_gene_sets(genes_table)
    
    testthat::expect_equal(
        length(gene_sets),
        nrow(genes_table %>% dplyr::select(id, category) %>% unique)
    )
    
    testthat::expect_equal(
        gene_sets[[1]][1:6],
        c('24_upregulated', 'TMEM59L', 'G0S2' , 'SPAG4', 'IL15RA', 'ACSM3')
    )
})


testthat::test_that('Test download_data', {
    # TODO
    # We don't want't to download data with every test
    # so either we have to mock httr.GET
    # or use local server
})


testthat::test_that('Test preprocess', {
    # TODO
    # See: Test preprocess for explanations
})


testthat::test_that('Test choose_unique_submissions', {
    samples <- data.frame(
        id=rep(c(1, 2), 6),
        group=rep(c('control', 'treatment'), each=6),
        sample=c(rep(c('a', 'b', 'c'), each=2), rep(c('d', 'e', 'f'), each='2'))
    )
    testthat::expect_equal(nrow(choose_unique_submissions(samples)), 1)
})

