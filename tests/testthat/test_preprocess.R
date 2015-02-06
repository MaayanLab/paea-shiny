testthat::test_that('Test preprocess_data', {
    data(example_data)
    testthat::expect_equal(dim(preprocess_data(example_data)), dim(example_data))
    testthat::expect_equal(
        preprocess_data(example_data)$geo_accession,
        factor(unlist(stringi::stri_replace_all_fixed(stringi::stri_trim_both(example_data$V1), '[ACCN]', '')))
    )
    testthat::expect_equal(preprocess_data(example_data)$id, example_data$V12)
    testthat::expect_false(any(grepl('&Acirc;&nbsp;', preprocess_data(example_data)$control)))
})


testthat::test_that('Test extract_samples', {
    data(example_data)
    example_data <- preprocess_data(example_data)
    samples_table <- extract_samples(example_data)
    
    control <- unlist(stringi::stri_split_fixed(example_data$control, ',', omit_empty = TRUE))
    treatment <- unlist(stringi::stri_split_fixed(example_data$treatment, ',', omit_empty = TRUE))
    
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
    data(example_data)
    example_data <- preprocess_data(example_data)
    genes_table <- extract_genes(example_data)
    upregulated <- unlist(stringi::stri_split_fixed(example_data$upregulated, '\n'))
    downregulated <- unlist(stringi::stri_split_fixed(example_data$downregulated, '\n'))
    
    testthat::expect_equal(length(unique(genes_table$id)), length(example_data$id))
    testthat::expect_equal(dim(dplyr::filter(genes_table, id == 24, category == 'upregulated'))[1], 140)
    testthat::expect_equal(nrow(dplyr::filter(genes_table, category == 'upregulated')), length(upregulated))
    testthat::expect_equal(nrow(dplyr::filter(genes_table, category == 'downregulated')), length(downregulated))
})


testthat::test_that('Test extract_description', {
    data(example_data)
    example_data <- preprocess_data(example_data)
    testthat::expect_equal(nrow(extract_description(example_data)), nrow(example_data))
    testthat::expect_equal(extract_description(example_data)$gene, example_data$gene)
    testthat::expect_equal(extract_description(example_data)$id, example_data$id)
    testthat::expect_equal(extract_description(example_data)$geo_accession, example_data$geo_accession)
})


testthat::test_that('Test prepare_gene_sets ', {
    data(example_data)
    example_data <- preprocess_data(example_data)
    genes_table <- extract_genes(example_data)
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


