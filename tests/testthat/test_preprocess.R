library(testthat)
library(stringi)
library(dplyr)

testthat::test_that('Test preprocess_data', {
    data(example_data)
    testthat::expect_equal(dim(preprocess_data(example_data)), dim(example_data))
    testthat::expect_equal(preprocess_data(example_data)$geo_accession, example_data$V1)
    testthat::expect_equal(preprocess_data(example_data)$id, example_data$V12)
    testthat::expect_false(any(grepl('&Acirc;&nbsp;', preprocess_data(example_data)$control)))
})

testthat::test_that('extract_samples', {
    data(example_data)
    example_data <- preprocess_data(example_data)
    samples_table <- extract_samples(example_data)
    
    control <- unlist(stri_split_fixed(example_data$control, ',', omit_empty = TRUE))
    treatment <- unlist(stri_split_fixed(example_data$treatment, ',', omit_empty = TRUE))
    
    testthat::expect_equal(as.character(samples_table$sample), c(control, treatment))
    testthat::expect_equal(
        as.character(filter(samples_table, id == 24 & group == 'control')$sample),
        c('GSM463015', 'GSM463016', 'GSM463017', 'GSM463018')
    )
    testthat::expect_equal(
        as.character(filter(samples_table, id == 35 & group == 'treatment')$sample),
        c('GSM1133119', 'GSM1133120', 'GSM1133121', 'GSM1133122', 'GSM1133123')
    )  
})

testthat::test_that('Test extract_genes', {
    data(example_data)
    example_data <- preprocess_data(example_data)
    genes_table <- extract_genes(example_data)
    upregulated <- unlist(stri_split_fixed(example_data$upregulated, '\n'))
    downregulated <- unlist(stri_split_fixed(example_data$downregulated, '\n'))
    
    testthat::expect_equal(length(unique(genes_table$id)), length(example_data$id))
    testthat::expect_equal(dim(filter(genes_table, id == 24, category == 'upregulated'))[1], 140)
    testthat::expect_equal(nrow(filter(genes_table, category == 'upregulated')), length(upregulated))
    testthat::expect_equal(nrow(filter(genes_table, category == 'downregulated')), length(downregulated))
})

