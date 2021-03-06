library(testthat)

testthat::test_that('Test submissions_per_user', {
    dataset <- list(description=extract_description(preprocess_data(example_microtask_data)))
    testthat::expect_true(is.data.frame(submissions_per_user(dataset)))
})


testthat::test_that('Test submissions_per_cell_type', {
    dataset <- list(description=extract_description(preprocess_data(example_microtask_data)))
    testthat::expect_true(is.data.frame(submissions_per_cell_type(dataset)))
})


testthat::test_that('Test submissions_per_organism', {
    dataset <- list(description=extract_description(preprocess_data(example_microtask_data)))
    testthat::expect_true(is.data.frame(submissions_per_organism(dataset)))
})


testthat::test_that('Test genes_coverage', {
     dataset <- list(genes=extract_genes(preprocess_data(example_microtask_data)))
     testthat::expect_true(is.data.frame(genes_coverage(dataset)))
})
