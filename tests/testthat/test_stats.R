library(testthat)

testthat::test_that('Test submissions_per_user', {
    dataset <- list(description=extract_description(preprocess_data(example_microtask_data)))
    testthat::expect_true(is.data.frame(submissions_per_user(dataset)))
})


testthat::test_that('Test submissions_per_cell_type', {
    dataset <- list(description=extract_description(preprocess_data(example_microtask_data)))
    testthat::expect_true(is.data.frame(submissions_per_cell_type(dataset)))
})
