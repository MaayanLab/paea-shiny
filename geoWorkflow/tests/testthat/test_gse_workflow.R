library(testthat)

testthat::test_that('Test process_gse', {
    result <- process_gse('GSE7596', tempdir(), tempdir())
    testthat::expect_equal(length(result), 1)
})
