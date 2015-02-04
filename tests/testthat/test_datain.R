library(testthat)
library(data.table)

testthat::test_that('Test plot_density', {
    testthat::expect_equal(
        class(plot_density(data.frame(letters[1:10], rnorm(10), rnorm(10)))),
        'ggvis'
    )     
})


test_that('Test datain_is_valid', {
    testthat::expect_false(datain_is_valid(NULL)$valid) 
    testthat::expect_false(datain_is_valid(data.frame())$valid)
    testthat::expect_false(datain_is_valid(data.frame(runif(10)))$valid) 
    testthat::expect_true(datain_is_valid(data.frame(gene=letters, 0, 0, 0, 0))$valid)  
    testthat::expect_true(datain_is_valid(data.table(gene=letters, 0, 0, 0, 0))$valid)  
    testthat::expect_false(datain_is_valid(data.table(gene=letters, 0, 0, 0, -1))$valid)  
})


