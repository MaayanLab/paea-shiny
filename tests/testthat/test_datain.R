library(testthat)

testthat::test_that('Test plot_density', {
    testthat::expect_equal(
        class(plot_density(data.frame(letters[1:10], rnorm(10), rnorm(10)))),
        'ggvis'
    )     
})