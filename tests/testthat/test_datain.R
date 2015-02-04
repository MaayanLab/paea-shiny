library(testthat)
library(data.table)
library(dplyr)
library(preprocessCore)

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


test_that('Test datain_log2_transform', {
    expr <- replicate(5, rlnorm(26))
    datain <- data.frame(letters, expr)
    testthat::expect_equivalent(
        as.matrix(datain_log2_transform(datain) %>% select_(-1)),
        log2(expr)
    )
    testthat::expect_equivalent(
        as.matrix(datain_log2_transform(as.data.table(datain)) %>% select_(-1)),
        log2(expr)
    )
    testthat::expect_equivalent(
        as.matrix(datain_log2_transform(tbl_df(datain)) %>% select_(-1)),
        log2(expr)
    )
})


test_that('datain_quantile_normalize', {
    expr <- simplify2array(replicate(5, rlnorm(26)))
    datain <- data.frame(letters, expr)
    testthat::expect_equivalent(
        datain_quantile_normalize(datain),
        data.frame(letters, normalize.quantiles(expr))
    )
    testthat::expect_equivalent(
        datain_quantile_normalize(as.data.table(datain)),
        data.frame(letters, normalize.quantiles(expr))
    )
    testthat::expect_equivalent(
        datain_quantile_normalize(tbl_df(datain)),
        data.frame(letters, normalize.quantiles(expr))
    )
    
})


test_that('Test datain_preprocess', {
    expr <- simplify2array(replicate(5, rlnorm(26)))
    datain <- data.frame(letters, expr)
    testthat::expect_equivalent(
        datain_preprocess(datain),
        datain
    )
    testthat::expect_equivalent(
        datain_preprocess(datain, log2_transform=TRUE),
        data.frame(letters, log2(expr))
    )
    testthat::expect_equivalent(
        datain_preprocess(datain, quantile_normalize=TRUE),
        data.frame(letters, normalize.quantiles(expr))
    )
    testthat::expect_equivalent(
        datain_preprocess(datain, log2_transform=TRUE, quantile_normalize=TRUE),
        data.frame(letters, normalize.quantiles(log2(expr)))
    )
})
