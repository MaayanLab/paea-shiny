library(testthat)

testthat::test_that('Test prepare_enrichr_input', {
    chdir_results <- data.frame(
        g=c('Mir29b-2', 'Acmsd', 'Tnp1'),
        v=c(-0.179, -0.179, -0.126)
    )
    testthat::expect_equal(
        prepare_enrichr_input(chdir_results),
        'Mir29b-2,0.179\nAcmsd,0.179\nTnp1,0.126'
    )
})