library(testthat)

testthat::test_that('Test prepare_results', {
    results <- structure(
        c(-0.608, 0.379, -0.348, 0.272, -0.208, 0.198, 0.179, 0.139, -0.121, -0.119),
        .Names = c("MCL1", "LIMD2", "RPL27", "MRPS18A", "TBL1X", "SOD1", "DPP4", "NOX4", "POLR2I", "ZDHHC20")
    )
    testthat::expect_equal(class(prepare_results(results, 3)), 'data.frame')
    testthat::expect_equal(dim(prepare_results(results, 3)), c(3, 2))
})


testthat::test_that('Test plot_top_genes', {
    results <- prepare_results(structure(
        c(-0.608, 0.379, -0.348, 0.272, -0.208, 0.198, 0.179, 0.139, -0.121, -0.119),
        .Names = c("MCL1", "LIMD2", "RPL27", "MRPS18A", "TBL1X", "SOD1", "DPP4", "NOX4", "POLR2I", "ZDHHC20")
    ))
    testthat::expect_equal(class(plot_top_genes(results)), 'ggvis')
})


testthat::test_that('Test prepare_down_genes and prepare_up_genes ', {
    results <- structure(
        c(-0.608, 0.379, -0.348, 0.272, -0.208, 0.198, 0.179, 0.139, -0.121, -0.119),
        .Names = c("MCL1", "LIMD2", "RPL27", "MRPS18A", "TBL1X", "SOD1", "DPP4", "NOX4", "POLR2I", "ZDHHC20")
    )
    testthat::expect_equal(nrow(prepare_down_genes(results)), 5)
    testthat::expect_true(all(prepare_down_genes(results)$v < 0))
    
    testthat::expect_equal(nrow(prepare_up_genes(results)), 5)
    testthat::expect_true(all(prepare_up_genes(results)$v > 0))
})

testthat::test_that('Test chdir_analysis_wrapper', {
    set.seed(323)
    datain <- data.frame(stringi::stri_rand_strings(100, 20), replicate(6, rlnorm(100)))
    colnames(datain) <- c('gene', 'c1', 'c2', 'c3', 't1', 't2', 't3')
    datain <- datain %>% dplyr::arrange(gene)
    
    sampleclass <- structure(c(1L, 1L, 1L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor")
    gammas <- list(1)

    testthat::expect_equal(
        {
            set.seed(323)
            png('/dev/null')
            chdir <- GeoDE::chdirAnalysis(
                datain,
                sampleclass, gammas, CalculateSig=TRUE, nnull=5
            )
            dev.off()
            chdir 
        },
        {
            set.seed(323)
            chdir_analysis_wrapper(datain, sampleclass, gammas, 5)
        }
    )
    
})


testthat::test_that('Test preprocess_chdir_input', {
    datain <- data.frame(x=c('a', 'a', 'b'), y=c(0, 1, 2))
    preprocessed_datain <- preprocess_chdir_input(datain)
    
    testthat::expect_true(is.data.frame(preprocessed_datain))
    testthat::expect_equal(dim(preprocessed_datain), c(2, 2))
    testthat::expect_equal(colnames(preprocessed_datain), c('IDENTIFIER', 'y'))
    testthat::expect_equal(preprocessed_datain$y, c(0.5, 2))
})


