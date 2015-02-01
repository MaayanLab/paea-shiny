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
    testthat::expect_equal(nrow(prepare_up_genes(results)), 5)
})