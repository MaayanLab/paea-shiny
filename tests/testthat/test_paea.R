library(testthat)

testthat::test_that('prepare_paea_results', {
    paea <- GeoDE::PAEAAnalysis(example_chdirresults, example_gmtfile)
    description <- extract_description(preprocess_data(example_microtask_data))
    testthat::expect_equal(
        nrow(prepare_paea_results(paea$p_values, description)),
        length(paea$p_values)
    )
})


testthat::test_that('paea_analysis_wrapper', {
    testthat::expect_identical(
        paea_analysis_wrapper(example_chdirresults, example_gmtfile),
        GeoDE::PAEAAnalysis(example_chdirresults, example_gmtfile)
    )
})


testthat::test_that('paea_analysis_dispatch', {
    expect_equal(
        paea_analysis_dispatch(example_chdirresults, example_gmtfile)$down,
        GeoDE::PAEAAnalysis(example_chdirresults_down, example_gmtfile_down)
    )
    
    expect_equal(
        paea_analysis_dispatch(example_chdirresults, example_gmtfile)$up,
        GeoDE::PAEAAnalysis(example_chdirresults_up, example_gmtfile_up)
    )
})