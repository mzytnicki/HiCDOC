test_that("plotConcordanceDifferences returns an error of no compartments", {
    data(exampleHiCDOCDataSet)
    expect_error(
        pp <- plotConcordanceDifferences(exampleHiCDOCDataSet),
        "Missing slots: comparisons"
    )
})

test_that("plotConcordanceDifferences behaves as expected", {
    data(exampleHiCDOCDataSetProcessed)
    expect_error(
        pp <- plotConcordanceDifferences(exampleHiCDOCDataSetProcessed),
        NA
    )
    labs <- ggplot2::get_labs(pp)
    expect_equivalent(labs[["x"]], "Concordance")
    expect_equivalent(labs[["fill"]], "Change\nof\ncompartment")
    expect_equivalent(labs[["title"]], 
                      "Distribution of concordance differences")
    expect_equivalent(labs[["y"]], "count")
    expect_equivalent(labs[["weight"]], "weight")
    
    expect_is(pp$layers[[1]]$geom, "GeomBar")
    # No error when printed
    expect_error(print(pp), NA)
})
