test_that("plotConcordances returns error if no compartments", {
    data(exampleHiCDOCDataSet)
    expect_error(
        pp <- plotConcordances(exampleHiCDOCDataSet),
        "No compartments found."
    )
})

test_that("plotConcordances behaves as expected", {
    data(exampleHiCDOCDataSetProcessed)
    expect_error(
        plotConcordances(exampleHiCDOCDataSetProcessed), 
        '"chromosome"'
    )
    expect_error(plotConcordances(exampleHiCDOCDataSetProcessed, 6), "Unknown")

    pp <- plotConcordances(exampleHiCDOCDataSetProcessed, 1)
    expect_true(ggplot2::is_ggplot(pp))
    labs <- ggplot2::get_labs(pp)
    expect_equivalent(labs[["title"]], 
        "Concordances of chromosome X by condition")
    expect_equivalent(labs[["caption"]], 
        "The grey areas are significant changes (adjusted p-value <= 5%)")
    expect_equivalent(labs[["x"]], "position")
    expect_equivalent(labs[["y"]], "concordance")
    expect_equivalent(labs[["colour"]], "replicate")
    
    expect_is(pp$layers[[1]]$geom, "GeomRect")
    expect_is(pp$layers[[2]]$geom, "GeomLine")
    # No error when printed
    expect_error(print(pp), NA)
})
