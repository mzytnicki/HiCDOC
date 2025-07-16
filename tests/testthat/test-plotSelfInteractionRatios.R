test_that("plotSelfInteractionRatios returns an error if no compartments", {
    data(exampleHiCDOCDataSet)
    expect_error(
        pp <- plotSelfInteractionRatios(exampleHiCDOCDataSet),
        "No compartments found."
    )
})

test_that("plotSelfInteractionRatios behaves as expected", {
    data(exampleHiCDOCDataSetProcessed)
    expect_error(
        plotSelfInteractionRatios(exampleHiCDOCDataSetProcessed), 
        '"chromosome"')
    expect_error(
        plotSelfInteractionRatios(exampleHiCDOCDataSetProcessed, 4), 
        "Unknown")

    pp <- plotSelfInteractionRatios(exampleHiCDOCDataSetProcessed, 1)
    expect_true(ggplot2::is_ggplot(pp))
    labs <- ggplot2::get_labs(pp)
    expect_equivalent(labs[["caption"]], 
        "Quality control:\nA/B assignment reliability: OK")
    expect_equivalent(labs[["x"]], "Compartment")
    expect_equivalent(labs[["y"]], "Interaction difference")
    expect_equivalent(labs[["title"]], 
        "Differences between self-interactions and other interactions")
    expect_equivalent(labs[["colour"]], "Compartment")
    expect_equivalent(labs[["subtitle"]], "Chromosome X")
    expect_is(pp$layers[[1]]$geom, "GeomPoint")
    expect_is(pp$layers[[2]]$geom, "GeomBoxplot")
    # No error when printed
    expect_error(print(pp), NA)
})
