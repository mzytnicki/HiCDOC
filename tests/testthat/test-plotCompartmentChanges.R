test_that("plotCompartmentChanges returns error if no compartments", {
    data(exampleHiCDOCDataSet)
    expect_error(
        pp <- plotCompartmentChanges(exampleHiCDOCDataSet),
        "No compartments found."
    )
})

test_that("plotCompartmentChanges behaves as expected", {
    data(exampleHiCDOCDataSetProcessed)
    expect_error(
        plotCompartmentChanges(exampleHiCDOCDataSetProcessed),
        "argument \"chromosome\""
    )
    expect_error(
        plotCompartmentChanges(exampleHiCDOCDataSetProcessed, 5), 
        "Unknown"
    )

    pp <- plotCompartmentChanges(exampleHiCDOCDataSetProcessed, 1)
    expect_true(ggplot2::is_ggplot(pp))
    labs <- ggplot2::get_labs(pp)
    expect_null(labs[["x"]])
    expect_null(labs[["y"]])
    expect_is(pp$layers[[1]]$geom, "GeomDrawGrob")
    # No error when printed
    expect_error(print(pp), NA)
})
