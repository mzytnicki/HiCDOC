test_that("plotCompartments returns error if no compartments", {
    data(exampleHiCDOCDataSet)
    expect_error(
        pp <- plotCompartments(exampleHiCDOCDataSet),
        "No compartments found."
    )
})

test_that("plotCompartments behaves as expected", {
    data(exampleHiCDOCDataSetProcessed)
    expect_error(plotCompartments(exampleHiCDOCDataSetProcessed),
        "argument \"chromosome\"")
    expect_error(plotCompartments(exampleHiCDOCDataSetProcessed, 5), "Unknown")

    pp <- plotCompartments(exampleHiCDOCDataSetProcessed, 1)
    expect_true(ggplot2::is_ggplot(pp))
    labs <- ggplot2::get_labs(pp)
    expect_equivalent(labs[["title"]], "Compartments of chromosome X by condition")
    expect_equivalent(labs[["x"]], "position")
    expect_equivalent(labs[["fill"]], "compartment")
    expect_equivalent(labs[["y"]], "count")
    expect_equivalent(labs[["weight"]], "weight")
    expect_is(pp$layers[[1]]$geom, "GeomBar")
    # No error when printed
    expect_error(print(pp), NA)
})
