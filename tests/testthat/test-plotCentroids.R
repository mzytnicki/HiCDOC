test_that("plotCentroids returns error if no centroids", {
    data(exampleHiCDOCDataSet)
    expect_error(
        pp <- plotCentroids(exampleHiCDOCDataSet),
        "No compartments found."
    )
})

test_that("plotCentroids behaves as expected", {
    data(exampleHiCDOCDataSetProcessed)
    expect_error(plotCentroids(exampleHiCDOCDataSetProcessed),
        "argument \"chromosome\"")
    expect_error(plotCentroids(exampleHiCDOCDataSetProcessed, 5), "Unknown")

    pp <- plotCentroids(exampleHiCDOCDataSetProcessed, 1)
    expect_true(ggplot2::is_ggplot(pp))
    labs <- ggplot2::get_labs(pp)
    expect_equivalent(labs[["caption"]], 
        "Quality controls:\nCentroid PC1 inertia: OK\nA/B clustering consistency: OK")
    expect_equivalent(labs[["x"]], "PC1  91.19 %")
    expect_equivalent(labs[["y"]], "PC2  6.82 %")
    expect_equivalent(labs[["title"]], 
        "PCA on centroids of chromosome X")
    expect_equivalent(labs[["colour"]], "compartment")
    expect_equivalent(labs[["shape"]], "condition")
    expect_is(pp$layers[[1]]$geom, "GeomPoint")
    # No error when printed
    expect_error(print(pp), NA)
})
