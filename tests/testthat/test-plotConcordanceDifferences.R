data(exampleHiCDOCDataSet)
object <- reduceHiCDOCDataSet(exampleHiCDOCDataSet,
                              replicates = c("R1", "R2"),
                              conditions = c("1", "2"))
object <- filterSparseReplicates(object)
object <- filterWeakPositions(object)

test_that("plotConcordanceDifferences behaves as expected", {
    expect_error(
        pp <- plotConcordanceDifferences(object),
        "Missing slots: comparisons"
    )
    set.seed(3215)
    object <- detectCompartments(object, parallel = FALSE)
    expect_error(
        pp <- plotConcordanceDifferences(object),
        NA
    )
    expect_is(pp, "ggplot")
    expect_identical(
        unlist(pp$labels),
        c(
            "x" = "Concordance",
            "title" = "Distribution of concordance differences",
            "fill" = "changed",
            "y" = "count",
            "weight" = "weight"
        )
    )
    expect_is(pp$layers[[1]]$geom, "GeomBar")
    # No error when printed
    expect_error(print(pp), NA)
})
