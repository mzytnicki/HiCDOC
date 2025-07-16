test_that("plotInteractions behaves as expected", {
    data(exampleHiCDOCDataSet)
    object <- reduceHiCDOCDataSet(exampleHiCDOCDataSet, 
                                  chromosomes = c("X", "Y"))
    
    expect_error(plotInteractions(object, 3), "Unknown chromosome")
    expect_error(plotInteractions(object), '"chromosome"')
    pp <- plotInteractions(object, 1)
    expect_true(ggplot2::is_ggplot(pp))
    labs <- ggplot2::get_labs(pp)
    expect_equivalent(labs[["title"]], "Chromosome X")
    expect_equivalent(labs[["x"]], "")
    expect_equivalent(labs[["y"]], "")
    expect_equivalent(labs[["z"]], "interaction")
    expect_equivalent(labs[["fill"]], "Intensity")
    # No error when printed
    expect_error(print(pp), NA)
})
