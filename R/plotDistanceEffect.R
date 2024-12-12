#' @title
#' Plot the distance effect.
#'
#' @description
#' Plots the distance effect on proportion of interactions.
#' Each point is a cell in the interaction matrix, such that
#' the x-axis is the distance with respect to the diagonal,
#' the y-axis is number of counts.
#' Dots are binned.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param chromosome
#' Name (character) or index of the chromosome, if the plot should be
#' restricted to only one chromosome. Default to NULL.
#' @param transformX
#' Transformation of the X axis. Default to "identity". See
#' \code{\link[ggplot2]{scale_x_continuous}} for other accepted values.
#' @param transformY
#' Transformation of the Y axis. Default to "identity". See
#' \code{\link[ggplot2]{scale_y_continuous}} for other accepted values.
#'
#' @return
#' A \code{ggplot}.
#'
#' @examples
#' data(exampleHiCDOCDataSet)
#' plotDistanceEffect(exampleHiCDOCDataSet)
#'
#' @export
plotDistanceEffect <- function(object, chromosome = NULL, transformX="identity", transformY="identity") {
    .validateSlots(object, slots = c("interactions"))
    if (!is.null(chromosome)) {
        if (length(chromosome) > 1) {
            warning(
                "`chromosome` should be of length 1, taking the first one."
            )
            chromosome < chromosome[1]
        }
        chromosomeName <- .validateNames(object, chromosome, "chromosomes")
        rowsId <- as.logical(
            S4Vectors::mcols(object)$chromosome == chromosomeName
        )
        addTitle <- paste(", chromosome", chromosomeName)
    } else {
        rowsId <- rep(TRUE, length(object))
        addTitle <- ""
    }

    distances <- InteractionSet::pairdist(object, type = "mid")[rowsId]
    matrixAssay <- SummarizedExperiment::assay(object)[rowsId, ]
    dfDistance <- data.table::data.table(
        "distance" = rep(distances, ncol(matrixAssay)),
        "interaction" = as.vector(matrixAssay)
    )
    dfDistance <- dfDistance[!is.na(interaction)]
    
    plot <- ggplot(
        dfDistance,
        aes(x = distance, y = interaction)
    ) + geom_bin2d() + scale_fill_gradient(
        low = "white",
        high = "blue",
        trans = "log2"
    ) + geom_point(col = "transparent") + 
        geom_smooth(col = "red") + 
        scale_y_continuous(trans=transformY)  + 
        scale_x_continuous(trans=transformX) + theme_bw() 
    
    margPlot <-  ggplot(dfDistance, aes(x = distance)) +
        geom_histogram(fill="transparent", col="black") + 
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) 
    
    layoutMatrix <- rbind(c(1,1,1,1,1,1,1,3),
                          c(2,2,2,2,2,2,2,3),
                          c(2,2,2,2,2,2,2,3),
                          c(2,2,2,2,2,2,2,3),
                          c(2,2,2,2,2,2,2,3))
    
    plot <- gridExtra::arrangeGrob(
                margPlot,
                plot + theme(legend.position = "none"),
                cowplot::get_plot_component(plot, 'guide-box-right', return_all = TRUE),
                layout_matrix = layoutMatrix,
                padding = unit(0.2, "lines"),
                top = paste0("Distance effect", addTitle)
            )
    return(cowplot::ggdraw(plot))
}
