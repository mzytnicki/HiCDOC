#' @title
#' Plot A/B compartments.
#'
#' @description
#' Plots the predicted compartments in each experiment condition.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param chromosome
#' A chromosome name or index in \code{chromosomes(object)}.
#' @param xlim
#' A vector of the minimum and maximum positions to display. If NULL, displays
#' all positions. Defaults to NULL.
#' @param colour
#' Border color for the compartments. Default to `gray90`. `NA` means no border.
#'
#' @return
#' A \code{ggplot}.
#'
#' @examples
#' data(exampleHiCDOCDataSetProcessed)
#' plotCompartments(exampleHiCDOCDataSetProcessed, chromosome = 1)
#'
#' @export
plotCompartments <- function(
    object,
    chromosome,
    xlim = NULL,
    colour = "gray90"
) {

    .validateSlots(object, slots = c("compartments"))
    chromosomeName <- .validateNames(object, chromosome, "chromosomes")
    xlim <- .validateXlim(xlim, object, chromosomeName)

    compartments <- object@compartments[
        Seqinfo::seqnames(object@compartments) == chromosomeName
    ]

    if (length(compartments) == 0) {
        message("No compartments for chromosome ", chromosomeName, ".")
        return(NULL)
    }
    compartments <- as.data.table(compartments)
    binSize <- .modeVector(compartments$width)
    compartments[, position := start + 0.5 * binSize]
    # for the last bin if not of the same size
    if(identical(xlim, c(min(compartments[,start]), max(compartments[,end])))){
        xlim <- c(min(xlim[1], compartments[,position]),
                  max(xlim[2], compartments[,start + binSize]))
    }
    plot <- ggplot(
        data = compartments,
        aes(x = position, fill = compartment)
    ) + geom_histogram(
        binwidth = binSize,
        colour = colour,
        linewidth = 0.05
    ) + coord_cartesian(
        xlim=c(xlim[1] - 0.5 * binSize, xlim[2] + 0.5 * binSize)
    ) + facet_grid(
        rows = vars(condition),
        margins = FALSE,
        switch = "y"
    ) + labs(title = paste0("Compartments of chromosome ", 
                            chromosomeName, " by condition")) +
        theme_minimal() + theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "pt"),
        legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        strip.placement = "outside"
    )

    return(plot)
}
