#' @title
#' Plot compartment changes.
#'
#' @description
#' Plots the predicted compartments, along with their concordance in each
#' replicate, and significant changes between experiment conditions.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param chromosome
#' A chromosome name or index in \code{chromosomes(object)}.
#' @param threshold
#' Significance threshold for the compartment changes. Defaults to 0.05.
#' @param xlim
#' A vector of the minimum and maximum positions to display. If NULL, displays
#' all positions. Defaults to NULL.
#' @param points
#' Whether or not to add points to the concordances. Defaults to FALSE.
#' @param checks
#' Whether or not to add sanity checks messages. Default to TRUE.
#' @param colour
#' Border color for the compartments. Default to `gray90`. `NA` means no border.
#'
#'
#' @return
#' A \code{ggplot}.
#'
#' @examples
#' data(exampleHiCDOCDataSetProcessed)
#' plotCompartmentChanges(exampleHiCDOCDataSetProcessed, chromosome = 1)
#'
#' @export
plotCompartmentChanges <- function(
    object,
    chromosome,
    threshold = 0.05,
    xlim = NULL,
    points = FALSE,
    checks = TRUE,
    colour = "gray90"
    
) {

    .validateSlots(
        object,
        slots = c(
            "concordances",
            "compartments",
            "differences"
        )
    )
    chromosomeName <- .validateNames(object, chromosome, "chromosomes")
    concordancesPlot <- plotConcordances(
        object,
        chromosomeName,
        xlim,
        threshold,
        points
    )

    compartmentsPlot <- plotCompartments(
        object,
        chromosomeName,
        xlim,
        colour
    )
    
    if (is.null(compartmentsPlot) || is.null(concordancesPlot)) {
        return(NULL)
    }
    
    # Messages for the user
    captionConcordances <- concordancesPlot$labels$caption
    concordancesPlot$labels$caption <- NULL
    
    # Horizontal alignment of the sub-graphs (change width of the plots)
    plotsGrobs <- lapply(
        list(
            compartmentsPlot + theme(legend.position = "none",
                                     plot.margin = unit(c(1,0,0,0), "lines")) +
                               labs(title=NULL),
            concordancesPlot + theme(legend.position = "none",
                                     plot.margin = unit(c(0,0,0,0), "lines")) +
                               labs(title=NULL)
        ),
        ggplot2::ggplotGrob
    )

    commonWidths <- plotsGrobs[[length(plotsGrobs)]]$widths
    plotsGrobs <- lapply(
        plotsGrobs,
        function(x) {
            x$widths <- commonWidths
            return(x)
        }
    )
    
    if(checks){
        messages <- .messageCheck(object, chromosomeName)
        messages <- paste(messages, collapse = "\n")
        messages <- paste0("Quality controls:\n", messages)
        legendsGrob <- gridExtra::arrangeGrob(
            gridExtra::arrangeGrob(
                cowplot::get_plot_component(compartmentsPlot, 'guide-box-bottom', return_all = TRUE),
                cowplot::get_plot_component(concordancesPlot, 'guide-box-bottom', return_all = TRUE),
                ncol = 1,
                nrow = 2
            ),
            grid::textGrob(label=messages, x=0.9, y=0.1,
                           just=c("right", "bottom"),
                           gp=grid::gpar(fontsize=8)),
            ncol = 2,
            nrow = 1,
            padding = unit(1, "cm")
        )
    } else {
        legendsGrob <- gridExtra::arrangeGrob(
            cowplot::get_plot_component(compartmentsPlot, 'guide-box-bottom', return_all = TRUE),
            cowplot::get_plot_component(concordancesPlot, 'guide-box-bottom', return_all = TRUE),
            ncol = 2,
            nrow = 1,
            padding = unit(1, "cm")
        )
    }
    plot <- gridExtra::arrangeGrob(
                plotsGrobs[[1]],
                plotsGrobs[[2]],
                grid::textGrob(label=captionConcordances, x=0.1, y=1, 
                               just=c("left", "top"),
                               gp=grid::gpar(fontsize=8)),
                legendsGrob,
                heights = c(2, 10, 0.5, 2),
                nrow=4, 
                ncol=1,
                padding = unit(1, "lines"),
                top = paste0(
                    "Compartments and concordances of chromosome ",
                    chromosomeName, " by condition"
            )
    )
    
    return(cowplot::ggdraw(plot))
}
