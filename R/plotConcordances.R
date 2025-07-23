#' @title
#' Plot concordances.
#'
#' @description
#' Plots the concordances of each replicate in each experiment condition. A
#' concordance can be understood as a confidence in a genomic position's
#' assigned compartment. Mathematically, it is the log ratio of a genomic
#' position's distance to each compartment's centroid, normalized by the
#' distance between both centroids, and min-maxed to a [-1,1] interval.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param chromosome
#' A chromosome name or index in \code{chromosomes(object)}.
#' @param xlim
#' A vector of the minimum and maximum positions to display. If NULL, displays
#' all positions. Defaults to NULL.
#' @param threshold
#' Significance threshold for the compartment changes. Defaults to 0.05.
#' @param points
#' Whether or not to add points to the concordances. Defaults to FALSE.
#'
#' @return
#' A \code{ggplot}.
#'
#' @examples
#' data(exampleHiCDOCDataSetProcessed)
#' plotConcordances(exampleHiCDOCDataSetProcessed, chromosome = 1)
#'
#' @export
plotConcordances <- function(
    object,
    chromosome,
    xlim = NULL,
    threshold = 0.05,
    points = FALSE
) {
    .validateSlots(object, slots = c("concordances", "differences"))
    chromosomeName <- .validateNames(object, chromosome, "chromosomes")
    xlim <- .validateXlim(xlim, object, chromosomeName)

    concordances <- object@concordances[
        Seqinfo::seqnames(object@concordances) == chromosomeName
    ]
    concordances <- data.table::as.data.table(concordances)
    concordances[, condition := paste0("Concordances\n", condition)]
    concordances <- concordances[start >= xlim[1] & start <= xlim[2]]
    binSize <- .modeVector(concordances$width)

    if (nrow(concordances) == 0) {
        message("No concordances for chromosome ", chromosomeName, ".")
        return(NULL)
    }

    # Significant differences
    differences <- object@differences[
        Seqinfo::seqnames(object@differences) == chromosomeName
    ]
    differences <- as.data.table(differences)
    differences <- differences[pvalue.adjusted <= threshold]
    differences <- differences[start >= xlim[1] & start <= xlim[2]]
    differences <- data.table::melt(
        differences,
        id.vars = c("seqnames", "start", "end"),
        measure.vars = c("condition.1", "condition.2"),
        value.name = "condition"
    )
    differences[, condition := paste0("Concordances\n", condition)]


    caption <- "The grey areas are significant changes"
    if (nrow(differences) == 0) {
        caption <- "No change is significant"
    }
    caption <- paste0(
        caption,
        " (adjusted p-value <= ",
        round(100 * threshold, 2),
        "%)"
    )

    ylim <- c(
        min(concordances$concordance, na.rm = TRUE),
        max(concordances$concordance, na.rm = TRUE)
    )

    plot <- ggplot()
    if (nrow(differences) > 0) {
        plot <- plot + geom_rect(
            data = differences,
            aes(
                xmin = start,
                xmax = end,
                ymin = ylim[1],
                ymax = ylim[2]
            ),
            color = NA,
            fill = "gray80"
        )
    }
    plot <- plot + geom_line(
        data = concordances,
        aes(
            x = start + 0.5 * binSize,
            y = concordance,
            color = replicate
        )
    )
    if (points) {
        plot <- plot + geom_point(
            data = concordances,
            aes(
                x = start + 0.5 * binSize,
                y = concordance,
                color = replicate
            )
        )
    }
    plot <- (
        plot +
        labs(caption = caption) +
        xlab("position") +
        xlim(xlim[1], xlim[2] + binSize) +
        ylim(ylim) +
            geom_hline(yintercept = 0.0, linewidth = 0.1) +
        facet_grid(rows = vars(condition), margins = FALSE, switch = "y") +
        labs(title = paste0("Concordances of chromosome ",
                            chromosomeName, " by condition")) +
        theme_minimal() +
        theme(axis.title.x = element_text(size = 10)) +
        theme(
            axis.title.y = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "bottom",  
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 8),
            strip.placement = "outside"
        )
    )
    return(plot)
}
