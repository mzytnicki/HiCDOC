#' @title
#' Plot boxplots of self interaction ratios.
#'
#' @description
#' Plots the boxplots of self interaction ratios, which are the differences
#' between self interaction and median of other interactions for each genomic
#' position. Since the A compartment is open with more interactions overall, it
#' is assumed that self interaction ratios in compartment A are smaller than in
#' compartment B.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param chromosome
#' A chromosome name or index in \code{chromosomes(object)}.
#' A \code{\link{HiCDOCDataSet}}.
#' @param checks
#' Logical. Should sanity checks messages be printed on plot ? Default to TRUE. 
#'
#' @return
#' A \code{ggplot}.
#'
#' @examples
#' data(exampleHiCDOCDataSetProcessed)
#' plotSelfInteractionRatios(exampleHiCDOCDataSetProcessed, chromosome = 1)
#'
#' @export
plotSelfInteractionRatios <- function(object, chromosome, checks=TRUE) {
    .validateSlots(object, slots = c("selfInteractionRatios", "compartments"))
    chromosomeName <- .validateNames(object, chromosome, "chromosomes")
    
    compartements <- as.data.table(
        object@compartments[
            Seqinfo::seqnames(object@compartments) == chromosomeName
        ]
    )
    dataplot <- data.table::merge.data.table(
        object@selfInteractionRatios[chromosome == chromosomeName],
        compartements[, .(
            chromosome = seqnames,
            condition,
            index,
            compartment
        )],
        by = c("chromosome", "condition", "index"),
        all.x = TRUE
    )

    plot <- ggplot(
        dataplot,
        aes(x = compartment, y = ratio)
    ) + geom_jitter(
        aes(color = compartment),
        width = 0.35
    ) + geom_boxplot(
        outlier.colour = NA,
        fill = NA,
        colour = "grey20"
    ) + labs(
        color = "Compartment",
        x = "Compartment",
        y = "Interaction difference",
        title = paste0(
            "Differences between self-interactions ",
            "and other interactions"
        ),
        subtitle = paste0("Chromosome ", chromosomeName)
    )
    
    if(checks){
        messages <- .messageCheck(object, chromosomeName)
        plot <- plot + labs(caption=paste0("Quality control:\n",
                                           messages$assignment))
    }
    return(plot)
}
