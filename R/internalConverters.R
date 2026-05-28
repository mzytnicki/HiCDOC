#' @description
#' Fill \code{\link{InteractionSet}} with possibly missing values
#'
#' @param interactionSet
#' An \code{\link{InteractionSet}}.
#' @param interactionSetUnion
#' The full \code{\link{InteractionSet}}.
#' @param fill
#' Fill missing values with this.
#'
#' @return
#' The full \code{\link{InteractionSet}}.
#'
#' @keywords internal
#' @noRd
.fillInteractionSet <- function(
    interactionSet,
    interactionSetUnion,
    fill = NA
) {
    over <- GenomicRanges::match(interactionSet, interactionSetUnion)
    totalColumns <- ncol(interactionSet)
    newAssays <- matrix(
        rep(fill, length(interactionSetUnion) * totalColumns),
        ncol = totalColumns
    )
    newAssays[over, ] <- SummarizedExperiment::assay(interactionSet)
    return(
        InteractionSet::InteractionSet(
            newAssays,
            interactionSetUnion,
            colData = SummarizedExperiment::colData(interactionSet)
        )
    )
}

#' @description
#' Format the outputs produced by \code{detectCompartements}.
#' @param object
#' a HiCDOCDataSet object
#' @return
#' a HiCDOCDataSet object
#'
#' @keywords internal
#' @noRd
.formatDetectCompartment <- function(object) {
    chromosomeNames <- object@chromosomes
    conditionNames <- sort(unique(object$condition))
    replicateNames <- sort(unique(object$replicate))

    all.regions <- InteractionSet::regions(object)

    # Concordances
    object@concordances[, `:=`(
        chromosome = factor(chromosome, levels = chromosomeNames),
        replicate = factor(replicate, levels = replicateNames)
    )]
    concordances <- object@concordances
    object@concordances <- all.regions[
        match(concordances$index, S4Vectors::mcols(all.regions)$index)
    ]
    S4Vectors::mcols(object@concordances) <- S4Vectors::DataFrame(
        concordances[, .(
            index,
            condition,
            replicate,
            compartment,
            concordance
        )]
    )

    # Centroids
    object@centroids[, `:=`(
        chromosome = factor(chromosome, levels = chromosomeNames),
        condition = factor(condition, levels = conditionNames)
    )]

    # Differences
    object@differences <- data.table::merge.data.table(object@differences,
        object@checks, by = "chromosome")
    object@differences[, chromosome := factor(
        chromosome,
        levels = chromosomeNames
    )]
    object@differences[, significance := ""]
    object@differences[pvalue.adjusted <= 0.05, significance := "*"]
    object@differences[pvalue.adjusted <= 0.01, significance := "**"]
    object@differences[pvalue.adjusted <= 0.001, significance := "***"]
    object@differences[pvalue.adjusted <= 0.0001, significance := "****"]

    differences <- object@differences
    object@differences <- all.regions[
        match(differences$index, S4Vectors::mcols(all.regions)$index)
    ]
    S4Vectors::mcols(object@differences) <- S4Vectors::DataFrame(
        differences[, .(
            index,
            condition.1,
            condition.2,
            pvalue,
            pvalue.adjusted,
            direction,
            significance,
            centroid.check,
            PC1.check,
            assignment.check
        )]
    )

    # Compartments
    object@compartments[, chromosome := factor(
        chromosome,
        levels = chromosomeNames
    )]
    object@compartments <- data.table::merge.data.table(object@compartments,
        object@checks, by = "chromosome")
    compartments <- object@compartments
    object@compartments <- all.regions[
        match(compartments$index, S4Vectors::mcols(all.regions)$index)
    ]
    S4Vectors::mcols(object@compartments) <- S4Vectors::DataFrame(
        compartments[, .(
            index,
            condition,
            compartment,
            centroid.check,
            PC1.check,
            assignment.check
        )]
    )

    # Distances
    object@distances[, `:=`(
        chromosome = factor(chromosome, levels = chromosomeNames),
        condition = factor(condition, levels = conditionNames),
        replicate = factor(replicate, levels = replicateNames)
    )]

    # Comparisons
    object@comparisons[, chromosome := factor(
        chromosome,
        levels = chromosomeNames
    )]

    # selfInteractionRatios
    object@selfInteractionRatios[, `:=`(
        chromosome = factor(chromosome, levels =  chromosomeNames),
        condition = factor(condition, levels = conditionNames),
        replicate = factor(replicate, levels = replicateNames)
    )]

    return(object)
}
