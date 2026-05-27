#' @description
#' Removes weak positions of a given chromosome.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param chromosomeName
#' The name of a chromosome.
#' @param threshold
#' The minimum average interaction for a position to be kept.
#'
#' @return
#' A list of:
#' - The weak positions.
#' - The filtered interactions.
#'
#' @keywords internal
#' @noRd
.filterWeakPositionsOfChromosome <- function(
    chromosomeName,
    reducedObject,
    threshold
) {

    validColumns <- reducedObject@validAssay[[chromosomeName]]

    interactions <- as.data.table(InteractionSet::interactions(reducedObject))
    interactions <- interactions[, .(index1, index2)]

    # All known bins
    minBin <- min(interactions$index1, interactions$index2)
    maxBin <- max(interactions$index1, interactions$index2)
    allBins <- seq(minBin, maxBin)

    # Reducing the values of diagonal by 0.5 factor -> only upper matrix.
    diagonal <- (interactions$index1 == interactions$index2)
    matrixAssay <- SummarizedExperiment::assay(reducedObject)[,validColumns]
    matrixAssay <- matrixAssay * (1-0.5*(diagonal))

    interactions <- base::cbind(
        interactions,
        matrixAssay
    )
    interactions <- data.table::melt.data.table(
        interactions,
        id.vars = c("index1", "index2"),
        na.rm = FALSE
    )
    interactions[is.na(value),value := 0]

    totalBins <- reducedObject@totalBins[chromosomeName]
    removedBins <- allBins[
        !(allBins %in% unique(c(interactions$index1, interactions$index1)))
    ]

    totalNewWeakBins <- 1
    totalRemovedBins <- 0

    # Recursive removal of bins - deleting a bin can create a new weak bin.
    while (totalNewWeakBins > 0 && totalRemovedBins <= length(allBins)) {
        sum1 <- interactions[
            ,
            .(sum1 = sum(value, na.rm = TRUE)),
            by = .(index = index1, variable)
        ]
        sum2 <- interactions[
            ,
            .(sum2 = sum(value, na.rm = TRUE)),
            by = .(index = index2, variable)
        ]
        sum12 <- data.table::merge.data.table(
            sum1,
            sum2,
            by = c("index", "variable"),
            all = TRUE
        )
        sum12[is.na(sum1), sum1 := 0]
        sum12[is.na(sum2), sum2 := 0]
        sum12[, mean := (sum1 + sum2) / totalBins]
        weakBins <- unique(sum12[mean < threshold, index])

        totalNewWeakBins <- length(weakBins) - totalRemovedBins
        removedBins <- c(removedBins, weakBins)

        # Remove interactions of weak bins
        if (totalNewWeakBins > 0) {
            interactions <- interactions[
                !(index1 %in% weakBins | index2 %in% weakBins)
            ]
            totalRemovedBins <- totalRemovedBins + totalNewWeakBins
        }
    }

    message(
        "Chromosome ",
        chromosomeName,
        ": ",
        length(removedBins),
        " position",
        if (length(removedBins) != 1) "s",
        " removed, ",
        length(allBins) - length(removedBins),
        " position",
        if (length(allBins) - length(removedBins) != 1) "s",
        " remaining."
    )
    return(removedBins)
}

#' @title
#' Filter weak positions.
#'
#' @description
#' Removes weak genomic positions whose interactions average is lower than the
#' threshold.
#'
#' @details
#' Detects weak genomic positions in each replicate, and removes them from all
#' replicates to guarantee comparability across conditions when calling
#' \code{\link{detectCompartments}}.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param threshold
#' The minimum average interaction for a position to be kept. If a position's
#' average interaction with the entire chromosome is lower than this value in
#' any of the replicates, it is removed from all replicates and conditions.
#' Defaults to \code{object$weakPositionThreshold} which is originally set to
#' \code{defaultHiCDOCParameters$weakPositionThreshold} = 1.
#'
#' @return
#' A filtered \code{\link{HiCDOCDataSet}}.
#'
#' @seealso
#' \code{\link{filterSmallChromosomes}},
#' \code{\link{filterSparseReplicates}},
#' \code{\link{HiCDOC}}
#'
#' @examples
#' data(exampleHiCDOCDataSet)
#' object <- exampleHiCDOCDataSet
#'
#' object <- filterWeakPositions(object)
#'
#' @export
filterWeakPositions <- function(object, threshold = NULL) {
    .validateSlots(
        object,
        slots = c(
            "chromosomes",
            "totalBins",
            "parameters"
        )
    )

    if (!is.null(threshold)) {
        object@parameters$weakPositionThreshold <- threshold
    }
    object@parameters <- .validateParameters(object@parameters)
    threshold <- object@parameters$weakPositionThreshold

    message(
        "Keeping positions with interactions average greater or equal to ",
        threshold,
        "."
    )

    objectChromosomes <- S4Vectors::split(
        object,
        SummarizedExperiment::mcols(object)$chromosome,
        drop = FALSE
    )

    weakBins <- pbapply::pbmapply(
        function(c, m, t) .filterWeakPositionsOfChromosome(c, m, t),
        object@chromosomes,
        objectChromosomes,
        threshold
    )

    object@weakBins <- weakBins

    indices <- as.data.table(InteractionSet::interactions(object))
    toRemove <- (
        indices$index1 %in% unlist(weakBins) |
        indices$index2 %in% unlist(weakBins)
    )
    if (sum(toRemove)>0) {
        object <- object[!toRemove,]
        object <- InteractionSet::reduceRegions(object)
        object@validAssay <- .determineValids(object)
        # Remove empty chromosomes
        leftChromosomes <- 
            names(object@validAssay[vapply(object@validAssay, 
                                           function(x) length(x)>0, 
                                           FUN.VALUE = TRUE)])
        if(!identical(leftChromosomes,object@chromosomes)){
            object <- reduceHiCDOCDataSet(object, chromosomes = leftChromosomes)
        }
    }

    totalWeakBins <- sum(vapply(weakBins, length, FUN.VALUE = 0))

    message(
        "Removed ",
        totalWeakBins,
        " position",
        if (totalWeakBins != 1) "s",
        " in total."
    )

    if (length(toRemove) == sum(toRemove)) {
        warning("No data left!", call. = FALSE)
    }
    return(object)
}
