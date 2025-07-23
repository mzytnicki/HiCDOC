#' @description
#' Reduces a \code{\link{HiCDOCDataSet}} by keeping only given chromosomes.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param chromosomeNames
#' The names of chromosomes to keep.
#' @param dropLevels
#' Whether or not to also remove unused factor levels after filtering. Should
#' be set to FALSE if the reduced objects are meant to be re-combined later.
#' Defaults to TRUE.
#'
#' @return
#' A reduced \code{\link{HiCDOCDataSet}}.
#'
#' @keywords internal
#' @noRd
.reduceHiCDOCChromosomes <- function(object, chromosomeNames, dropLevels) {
    chromosomeIds <- which(object@chromosomes %in% chromosomeNames)
    object@chromosomes <- object@chromosomes[chromosomeIds]

    object@weakBins <- object@weakBins[chromosomeIds]
    object@totalBins <- object@totalBins[chromosomeIds]
    object@validAssay <- object@validAssay[chromosomeIds]

    toKeep <- S4Vectors::`%in%`(
        S4Vectors::mcols(object)$chromosome,
        chromosomeNames
    )
    object <- object[toKeep, ]

    if (dropLevels) {
        SummarizedExperiment::mcols(object)$chromosome <- droplevels(
            SummarizedExperiment::mcols(object)$chromosome
        )
        object <- InteractionSet::reduceRegions(object)
        Seqinfo::seqlevels(
            InteractionSet::regions(object),
            pruning.mode = "coarse"
        ) <- object@chromosomes

    }
    for (slotName in c(
        "distances",
        "selfInteractionRatios",
        "compartments",
        "concordances",
        "differences",
        "centroids",
        "comparisons"
    )) {
        if (!is.null(slot(object, slotName))) {
            tmp <- slot(object, slotName)
            if (is(tmp, "data.table")) {
                tmp <- tmp[chromosome %in% chromosomeNames]
                if (dropLevels) {
                    tmp[, chromosome := droplevels(chromosome)]
                }
            } else {
                if (!is(tmp, "GRanges")) {
                    stop("malformed HiCDOCDataSet")
                }
                if (dropLevels) {
                    Seqinfo::seqlevels(
                        tmp,
                        pruning.mode = "coarse"
                    ) <- object@chromosomes
                } else {
                    tmp <- tmp[S4Vectors::`%in%`(tmp@seqnames, chromosomeNames)]
                }
            }
            slot(object, slotName)  <- tmp
        }
    }
    return(object)
}

#' @description
#' Reduces a \code{\link{HiCDOCDataSet}} by keeping only given conditions.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param conditionNames
#' The names of conditions to keep.
#' @param dropLevels
#' Whether or not to also remove unused factor levels after filtering. Should
#' be set to FALSE if the reduced objects are meant to be re-combined later.
#' Defaults to TRUE.
#'
#' @return
#' A reduced \code{\link{HiCDOCDataSet}}.
#'
#' @keywords internal
#' @noRd
.reduceHiCDOCConditions <- function(object, conditionNames, dropLevels) {
    conditionIds <- which(object$condition %in% conditionNames)
    object <- object[, conditionIds]

    object@validAssay <- .determineValids(object)
    for (slotName in c(
        "distances",
        "selfInteractionRatios",
        "compartments",
        "concordances",
        "centroids"
    )) {
        if (!is.null(slot(object, slotName))) {
            tmp <- slot(object, slotName)
            if (is(tmp, "data.table")) {
                tmp <- tmp[condition %in% conditionNames]
                if (dropLevels) {
                    tmp[, condition := droplevels(condition)]
                }
            } else {
                if (!is(tmp, "GRanges")) {
                    stop("malformed HiCDOCDataSet")
                }
                tmp <- tmp[tmp$condition %in% conditionNames]
                if (dropLevels) {
                    tmp$condition <- droplevels(tmp$condition)
                }
            }
            slot(object, slotName)  <- tmp
        }
    }
    return(object)
}

#' @description
#' Reduces a \code{\link{HiCDOCDataSet}} by keeping only given replicates.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param replicateNames
#' The names of replicates to keep.
#' @param dropLevels
#' Whether or not to also remove unused factor levels after filtering. Should
#' be set to FALSE if the reduced objects are meant to be re-combined later.
#' Defaults to TRUE.
#'
#' @return
#' A reduced \code{\link{HiCDOCDataSet}}.
#'
#' @keywords internal
#' @noRd
.reduceHiCDOCReplicates <- function(object, replicateNames, dropLevels) {
    replicateIds <- which(object$replicate %in% replicateNames)
    object <- object[, replicateIds]

    object@validAssay <- .determineValids(object)


    for (slotName in c(
        "distances",
        "selfInteractionRatios",
        "concordances"
    )) {
        if (!is.null(slot(object, slotName))) {
            tmp <- slot(object, slotName)
            if (is(tmp, "data.table")) {
                tmp <- tmp[replicate %in% replicateNames]
                if (dropLevels) {
                    tmp[, replicate := droplevels(replicate)]
                }
            } else {
                if (!is(tmp, "GRanges")) {
                    stop("malformed HiCDOCDataSet")
                }
                tmp <- tmp[tmp$replicate %in% replicateNames]
                if (dropLevels) {
                    tmp$replicate <- droplevels(tmp$replicate)
                }
            }
            slot(object, slotName)  <- tmp
        }
    }
    return(object)
}

#' @title
#' Reduce a \code{\link{HiCDOCDataSet}}.
#'
#' @description
#' Reduces a \code{\link{HiCDOCDataSet}} by keeping only given chromosomes,
#' conditions, or replicates.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param chromosomes
#' The chromosome names or indices in \code{chromosomes(object)} to keep.
#' Defaults to NULL.
#' @param conditions
#' The condition names in \code{sampleConditions(object)} to keep. 
#' Defaults to NULL.
#' @param replicates
#' The replicate names in \code{sampleReplicates(object)} to keep. 
#' Defaults to NULL.
#' @param dropLevels
#' Whether or not to also remove unused factor levels after filtering. Should
#' be set to FALSE if the reduced objects are meant to be re-combined later.
#' Defaults to TRUE.
#'
#' @return
#' A reduced \code{\link{HiCDOCDataSet}}.
#'
#' @examples
#' data(exampleHiCDOCDataSet)
#' reduced <- reduceHiCDOCDataSet(exampleHiCDOCDataSet, chromosomes = c(1, 2))
#'
#' @export
reduceHiCDOCDataSet <- function(
    object,
    chromosomes = NULL,
    conditions = NULL,
    replicates = NULL,
    dropLevels = TRUE
) {
    if (!is.null(object@differences)) {
        warning(
            "You should not reduce a HiCDOCDataSet after calling ",
            "'detectCompartments()'. All chromosomes, conditions, and ",
            "replicates have been used in the computations.",
            call. = FALSE
        )
    }

    if (!is.null(chromosomes)) {
        chromosomeNames <- .validateNames(object, chromosomes, "chromosomes")
        object <- .reduceHiCDOCChromosomes(object, chromosomeNames, dropLevels)
    }

    if (!is.null(conditions)) {
        conditionNames <- .validateNames(object, conditions, "conditions")
        object <- .reduceHiCDOCConditions(object, conditions, dropLevels)
    }

    if (!is.null(replicates)) {
        replicateNames <- .validateNames(object, replicates, "replicates")
        object <- .reduceHiCDOCReplicates(object, replicates, dropLevels)
    }

    return(object)
}
