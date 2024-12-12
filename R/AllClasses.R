#### HiCDOCDataSet class definition ####
#' @title
#' \code{HiCDOCDataSet} S4 class.
#'
#' @name
#' HiCDOCDataSet-class
#' 
#' @aliases
#' HiCDOCDataSet
#' 
#' @description
#' Data structure for a Hi-C experiment.
#'
#' @details
#' An instance of \code{HiCDOCDataSet} describes a Hi-C experiment with slots
#' for path(s) to input file(s), interactions, pipeline parameters defaulting to
#' \code{defaultHiCDOCParameters}, and computation results. It can be
#' constructed from 4 different types of data:
#' - Tabular files: see \code{\link{HiCDOCDataSetFromTabular}}
#' - (m)Cool files: see \code{\link{HiCDOCDataSetFromCool}}
#' - HiC files: see \code{\link{HiCDOCDataSetFromHiC}}
#' - HiC-Pro matrices and bed files: see \code{\link{HiCDOCDataSetFromHiCPro}}
#' An example \code{HiCDOCDataSet} is also available, see
#' \code{\link{exampleHiCDOCDataSet}}.
#' The \code{HiCDOCDataSet} object can be explored using the appropriate
#' accessors.
#' 
#' @details # Accessors
#' The accessors for a HiCDOCDataset object are the following:
#' - \code{\link{chromosomes}} to retrieve the vector of chromosome names.
#' - \code{\link{sampleConditions}} to  retrieve the vector of condition names, 
#' one for each sample.
#' - \code{\link{sampleReplicates}} to retrieve the vector of replicate names, 
#' one for each sample.
#' 
#' After the detection of compartments you can use this accessors:
#' - \code{\link{compartments}} returns a GenomicRange of the compartment 
#' of every position in every condition.
#' - \code{\link{concordances}} returns a GenomicRange of the significant 
#' compartment differences between conditions, and their p-values.
#' - \code{\link{differences}} returns a GenomicRange of the concordance 
#' (confidence in assigned compartment) of every position in every replicate.
#' 
#' 
#' See the \link{HiCDOCDataSet-methods} man page for more details on methods
#' and accessors. 
#'
#' @seealso
#' \code{\link{HiCDOC}},
#' \code{\link{exampleHiCDOCDataSet}},
#' \code{\link{HiCDOCDataSetFromTabular}},
#' \code{\link{HiCDOCDataSetFromCool}},
#' \code{\link{HiCDOCDataSetFromHiC}},
#' \code{\link{HiCDOCDataSetFromHiCPro}}
#'
#' @md
#' @export
setClass(
    "HiCDOCDataSet",
    contains = "InteractionSet",
    slots = c(
        input = "ANY",
        parameters = "ANY",
        chromosomes = "ANY",
        totalBins = "ANY",
        weakBins = "ANY",
        validAssay = "ANY",
        compartments = "ANY",
        concordances = "ANY",
        differences = "ANY",
        comparisons = "ANY",
        distances = "ANY",
        centroids = "ANY",
        selfInteractionRatios = "ANY",
        checks = "ANY"
    )
)

#' @rdname HiCDOCDataSet-parameters
#' @name HiCDOCDataSet-parameters
#' @export
defaultHiCDOCParameters <- list(
    smallChromosomeThreshold = 100,
    sparseReplicateThreshold = 0.3,
    weakPositionThreshold = 1,
    cyclicLoessSpan = NA_real_,
    loessSampleSize = 20000,
    kMeansDelta = 0.0001,
    kMeansIterations = 50,
    kMeansRestarts = 20,
    PC1CheckThreshold = 0.75
)

#' @title
#' \code{\link{HiCDOCDataSet}} constructor from a tabular file.
#'
#' @description
#' Constructs a \code{\link{HiCDOCDataSet}} from a tabular file.
#'
#' @details
#' Accepts a tabular file with \code{chromosome}, \code{position 1},
#' \code{position 2}, and multiple replicate columns listing interaction counts.
#' Null interactions do not have to be listed. Values must be separated by
#' tabulations. The header must be
#' \code{chromosome position 1 position 2 x.y x.y x.y ...} with \code{x}
#' replaced by condition names and \code{y} replaced by replicate names.
#'
#' @param path
#' A path to a tabular file.
#' @param sep
#' The separator of the tabular file. Default to tabulation.
#'
#' @return
#' A \code{\link{HiCDOCDataSet}}.
#'
#' @examples
#' path <- system.file("extdata", "liver_18_10M_500000.tsv", package = "HiCDOC")
#' object <- HiCDOCDataSetFromTabular(path, sep = '\t')
#'
#' @usage
#' HiCDOCDataSetFromTabular(path, sep = '\t')
#'
#' @export
HiCDOCDataSetFromTabular <- function(
    path = NULL,
    sep = "\t"
) {

    if (!is.character(path) || length(path) > 1) {
        stop("'paths' must be a string of characters.", call. = FALSE)
    }
    if (!file.exists(path)) {
        stop("'", path, "' does not exist.", call. = FALSE)
    }

    object <- .parseTabular(path, sep = sep)
    object <- .fillHiCDOCDataSet(object)
    return(invisible(object))
}

#' @title
#' \code{\link{HiCDOCDataSet}} constructor from Cool files.
#'
#' @description
#' Constructs a \code{\link{HiCDOCDataSet}} from a set of \code{.cool} or
#' \code{.mcool} files.
#'
#' @param paths
#' A vector of paths to \code{.cool} or \code{.mcool} files.
#' @param replicates
#' A vector of replicate names repeated along the conditions.
#' @param conditions
#' A vector of condition names repeated along the replicates.
#' @param binSize
#' The resolution (span of each position in number of bases). Optionally
#' provided to select the appropriate resolution in \code{.mcool} files.
#' Defaults to NULL.
#'
#' @return
#' A \code{\link{HiCDOCDataSet}}.
#'
#' @examples
#' \dontrun{
#'     # Path to each file
#'     paths = c(
#'       'path/to/condition-1.replicate-1.cool',
#'       'path/to/condition-1.replicate-2.cool',
#'       'path/to/condition-2.replicate-1.cool',
#'       'path/to/condition-2.replicate-2.cool',
#'       'path/to/condition-3.replicate-1.cool'
#'     )
#'
#'     # Replicate and condition of each file. Can be names instead of numbers.
#'     replicates <- c(1, 2, 1, 2, 1)
#'     conditions <- c(1, 1, 2, 2, 3)
#'
#'     # Resolution to select in .mcool files
#'     binSize = 500000
#'
#'     # Instantiation of data set
#'     object <- HiCDOCDataSetFromCool(
#'       paths,
#'       replicates = replicates,
#'       conditions = conditions,
#'       binSize = binSize # Specified for .mcool files.
#'     )
#' }
#'
#' @export
HiCDOCDataSetFromCool <- function(
    paths,
    replicates,
    conditions,
    binSize = NA
) {
    if(!requireNamespace('rhdf5')) stop("'rhdf5' package is required. Please install it and retry.")
    if (is.factor(paths)) {
        paths <- as.vector(paths)
    }
    if (!is.character(paths)) {
        stop("'paths' must be a vector of characters.", call. = FALSE)
    }
    for (path in paths) {
        if (!file.exists(path)) {
            stop("'", path, "' does not exist.", call. = FALSE)
        }
    }

    if (is.factor(replicates)) {
        conditions <- as.vector(replicates)
    }
    if (is.null(replicates)) {
        stop("'replicates' must be a vector of replicates.", call. = FALSE)
    }

    if (is.factor(conditions)) {
        conditions <- as.vector(conditions)
    }
    if (is.null(conditions)) {
        stop("'conditions' must be a vector of conditions.", call. = FALSE)
    }

    if (!is.na(binSize) && (!is.numeric(binSize) || length(binSize) != 1)) {
        stop("'binSize' must be an integer.", call. = FALSE)
    }

    object <- new("HiCDOCDataSet")
    object@input <- paths
    object <- .parseCool(object, binSize, replicates, conditions)
    object <- .fillHiCDOCDataSet(object)
    return(invisible(object))
}

#' @title
#' \code{\link{HiCDOCDataSet}} constructor from HiC files.
#'
#' @description
#' Constructs a \code{\link{HiCDOCDataSet}} from a set of
#' \code{.hic} files.
#'
#' @param paths
#' A vector of paths to \code{.hic} files.
#' @param replicates
#' A vector of replicate names repeated along the conditions.
#' @param conditions
#' A vector of condition names repeated along the replicates.
#' @param binSize
#' The resolution (span of each position in number of bases) to select within
#' the \code{.hic} files.
#'
#' @return
#' A \code{\link{HiCDOCDataSet}}.
#'
#' @examples
#' \dontrun{
#'     #' # Path to each file
#'     paths = c(
#'       'path/to/condition-1.replicate-1.hic',
#'       'path/to/condition-1.replicate-2.hic',
#'       'path/to/condition-2.replicate-1.hic',
#'       'path/to/condition-2.replicate-2.hic',
#'       'path/to/condition-3.replicate-1.hic'
#'     )
#'
#'     # Replicate and condition of each file. Can be names instead of numbers.
#'     replicates <- c(1, 2, 1, 2, 1)
#'     conditions <- c(1, 1, 2, 2, 3)
#'
#'     # Resolution to select
#'     binSize <- 500000
#'
#'     # Instantiation of data set
#'     hic.experiment <- HiCDOCDataSetFromHiC(
#'       paths,
#'       replicates = replicates,
#'       conditions = conditions,
#'       binSize = binSize
#'     )
#' }
#'
#' @usage
#' HiCDOCDataSetFromHiC(paths, replicates, conditions, binSize)
#'
#' @export
HiCDOCDataSetFromHiC <- function(
    paths = NULL,
    replicates = NULL,
    conditions = NULL,
    binSize = NULL
) {

    if (is.factor(paths)) {
        paths <- as.vector(paths)
    }
    if (!is.character(paths)) {
        stop("'paths' must be a vector of characters.", call. = FALSE)
    }
    for (path in paths) {
        if (!file.exists(path)) {
            stop("'", path, "' does not exist.", call. = FALSE)
        }
    }

    if (is.factor(replicates)) {
        replicates <- as.vector(replicates)
    }
    if (is.null(replicates)) {
        stop("'replicates' must be a vector of replicates.", call. = FALSE)
    }
    if (length(replicates) != length(paths)) {
        stop("'replicates' should have the same length as 'paths'")
    }

    if (is.factor(conditions)) {
        conditions <- as.vector(conditions)
    }
    if (is.null(conditions)) {
        stop("'conditions' must be a vector of conditions.", call. = FALSE)
    }
    if (length(conditions) != length(paths)) {
        stop("'conditions' should have the same length as 'paths'")
    }

    if (!is.numeric(binSize) || length(binSize) != 1) {
        stop("'binSize' must be an integer.", call. = FALSE)
    }
    binSize <- as.integer(binSize)

    object <- new("HiCDOCDataSet")
    object@input <- paths
    object <- .parseHiC(object, binSize, replicates, conditions)
    object <- .fillHiCDOCDataSet(object)
    return(invisible(object))
}

#' @title
#' \code{\link{HiCDOCDataSet}} constructor from HiC-Pro files.
#'
#' @description
#' Constructs a \code{\link{HiCDOCDataSet}} from a set of HiC-Pro generated
#' files.
#'
#' @param matrixPaths
#' A vector of paths to HiC-Pro matrix files.
#' @param bedPaths
#' A vector of paths to HiC-Pro bed files.
#' @param replicates
#' A vector of replicate names repeated along the conditions.
#' @param conditions
#' A vector of condition names repeated along the replicates.
#'
#' @return
#' A \code{\link{HiCDOCDataSet}}.
#'
#' @examples
#' \dontrun{
#'     # Path to each matrix file
#'     matrixPaths = c(
#'       'path/to/condition-1.replicate-1.matrix',
#'       'path/to/condition-1.replicate-2.matrix',
#'       'path/to/condition-2.replicate-1.matrix',
#'       'path/to/condition-2.replicate-2.matrix',
#'       'path/to/condition-3.replicate-1.matrix'
#'     )
#'
#'     # Path to each bed file
#'     bedPaths = c(
#'       'path/to/condition-1.replicate-1.bed',
#'       'path/to/condition-1.replicate-2.bed',
#'       'path/to/condition-2.replicate-1.bed',
#'       'path/to/condition-2.replicate-2.bed',
#'       'path/to/condition-3.replicate-1.bed'
#'     )
#'
#'     # Replicate and condition of each file. Can be names instead of numbers.
#'     replicates <- c(1, 2, 1, 2, 1)
#'     conditions <- c(1, 1, 2, 2, 3)
#'
#'     # Instantiation of data set
#'     hic.experiment <- HiCDOCDataSetFromHiCPro(
#'       matrixPaths = matrixPaths,
#'       bedPaths = bedPaths,
#'       replicates = replicates,
#'       conditions = conditions
#'     )
#' }
#'
#' @usage
#' HiCDOCDataSetFromHiCPro(matrixPaths, bedPaths, replicates, conditions)
#'
#' @export
HiCDOCDataSetFromHiCPro <- function(
    matrixPaths = NULL,
    bedPaths = NULL,
    replicates = NULL,
    conditions = NULL
) {

    if (is.factor(matrixPaths)) {
        matrixPaths <- as.vector(matrixPaths)
    }
    if (!is.character(matrixPaths)) {
        stop("'matrixPaths' must be a vector of characters.", call. = FALSE)
    }

    if (is.factor(bedPaths)) {
        bedPaths <- as.vector(bedPaths)
    }
    if (!is.character(bedPaths)) {
        stop("'bedPaths' must be a vector of characters.", call. = FALSE)
    }

    if (length(matrixPaths) != length(bedPaths)) {
        stop(
            "'matrixPaths' and 'bedPaths' must have the same length.",
             call. = FALSE
        )
    }

    paths <-
        base::split(
            base::cbind(matrixPaths, bedPaths),
            seq(length(matrixPaths))
        )

    for (path in unlist(paths)) {
        if (!file.exists(path)) {
            stop("'", path, "' does not exist.", call. = FALSE)
        }
    }

    if (is.factor(replicates)) {
        replicates <- as.vector(replicates)
    }
    if (is.null(replicates)) {
        stop("'replicates' must be a vector of replicates.", call. = FALSE)
    }

    if (is.factor(conditions))
        conditions <- as.vector(conditions)
    if (is.null(conditions)) {
        stop("'conditions' must be a vector of conditions.", call. = FALSE)
    }

    if (length(conditions) != length(replicates)) {
        stop(
            "'conditions' and 'replicates' must have the same length",
             call. = FALSE
        )
    }

    object <- new("HiCDOCDataSet")
    object@input <- paths
    object <- .parseHiCPro(object, replicates, conditions)
    object <- .fillHiCDOCDataSet(object)
    return(invisible(object))
}

#' @title
#' Default pipeline to run on the HiCDOC analysis.
#'
#' @description
#' Runs the default filtering, normalization, and computational steps on a
#' \code{HiCDOCDataSet}. To learn more about HiCDOC, browse the vignette:
#' \code{browseVignettes(package = "HiCDOC")}.
#'
#' @details
#' \subsection{\code{HiCDOC} pipeline}{
#'     The HiCDOC pipeline has seven steps: \\
#'     \describe{
#'         Three filtering steps:
#'         \itemize{
#'             \item{\code{\link{filterSmallChromosomes}}} to filter out 
#'             small chromosomes
#'             \item{\code{\link{filterWeakPositions}}} to filter out weak 
#'             positions with very few interactions
#'             \item{\code{\link{filterSparseReplicates}}} to filter out 
#'             sparse replicates with many null interactions
#'         }
#'         Three normalization steps:
#'         \itemize{
#'             \item{\code{\link{normalizeTechnicalBiases}}} to normalize 
#'             technical biases in each replicates
#'             \item{\code{\link{normalizeBiologicalBiases}}} to normalize 
#'             biological biases in each replicate
#'             \item{\code{\link{normalizeDistanceEffect}}} to normalize the 
#'             distance effect in each chromosome
#'         }
#'         One computational step:
#'         \itemize{
#'             \item{\code{\link{detectCompartments}}} to detect compartments 
#'             in each condition and find significant changes between 
#'             conditions.
#'         }
#'     }
#' }
#' \subsection{Parallel processing}{
#' The parallel version of HiCDOC uses the
#' \code{\link{BiocParallel}} package. Before to call the
#' function in parallel you should specify the parallel parameters such as:
#'     \itemize{
#'         \item{On Linux:}
#'            \code{multiParam <- BiocParallel::MulticoreParam(workers = 10)}
#'          \item{On Windows:}
#'              \code{multiParam <- BiocParallel::SnowParam(workers = 10)}
#'     }
#'     And then you can register the parameters to be used by BiocParallel: \\
#'     \code{BiocParallel::register(multiParam, default = TRUE)}
#'
#'     You should be aware that using MulticoreParam, reproducibility of the
#'     detectCompartments function using a RNGseed may not work. See this
#'     \href{https://github.com/Bioconductor/BiocParallel/issues/122}{issue}
#'     for more details.
#' }
#'
#' @param object
#' A \code{HiCDOCDataSet}.
#' @param parallel
#' Whether or not to parallelize each step. Defaults to FALSE.
#'
#' @return
#' A HiCDOCDataSet with all slots filled.
#'
#' @seealso
#' \code{\link{HiCDOCDataSet}}, \code{\link{filterSmallChromosomes}},
#' \code{\link{filterWeakPositions}}, \code{\link{filterSparseReplicates}},
#' \code{\link{normalizeTechnicalBiases}},
#' \code{\link{normalizeBiologicalBiases}},
#' \code{\link{normalizeDistanceEffect}},
#' \code{\link{detectCompartments}}
#'
#' @examples
#' data(exampleHiCDOCDataSet)
#' # Default HiCDOC pipeline
#' # Not printing loess warnings for example purpose. 
#' # Results should be inspected if there is any.
#' suppressWarnings(
#' object <- HiCDOC(exampleHiCDOCDataSet)
#' )
#' 
#' # Equivalent to
#' if(FALSE){
#'     object <- filterSmallChromosomes(exampleHiCDOCDataSet)
#'     object <- filterSparseReplicates(object)
#'     object <- filterWeakPositions(object)
#'     object <- normalizeTechnicalBiases(object)
#'     object <- normalizeBiologicalBiases(object)
#'     object <- normalizeDistanceEffect(object)
#'     object <- detectCompartments(object)
#' }
#'
#' @export
HiCDOC <- function(
    object,
    parallel = FALSE
) {

    object <- filterSmallChromosomes(object)
    object <- filterSparseReplicates(object)
    object <- filterWeakPositions(object)
    object <- normalizeTechnicalBiases(object, parallel = parallel)
    object <- normalizeBiologicalBiases(object, parallel = parallel)
    object <- normalizeDistanceEffect(object)
    object <- detectCompartments(object, parallel = parallel)
    return(invisible(object))
}
