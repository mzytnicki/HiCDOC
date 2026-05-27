#' @title
#' Methods to access a \code{\link{HiCDOCDataSet}} components.
#'
#' @name
#' HiCDOCDataSet-methods
#'
#' @description
#' Retrieve information and results from a \code{\link{HiCDOCDataSet}}.
#' 
#' @examples
#' # Load an example dataset already processed 
#' # (i.e. after the detection of compartments)
#' data(exampleHiCDOCDataSetProcessed)
#' 
#' exampleHiCDOCDataSetProcessed
#' chromosomes(exampleHiCDOCDataSetProcessed)
#' sampleConditions(exampleHiCDOCDataSetProcessed)
#' sampleReplicates(exampleHiCDOCDataSetProcessed)
#' compartments(exampleHiCDOCDataSetProcessed)
#' differences(exampleHiCDOCDataSetProcessed)
#' concordances(exampleHiCDOCDataSetProcessed)
#' 
#' @return
#' A character vector (for \code{chromosomes}, \code{sampleConditions},
#' \code{sampleReplicates}), 
#' or a GRanges object
#' (for \code{compartments}, \code{concordances}, \code{differences}).
NULL

#' @describeIn HiCDOCDataSet-methods
#' Retrieves the vector of chromosome names.
#' @usage
#' NULL
#' @export
setGeneric(
    name = "chromosomes",
    def = function(object) standardGeneric("chromosomes")
)

#### sampleConditions ####
#' @describeIn HiCDOCDataSet-methods
#' Retrieves the vector of condition names, one for each sample.
#' @usage
#' NULL
#' @export
setGeneric(
    name = "sampleConditions",
    def = function(object) standardGeneric("sampleConditions")
)

#### sampleReplicates ####
#' @describeIn HiCDOCDataSet-methods
#' Retrieves the vector of replicate names, one for each sample.
#' @usage
#' NULL
#' @export
setGeneric(
    name = "sampleReplicates",
    def = function(object) standardGeneric("sampleReplicates")
)

#### compartments ####
#' @describeIn HiCDOCDataSet-methods
#' Retrieves a \code{GenomicRange} of the compartment of every position
#' in every condition.
#' @param passChecks logical. Display only the concordances/compartments for 
#' the chromosomes passing sanity checks.
#' @usage
#' NULL
#' @export
setGeneric(
    name = "compartments",
    def = function(object, passChecks = TRUE) standardGeneric("compartments")
)

#### differences ####
#' @describeIn HiCDOCDataSet-methods
#' Retrieves a \code{GenomicRange} of the significant compartment differences
#' between conditions, and their p-values.
#' @usage
#' NULL
#' @param object
#' a HiCDOCDataSet object
#' @param threshold
#' a numeric value between 0 and 1. If no threshold, all the differences will
#' be printed even the non significant ones. Otherwise the differences printed
#' are filtered to show the ones with an adjusted p-value <= \code{threshold}.
#' @export
#' @usage
#' NULL
setGeneric(
    name = "differences",
    def = function(object, threshold = 0.05) standardGeneric("differences")
)

#### concordances ####
#' @describeIn HiCDOCDataSet-methods
#' Retrieves a \code{GenomicRange} of the concordance (confidence in assigned
#' compartment) of every position in every replicate.
#' @usage
#' NULL
#' @export
setGeneric(
    name = "concordances",
    def = function(object, passChecks = TRUE) standardGeneric("concordances")
)

#' @title
#' Access the parameters of a \code{\link{HiCDOCDataSet}}.
#
#' @name
#' HiCDOCDataSet-parameters
#' 
#' @description
#' Retrieves or sets parameters used for filtering, normalization, and
#' prediciton of compartments.
#'
#' @details
#' A \code{\link{HiCDOCDataSet}}'s parameters are automatically set to default
#' values retrieved from \code{\link{defaultHiCDOCParameters}}. They are
#' accessed by filtering, normalization, and compartment detection functions.
#' If those functions are called with custom arguments, the object's
#' parameters are updated to record the actual parameters used. If the
#' object's parameters are customized before calling the functions, the
#' custom parameters will be used.
#'
#' See
#' \code{\link{filterSmallChromosomes}},
#' \code{\link{filterSparseReplicates}},
#' \code{\link{filterWeakPositions}},
#' \code{\link{normalizeDistanceEffect}}, and
#' \code{\link{detectCompartments}},
#' for details on how these parameters are used.

#' \subsection{All parameters are listed here:}{
#'     \describe{
#'         \item{\code{smallChromosomeThreshold}}{
#'             The minimum length (number of positions) for a chromosome to be
#'             kept when filtering with \code{\link{filterSmallChromosomes}}.
#'             Defaults to
#'             \code{defaultHiCDOCParameters$smallChromosomeThreshold} = 0.
#'         }
#'         \item{\code{sparseReplicateThreshold}}{
#'             The minimum percentage of non-zero interactions for a chromosome
#'             replicate to be kept when filtering with
#'             \code{\link{filterSparseReplicates}}. If a chromosome replicate's
#'             percentage of non-zero interactions is lower than this value, it
#'             is removed. Defaults to
#'             \code{defaultHiCDOCParameters$sparseReplicateThreshold} = 0.3.
#'         }
#'         \item{\code{weakPositionThreshold}}{
#'             The minimum average interaction for a position to be kept when
#'             filtering with \code{\link{filterWeakPositions}}. If a position's
#'             average interaction with the entire chromosome is lower than this
#'             value in any of the replicates, it is removed from all replicates
#'             and conditions. Defaults to
#'             \code{defaultHiCDOCParameters$weakPositionThreshold} = 1.
#'         }
#'         \item{\code{cyclicLoessSpan}}{
#'             The span for cyclic loess normalization used in 
#'             \code{\link{normalizeTechnicalBiases}}. This value is passed to 
#'             \code{multiHiCcompare::cyclic_loess}. 
#'             Defaults to `NA` indicating that span will be automatically 
#'             calculated using generalized cross validation.
#'             For large dataset, it is highly recommended to set this value
#'             to reduce computing time and necessary memory.
#'         }
#'         \item{\code{loessSampleSize}}{
#'             The number of positions used as a sample to estimate the effect
#'             of distance on proportion of interactions when normalizing with
#'             \code{\link{normalizeDistanceEffect}}. Defaults to
#'             \code{defaultHiCDOCParameters$loessSampleSize} = 20000.
#'         }
#'         \item{\code{kMeansDelta}}{
#'             The convergence stop criterion for the clustering when detecting
#'             compartments with \code{\link{detectCompartments}}. When the
#'             centroids' distances between two iterations is lower than this
#'             value, the clustering stops. Defaults to
#'             \code{defaultHiCDOCParameters$kMeansDelta} = 0.0001.
#'         }
#'         \item{\code{kMeansIterations}}{
#'             The maximum number of iterations during clustering when detecting
#'             compartments with \code{\link{detectCompartments}}. Defaults to
#'             \code{defaultHiCDOCParameters$kMeansIterations} = 50.
#'         }
#'         \item{\code{kMeansRestarts}}{
#'             The amount of times the clustering is restarted when detecting
#'             compartments with \code{\link{detectCompartments}}. For each
#'             restart, the clustering iterates until convergence or reaching
#'             the maximum number of iterations. The clustering that minimizes
#'             inner-cluster variance is selected. Defaults to
#'             \code{defaultHiCDOCParameters$kMeansRestarts} = 20.
#'         }
#'         \item{\code{PC1CheckThreshold}}{
#'             The minimum percentage of variance that should be explained by
#'             the first principal component of centroids to pass sanity check.
#'             Defaults to 
#'             \code{defaultHiCDOCParameters$PC1CheckThreshold} = 0.75.
#'         }
#'     }
#' }
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#'
#' @examples
#' data(exampleHiCDOCDataSet)
#'
#' # Retrieve parameters
#' parameters(exampleHiCDOCDataSet)
#'
#' # Set parameters
#' parameters(exampleHiCDOCDataSet) <- list("smallChromosomeThreshold" = 50)
#' parameters(exampleHiCDOCDataSet) <- list(
#'     "weakPositionThreshold" = 10,
#'     "kMeansRestarts" = 30
#' )
NULL

#### parameters ####
#' @rdname HiCDOCDataSet-parameters
#' @usage NULL
#' @export
setGeneric(
    name = "parameters",
    def = function(object) standardGeneric("parameters")
)

#### parameters <- ####
#' @rdname HiCDOCDataSet-parameters
#' @usage NULL
#' @param value a named list containing the names and valued of the
#' parameters to change (see Details).
#' @export
setGeneric(
    name = "parameters<-",
    def = function(object, value) standardGeneric("parameters<-")
)
