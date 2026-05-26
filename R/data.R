#' @title
#' Example HiCDOCDataSet.
#'
#' @description
#' A S4 HiCDOCDataSet object with 4 chromosomes, 3 conditions and 3 replicates.
#'
#' @format
#' S4 HiCDOCDataSet object with the following characteristics:
#' \describe{
#'   \item{chromosomes}{4 chromosomes: W, X, Y, Z}
#'   \item{conditions}{3 conditions: 1, 2, 3}
#'   \item{replicates}{3 replicates: R1, R2, R3}
#'   \item{binSize}{A resolution of 137 bases}
#' }
#'
#' @usage data(exampleHiCDOCDataSet)
#'
#' @return
#' A \code{\link{HiCDOCDataSet}}.
#'
#' @examples
#' data(exampleHiCDOCDataSet)
#' exampleHiCDOCDataSet
"exampleHiCDOCDataSet"


#' @title
#' Example HiCDOCDataSet, filtered, normalized and with compartements detected.
#'
#' @description
#' A S4 HiCDOCDataSet object with 3 chromosomes, 3 conditions and 3 replicates.
#' Can be retrieved by running :
#' \code{data(exampleHiCDOCDataSet); 
#' set.seed(123); 
#' parameters(exampleHiCDOCDataSet) <- list(
#'   smallChromosomeThreshold = 100,
#'   sparseReplicateThreshold = 0.3,
#'   weakPositionThreshold = 1,
#'   cyclicLoessSpan = NA_real_,
#'   loessSampleSize = 20000,
#'   kMeansDelta = 0.0001,
#'   kMeansIterations = 50,
#'   kMeansRestarts = 20,
#'   PC1CheckThreshold = 0.75
#' )
#' exampleHiCDOCDataSetProcessed <- HiCDOC(exampleHiCDOCDataSet)}
#'
#' @format
#' S4 HiCDOCDataSet object with the following characteristics:
#' \describe{
#'   \item{chromosomes}{4 chromosomes: X, Y, Z}
#'   \item{conditions}{3 conditions: 1, 2, 3}
#'   \item{replicates}{3 replicates: R1, R2, R3}
#'   \item{binSize}{A resolution of 137 bases}
#' }
#'
#' @usage data(exampleHiCDOCDataSetProcessed)
#'
#' @return
#' A \code{\link{HiCDOCDataSet}}, already filtered and normalized.
#'
#' @examples
#' data(exampleHiCDOCDataSetProcessed)
#' exampleHiCDOCDataSetProcessed
"exampleHiCDOCDataSetProcessed"
