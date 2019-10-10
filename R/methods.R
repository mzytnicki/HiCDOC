##- interactionMatrix --------------------------------------------------------#
##----------------------------------------------------------------------------#
#' Accessors for the 'interaction' slot of an HiCDOCExp object
#'
#' The \code{interaction} slot contains the (transformed) interaction profiles.
#'
#' @docType methods
#' @name interactionMatrix
#' @rdname interactionMatrix
#' @aliases interactionMatrix interactinMatrix,HiCDOCExp-method
#' @param object An \code{HiCDOCExp} object.
#' @return A tibble
#' @examples
#' exp <- HiCDOCExample()
#' interactionMatrix(exp)
#'
#' @export
setMethod(f = "interactionMatrix", signature = "HiCDOCExp",
          definition = function(object) {
              object@interactionMatrix
          }
)


##- DIR ----------------------------------------------------------------------#
##----------------------------------------------------------------------------#
#' Extracts differentially interacting regions of an HiCDOCExp object
#'
#' This function extracts the differentially interacting regions from
#' \code{\link{HiCDOCExp}}.
#'
#' @docType methods
#' @name DIR
#' @rdname DIR
#'
#' @aliases DIR DIR,HiCDOCExp-method
#'
#' @param object An \code{HiCDOCExp} object.
#' @param pvalue Numeric cutoff value for adjusted p-values. Only regions with
#'               adjusted p-values equal or lower than specified are returned.
#'               Default to 1, all regions are returned.
#'
#' @return A \code{GenomicRanges} object of the selected differentially
#' interacting regions.
#'
#' @examples
#' exp <- HiCDOCExample()
#' exp <- HiCDOC(exp)
#' DIR(exp, pvalue = 0.05)
#'
#' @export
setMethod(f = "DIR", signature = "HiCDOCExp",
            definition = function(object, pvalue=1) {
                if (is.null(object@DIR)) {
                    message("No 'DIR' slot found in the HiCDOCExp",
                            " object. Run HiCDOC first.")
                }
                else if (length(object@DIR) == 0) {
                    message("No 'DIR' found.")
                    return(GRanges())
                }
                else {
                    ##- checking input value ---------------------------------#
                    ##--------------------------------------------------------#
                    if (length(pvalue) != 1) {
                        stop("'pvalue' must be a single value.", call. = FALSE)
                    }

                    if (is.null(pvalue) || !is.numeric(pvalue) ||
                        !is.finite(pvalue)) {
                        stop("'pvalue' value must be numeric.", call. = FALSE)
                    }

                    if ((pvalue > 1) || (pvalue < 0)) {
                        stop("'pvalue' value ", pvalue, ", outside the",
                            " interval [0,1].", call. = FALSE)
                    }
                    ##- end check -------------------------------------------#

                    gr <- object@DIR %>%
                        filter(abs(padj) <= pvalue) %>%
                        mutate(start = start + 1)
                    if (nrow(gr) == 0) {
                        message(paste0("No 'DIR' found at p-value ",
                                       pvalue,
                                       ": best is: ",
                                       min(abs(object@DIR$padj)),
                                       "."))
                        return(GRanges())
                    }
                    return (makeGRangesFromDataFrame(gr,
                                                     keep.extra.columns = TRUE,
                                                     ignore.strand = TRUE))
                }
            }
)


##- concordances -------------------------------------------------------------#
##----------------------------------------------------------------------------#
#' Extracts concordances
#'
#' This function extracts the concordances from \code{\link{HiCDOCExp}}.
#'
#' @docType methods
#' @name concordances
#' @rdname concordances
#'
#' @aliases concordances concordances,HiCDOCExp-method
#'
#' @param object An \code{HiCDOCExp} object.
#'
#' @return A \code{tibble} object of the concordance
#'
#' @examples
#' exp <- HiCDOCExample()
#' exp <- HiCDOC(exp)
#' concordances(exp)
#'
#' @export
setMethod(f = "concordances", signature = "HiCDOCExp",
            definition = function(object) {
                if (is.null(object@concordances)) {
                    message("No 'concordances' slot found in the HiCDOCExp",
                            " object. Run HiCDOC first.")
                } else {
                    return(object@concordances)
                }
            }
)


##- compartments -------------------------------------------------------------#
##----------------------------------------------------------------------------#
#' Extracts compartments
#'
#' This function extracts the compartments from \code{\link{HiCDOCExp}}.
#'
#' @docType methods
#' @name compartments
#' @rdname compartments
#'
#' @aliases compartments compartments,HiCDOCExp-method
#'
#' @param object An \code{HiCDOCExp} object.
#'
#' @return A \code{tibble} object of the concordance
#'
#' @examples
#' exp <- HiCDOCExample()
#' exp <- HiCDOC(exp)
#' compartments(exp)
#'
#' @export
setMethod(f = "compartments", signature = "HiCDOCExp",
            definition = function(object) {
                if (is.null(object@compartments)) {
                    message("No 'compartments' slot found in the HiCDOCExp",
                            " object. Run HiCDOC first.")
                } else {
                    #grl <- object@compartments %>%
                    grl <- object@compartments %>%
                        mutate(start = position + 1) %>%
                        mutate(end = start + object@binSize - 1) %>%
                        select(-c(position)) %>%
                        mutate(condition = factor(condition)) %>%
                        rename(compartment = value) %>%
                        makeGRangesListFromDataFrame(keep.extra.columns = TRUE,
                                                     ignore.strand = TRUE,
                                                     split.field = "condition")
                    grl2 <- lapply(grl, function(x) { split(x, ~ compartment) })
                    grl3 <- as(lapply(grl2,
                               function(x) {
                                   unlist(as(
                                       lapply(names(x), function(y) {
                                           z <- GenomicRanges::reduce(x[[y]])
                                           z$compartment <- factor(y)
                                           return(z)
                                       })
                                       , "GRangesList"))
                               }), "GRangesList")
                    return(grl3)
                }
            }
)





##- parameters ---------------------------------------------------------------#
##----------------------------------------------------------------------------#
#' Accessors for the 'parameters' slot of an HiCDOCExp object
#'
#' The \code{parameters} slot holds the parameter values
#' used in an experiment as a named \code{list}. Default values
#' exist for parameters, but these can also be supplied as input
#' values in the \code{useParameters} argument of the \code{\link{HiCDOC}}
#' function or using the assignment function \code{\link{parameters<-}}.
#'
#' Parameters in a HiCDOC experment.
#'
#' \subsection{Global parameters}{
#'    \describe{
#'        \item{\code{minDepth}}{The cutoff to filter the base-level
#'              coverage. Bases where at least one sample has (normalized)
#'              coverage greater than \code{minDepth} be been retained.
#'              Default to \code{10}.}
#'        \item{\code{minSize}}{The minimum size (in base-pairs) of the
#'              regions to be found. Default to \code{18}.}
#'        \item{\code{maxSize}}{The maximum size (in base-pairs) of the
#'              regions to be found. Default to \code{1000000}.}
#'        \item{\code{minGap}}{The minimum gap between regions. Regions
#'              separated by a gap of at most \code{minGap} positions
#'              are merged. Default to \code{100}.}
#'        \item{\code{maxDiff}}{The maximum number of different bases between
#'              two regions. Near-identical regions are collapsed.
#'              Only regions with at most \code{maxDiff} different
#'              positions are considered identicals and are collapsed
#'              into one single region. Default to \code{20}.}
#'        \item{\code{minOverlap}}{This parameters is used in the construction
#'              of the \code{\link{countMatrix}} matrix. Only reads (ranges)
#'              with a minimum of \code{minOverlap} overlapping each expressed
#'              region are considered to be overlapping. Default to \code{10}.}
#'    }
#' }
#'
#' \subsection{Parameters for the HMM method}{
#'    \describe{
#'        \item{\code{noDiffToDiff}}{Initial transition probability from
#'              no differentially expressed state to differentially expressed.
#'              Default to \code{0.001}.}
#'        \item{\code{diffToNoDiff}}{Initial transition probability from
#'              differentially expressed state to no differentially expressed.
#'              Default to \code{0.000001}.}
#'        \item{\code{emission}}{Emission probability. Default to \code{0.9}.}
#'        \item{\code{emissionThreshold}}{Emission threshold. A real number
#'              between \code{0} and \code{1}. Default to \code{0.1}.}
#'    }
#' }
#'
#' \subsection{Parameters for the Naive and Slice methods}{
#'    \describe{
#'        \item{\code{cutoff}}{The cutoff used in the naive method to
#'              determine candidate regions. Default to \code{1}.}
#'        \item{\code{minLogFC}}{The minimun sliding threshold used in the
#'              Slice method. Default to \code{0.5}.}
#'    }
#' }
#'
#' @docType methods
#' @name parameters
#' @rdname parameters
#' @aliases parameters parameters,HiCDOCExp-method
#' parameters<- parameters<-,HiCDOCExp-method
#' @param object An \code{HiCDOCExp} object.
#' @param value  A named \code{list} containing valid parameters. See details.
#' @return The named list of the parameters used in the analysis.
#' @seealso
#' \code{useParameters} argument in \code{\link{HiCDOC}} function.
#' @examples
#' exp <- HiCDOCExample()
#' exp <- HiCDOC(exp)
#' print(parameters(exp))
#'
#' parameters(srnaExp) <- list("minSize" = 1, "maxSize" = 1500)
#'
#' @export
setMethod(f = "parameters", signature = "HiCDOCExp",
            definition = function(object) {
                if (is.null(object@parameters)) {
                    message("No 'parameters' slot found in the HiCDOCExp",
                            " object. Run HiCDOC first or assign a named",
                            " list of valid parameters. See help(parameters)",
                            " for details.")
                } else {
                    object@parameters
                    class(object@parameters) <- "HiCDOCParameters"
                    return(invisible(object@parameters))
                }
            }
)

#' @name parameters
#' @rdname parameters
#' @exportMethod "parameters<-"
setReplaceMethod("parameters",
                signature(object = "HiCDOCExp", value = "ANY"),
                function(object, value) {

                    ##- checking input value ---------------------------------#
                    ##--------------------------------------------------------#
                     defaultParNames <- names(HiCDOCDefaultParameters)

                     if (!is.null(object@parameters)) {
                         HiCDOCDefaultParameters <- object@parameters
                     }

                     if (!is(value, "list")) {
                         print(value)
                         print(typeof(value))
                         print(class(value))
                         stop("'value' must be a named list. See",
                             " help(parameters) for details.", call. = FALSE)
                     }

                     valueNames <- names(value)

                     if (any(duplicated(valueNames))) {
                         stop("duplicate name parameters in 'value'. See",
                              " help(parameters) for details.", call. = FALSE)
                     }

                     if (!all(valueNames %in% defaultParNames)) {
                         stop("'value' must be a named list of valid",
                            " parameters. See help(parameters) for details.",
                            call. = FALSE)
                     }

                     ##- individual parameters
                     HiCDOCDefaultParameters[valueNames] <- value
                     checkParameters(HiCDOCDefaultParameters)

                     ##- end check -------------------------------------------#

                     object@parameters <- HiCDOCDefaultParameters
                     object
                 }
)

##- show ---------------------------------------------------------------------#
##----------------------------------------------------------------------------#
#' @rdname HiCDOCExp
#' @param object An \code{HiCDOCExp} object.
#' @return The \code{show} method informatively display object contents.
#' @export
setMethod(f = "show", signature = "HiCDOCExp",
            definition = function(object) {
                cat("Object of class HiCDOCExp.\n",
                    "Sample information\n")
                print("TODO")
            }
)


##- print method for parameters ----------------------------------------------#
##----------------------------------------------------------------------------#
#' Dispatch print method for the parameters used by an \code{HiCDOC} object.
#'
#' @docType methods
#' @name parameters
#' @rdname parameters
#' @aliases parameters parameters,HiCDOCExp-method
#' @param x The first element of the parameters used by an \code{HiCDOC}
#'          object
#' @param ... The other elements of the parameters
#' @examples
#' exp <- HiCDOCExample()
#' exp <- HiCDOC(srnaExp)
#' print(parameters(exp))
#'
#' @export
printHiCDOCParameters <- function(x, ...) {

    cat("\n Global parameters: \n",
        "------------------ \n")
    df <- data.frame(value = unlist(x[1:6]))
    print(df)

    cat("\n Constrained K-means parameters: \n",
        "---------------------- \n")
    df <- data.frame(value = unlist(x[7:10]))
    print(df)
}