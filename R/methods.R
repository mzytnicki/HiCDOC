#### chromosomes ####
#' Retrieves the vector of chromosome names.
#' @rdname HiCDOCDataSet-methods
#' @usage
#' NULL
#' @export
setMethod("chromosomes", "HiCDOCDataSet", function(object) object@chromosomes)

#### positions ####
#' Retrieves the genomic positions corresponding to bins for each chromosome.
#' @rdname HiCDOCDataSet-methods
#' @usage
#' NULL
#' @export
setMethod("positions", "HiCDOCDataSet", function(object) object@positions)

#### conditions ####
#' Retrieves the vector of condition names.
#' @rdname HiCDOCDataSet-methods
#' @usage
#' NULL
#' @export
setMethod("conditions", "HiCDOCDataSet", function(object) object@conditions)

#### replicates ####
#' Retrieves the vector of replicate names.
#' @rdname HiCDOCDataSet-methods
#' @usage
#' NULL
#' @export
setMethod("replicates", "HiCDOCDataSet", function(object) object@replicates)

#### binSize ####
#' Retrieves the resolution (span of each position in number of bases).
#' @rdname HiCDOCDataSet-methods
#' @usage
#' NULL
#' @export
setMethod("binSize", "HiCDOCDataSet", function(object) object@binSize)

#### interactions ####
#' Retrieves a tibble of the interactions.
#' @rdname HiCDOCDataSet-methods
#' @usage
#' NULL
#' @export
setMethod("interactions", "HiCDOCDataSet", function(object) {
    if (is.null(object@interactions)) return(NULL)

    interactions <-
        object@interactions %>%
        dplyr::left_join(
            object@positions %>% dplyr::select(
                chromosome,
                bin.1 = bin,
                position.1 = start,
            ),
            by = c("chromosome", "bin.1")
        ) %>%
        dplyr::left_join(
            object@positions %>% dplyr::select(
                chromosome,
                bin.2 = bin,
                position.2 = start,
            ),
            by = c("chromosome", "bin.2")
        ) %>%
        dplyr::select(
            chromosome,
            position.1,
            position.2,
            condition,
            replicate,
            interaction
        )

    return(interactions)
})

#### compartments ####
#' Retrieves a \code{GenomicRange} of the compartment of every position
#' @rdname HiCDOCDataSet-methods
#' @usage
#' NULL
#' @export
setMethod("compartments", "HiCDOCDataSet", function(object) {

    if (is.null(object@compartments)) return(NULL)

    compartments <-
        object@compartments %>%
        dplyr::left_join(object@positions, by = c("chromosome", "bin")) %>%
        dplyr::select(
            chromosome,
            start,
            end,
            condition,
            compartment
        ) %>%
        dplyr::arrange(chromosome, condition, start, end) %>%
        dplyr::mutate(
            consecutive = (
                start - dplyr::lag(end) == 1 &
                dplyr::lag(chromosome) == chromosome &
                dplyr::lag(condition) == condition &
                dplyr::lag(compartment) == compartment
            ),
            consecutive = tidyr::replace_na(consecutive, TRUE),
            switching = dplyr::if_else(consecutive, 0, 1),
            group = cumsum(switching)
        ) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(start = min(start), end = max(end)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-consecutive, -switching, -group) %>%
        GenomicRanges::makeGRangesFromDataFrame(keep.extra.columns = TRUE)

    return(compartments)
})

#### differences ####
#' Retrieves a \code{GenomicRange} of the significant compartment differences
#' @rdname HiCDOCDataSet-methods
#' @usage
#' NULL
#' @export
setMethod("differences", "HiCDOCDataSet", function(object, threshold = NULL) {

    if (is.null(object@differences)) return(NULL)

    if (
        !is.null(threshold) &&
        (!is.numeric(threshold) || length(threshold) > 1)
    ) {
        stop(
            "'threshold' must be a number.",
            call. = FALSE
        )
    }

    differences <-
        object@differences %>%
        dplyr::left_join(
            object@positions,
            by = c("chromosome", "bin")
        ) %>%
        dplyr::mutate(
            significance = dplyr::case_when(
                pvalue.adjusted <= 0.0001 ~ "****",
                pvalue.adjusted <= 0.001 ~ "***",
                pvalue.adjusted <= 0.01 ~ "**",
                pvalue.adjusted <= 0.05 ~ "*",
                TRUE ~ ""
            )
        ) %>%
        dplyr::select(
            chromosome,
            start,
            end,
            condition.1,
            condition.2,
            pvalue,
            pvalue.adjusted,
            direction,
            significance
        )

    if (!is.null(threshold)) {
        differences %<>% dplyr::filter(pvalue.adjusted <= threshold)
    }

    if (nrow(differences) == 0) {
        if (is.null(threshold)){
            message("No differences found.")
        } else {
            message(
                "No differences found with adjusted p-value <= ",
                threshold,
                "."
            )
        }
        return(NULL)
    }

    genomicRange <-
        GenomicRanges::makeGRangesFromDataFrame(
            differences,
            keep.extra.columns = TRUE
        )

    return(genomicRange)
})

#### concordances ####
#' Retrieves a \code{GenomicRange} of the concordance (confidence in assigned
#' compartment) of every position in every replicate.
#' @rdname HiCDOCDataSet-methods
#' @usage
#' NULL
#' @export
setMethod("concordances", "HiCDOCDataSet", function(object) {

    if (is.null(object@concordances)) return(NULL)

    concordances <-
        object@concordances %>%
        dplyr::left_join(object@positions, by = c("chromosome", "bin")) %>%
        dplyr::select(
            chromosome,
            start,
            end,
            condition,
            replicate,
            compartment,
            concordance
        ) %>%
        GenomicRanges::makeGRangesFromDataFrame(keep.extra.columns = TRUE)

    return(concordances)
})

#### show ####
#' @describeIn HiCDOCDataSet-methods
#' Describes the object and its methods.
#' @usage
#' NULL
#' @export
setMethod("show", "HiCDOCDataSet", function(object) {

    cat(
        "Object of class 'HiCDOCDataSet'\n\n",
        "- Inputs:\n",
        paste0(
            "  ",
            sapply(object@input, function(x) paste0(x)),
            "\n"
        ),
        "\n",
        "- Chromosomes:\n  ",
        if (is.null(object@chromosomes) || length(object@chromosomes) == 0)
        "None"
        else
        paste(object@chromosomes, collapse = ", "),
        "\n\n",
        "- Replicates:\n",
        if (is.null(object@replicates) || length(object@replicates) == 0)
        "  None\n"
        else
        paste0(
            "  condition ",
            object@conditions,
            ", replicate ",
            object@replicates,
            "\n"
        ),
        "\n",
        "- Resolution (bin size):\n  ",
        if (is.null(object@binSize))
        "None"
        else
        object@binSize,
        "\n\n",
        "- Parameters:\n",
        paste0(
          "  ",
          sapply(
            1:length(parameters(object)),
            function(x) {
              paste(
                names(parameters(object))[x],
                '=',
                parameters(object)[x]
              )
            }
          ),
          "\n"
        ),
        "\n",
        "- Methods:\n",
        "  chromosomes(object)\n",
        "  conditions(object)\n",
        "  replicates(object)\n",
        "  binSize(object)\n",
        "  interactions(object)\n",
        "  compartments(object)\n",
        "  differences(object)\n",
        "  concordances(object)\n",
        "  parameters(object)\n",
        "  parameters(object) <- list()\n\n",
        sep = ""
    )
})

#### parameters ####
#' Access the parameters of a \code{\link{HiCDOCDataSet}}.
#' @rdname HiCDOCDataSet-parameters
#' @usage
#' NULL
#' @export
setMethod("parameters", "HiCDOCDataSet", function(object) object@parameters)

#### parameters<- ####
#' Change the parameters of a \code{\link{HiCDOCDataSet}}.
#' @rdname HiCDOCDataSet-parameters
#' @usage
#' NULL
#' @export
setReplaceMethod("parameters", "HiCDOCDataSet", function(object, value) {

    defaultParameterNames <- names(defaultHiCDOCParameters)

    if (!is(value, "list")) {
        stop(
            "'parameters' must be a named list.\n",
            "No parameters were updated. ",
            "See 'help(parameters)' for details.",
            call. = FALSE
        )
    }

    parameterNames <- names(value)

    duplicatedParameterNames <-
        unique(parameterNames[duplicated(parameterNames)])

    if (length(duplicatedParameterNames) > 0) {
        stop(
            "Duplicate parameter",
            if (length(duplicatedParameterNames) != 1) "s",
            " provided: ",
            paste(duplicatedParameterNames, collapse = ", "),
            "\nNo parameters were updated. ",
            "See 'help(parameters)' for details.",
            call. = FALSE
        )
    }

    invalidParameterNames <-
        parameterNames[!(parameterNames %in% defaultParameterNames)]

    if (length(invalidParameterNames) > 0) {
        stop(
            "Invalid parameter",
            if (length(invalidParameterNames) != 1) "s",
            " provided: ",
            paste(invalidParameterNames, collapse = ", "),
            "\nNo parameters were updated. ",
            "See 'help(parameters)' for details.",
            call. = FALSE
        )
    }

    object@parameters[parameterNames] <- value

    return(object)
})
