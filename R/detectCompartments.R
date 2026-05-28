#' @description
#' Computes the euclidean distance between two vectors.
#'
#' @param x
#' A vector.
#' @param y
#' A vector.
#'
#' @return
#' A float number.
#'
#' @keywords internal
#' @noRd
.euclideanDistance <- function(x, y) {
    sqrt(sum((x - y) ^ 2))
}

#' @description
#' Computes the log ratio of the distance of a position to each centroid.
#'
#' @param x
#' The vector of a genomic position.
#' @param centroids
#' A list of two vectors.
#' @param eps
#' A small float number to avoid log(0).
#'
#' @return
#' A float number.
#'
#' @keywords internal
#' @noRd
.distanceRatio <- function(x, centroids) {
    epsilon = .euclideanDistance(centroids[[1]], centroids[[2]]) * 1e-10
    return(
        log(
            (
                .euclideanDistance(x, centroids[[1]]) + epsilon
            ) / (
                .euclideanDistance(x, centroids[[2]]) + epsilon
            )
        )
    )
}

#' @description
#' Assigns correct cluster labels by comparing centroids across conditions.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#'
#' @return
#' A \code{\link{HiCDOCDataSet}} with corrected cluster labels in compartments,
#' concordances, distances and centroids.
#'
#' @keywords internal
#' @noRd
.tieCentroids <- function(object) {

    validChromosomeNames <- names(
        base::Filter(
            function(x) !is.null(x),
            object@validAssay
        )
    )

    referenceConditionNames <- vapply(
        validChromosomeNames,
        FUN = function(x) sort(object$condition[object@validAssay[[x]]])[1],
        FUN.VALUE = ""
    )

    referenceCentroids <- data.table::merge.data.table(
        object@centroids[
            cluster == 1 &
            condition == referenceConditionNames[chromosome],
            .(chromosome, reference.1 = centroid)
        ],
        object@centroids[
            cluster == 2 &
            condition == referenceConditionNames[chromosome],
            .(chromosome, reference.2 = centroid)
        ],
        all = TRUE
    )

    clusters <- data.table::merge.data.table(
        object@centroids[
            cluster == 1,
            .(chromosome, condition, centroid.1 = centroid)
        ],
        object@centroids[
            cluster == 2,
            .(chromosome, condition, centroid.2 = centroid)
        ],
        all = TRUE
    )

    clusters <- data.table::merge.data.table(
        clusters,
        referenceCentroids,
        all = TRUE
    )

    c1_r1 <- mapply(
        function(x, y) .euclideanDistance(unlist(x), unlist(y)),
        clusters$centroid.1,
        clusters$reference.1
    )
    c1_r2 <- mapply(
        function(x, y) .euclideanDistance(unlist(x), unlist(y)),
        clusters$centroid.1,
        clusters$reference.2
    )
    c2_r1 <- mapply(
        function(x, y) .euclideanDistance(unlist(x), unlist(y)),
        clusters$centroid.2,
        clusters$reference.1
    )
    c2_r2 <- mapply(
        function(x, y) .euclideanDistance(unlist(x), unlist(y)),
        clusters$centroid.2,
        clusters$reference.2
    )

    clusters[, cluster.1 := 1 * ((c1_r1 * c2_r2) >= (c1_r2 * c2_r1)) + 1]
    clusters[, cluster.2 := 1 + (cluster.1 == 1)]

    clusters <- clusters[, .(chromosome, condition, cluster.1, cluster.2)]

    object@compartments <- data.table::merge.data.table(
        object@compartments,
        clusters,
        by = c("chromosome", "condition"),
        all.x = TRUE,
        sort = FALSE
    )

    object@compartments[
        ,
        cluster := ifelse(
            cluster == 1,
            cluster.1,
            cluster.2
        )
    ]

    object@compartments[, `:=`(cluster.1 = NULL, cluster.2 = NULL)]

    object@concordances <- data.table::merge.data.table(
        object@concordances,
        clusters,
        by = c("chromosome", "condition"),
        all.x = TRUE,
        sort = FALSE
    )

    object@concordances[, change := -1]
    object@concordances[
        cluster == 1 & cluster == cluster.1,
        change := 1
    ]
    object@concordances[
        cluster == 2 & cluster == cluster.2,
        change := 1
    ]
    object@concordances[, concordance := change * concordance]

    object@concordances[, compartment := data.table::fifelse(
        cluster == 1,
        cluster.1,
        cluster.2
    )]

    object@concordances[, `:=`(
        cluster.1 = NULL,
        cluster.2 = NULL,
        change = NULL
    )]

    object@distances <- data.table::merge.data.table(
        object@distances,
        clusters,
        by = c("chromosome", "condition"),
        all.x = TRUE,
        sort = FALSE
    )
    object@distances[, cluster := data.table::fifelse(
        cluster == 1,
        cluster.1,
        cluster.2
    )]
    object@distances[, `:=`(cluster.1 = NULL, cluster.2 = NULL)]

    object@centroids <- data.table::merge.data.table(
        object@centroids,
        clusters,
        by = c("chromosome", "condition"),
        all.x = TRUE,
        sort = FALSE
    )

    object@centroids[, cluster := data.table::fifelse(
        cluster == 1,
        cluster.1,
        cluster.2
    )]
    object@centroids[, `:=`(cluster.1 = NULL, cluster.2 = NULL)]

    return(object)
}

#' @description
#' Constructs a link matrix of interaction rows to be clustered together.
#'
#' @param totalReplicates
#' The number of replicates.
#' @param totalBins
#' The number of bins.
#'
#' @return
#' A matrix, each row holding the row indices of interactions to be clustered
#' together.
#'
#' @keywords internal
#' @noRd
.constructLinkMatrix <- function(totalReplicates, totalBins) {
    return(
        matrix(
            rep(0:(totalReplicates - 1), totalBins) * totalBins +
            rep(0:(totalBins - 1), each = totalReplicates),
            nrow = totalBins,
            byrow = TRUE
        )
    )
}

#' @description
#' Segregates positions of a chromosome into two clusters using constrained
#' K-means.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param chromosomeName
#' The name of a chromosome.
#' @param conditionName
#' The name of a condition.
#'
#' @return
#' A list of:
#' - The compartment (cluster number) of each position.
#' - The concordance (float) of each genomic position in each replicate.
#' - The distances to centroids (float) of each position in each replicate.
#' - The centroid (vector) of each cluster.
#'
#' @md
#' @keywords internal
#' @noRd
.clusterizeChromosomeCondition <- function(object) {
    chromosomeName <- object@chromosomes
    conditionName <- object$condition[1]

    totalBins <- length(InteractionSet::regions(object))
    validAssay <- object@validAssay[[chromosomeName]]
    if (length(validAssay) == 0) {
        return(NULL)
    }
    replicateNames <- object$replicate[validAssay]
    orderedAssay <- validAssay[order(replicateNames)]

    chromosomeInteractionSet <- InteractionSet::InteractionSet(
        SummarizedExperiment::assay(object),
        InteractionSet::interactions(object)
    )
    matrixAssay <- lapply(
        orderedAssay,
        FUN = function(x) {
            InteractionSet::inflate(
                chromosomeInteractionSet,
                rows = chromosomeName,
                columns = chromosomeName,
                sample = x
            )
        }
    )
    matrixAssay <- lapply(
        matrixAssay,
        function(x) x@matrix
    )
    matrixAssay <- do.call("rbind", matrixAssay)
    matrixAssay[is.na(matrixAssay)] <- 0

    mustLink <- .constructLinkMatrix(length(replicateNames), totalBins)
    clusteringOutput <- constrainedClustering(
        matrixAssay,
        mustLink,
        object@parameters$kMeansDelta,
        object@parameters$kMeansIterations,
        object@parameters$kMeansRestarts
    )
    # TODO : question : pourquoi on ne prend que les premiers ?
    # Quel est l'intérêt de retourner 2 fois ?
    clusters <- as.integer(clusteringOutput[["clusters"]][0:totalBins] + 1)
    centroids <- clusteringOutput[["centroids"]]

    min <- .distanceRatio(centroids[[1]], centroids)
    max <- .distanceRatio(centroids[[2]], centroids)

    concordances <- apply(
        matrixAssay,
        1,
        function(row) {
            2 * (.distanceRatio(row, centroids) - min) / (max - min) - 1
        }
    )

    distances <- apply(
        matrixAssay,
        1,
        function(row) {
            c(
                .euclideanDistance(row, centroids[[1]]),
                .euclideanDistance(row, centroids[[2]])
            )
        }
    )

    indices <- S4Vectors::mcols(InteractionSet::regions(object))$index

    dfCompartments <- data.table::data.table(
        "chromosome" = chromosomeName,
        "index" = indices,
        "condition" = conditionName,
        "cluster" = clusters
    )

    dfConcordances <- data.table::data.table(
        "chromosome" = chromosomeName,
        "index" = rep(indices, length(replicateNames)),
        "condition" = conditionName,
        "replicate" = rep(sort(replicateNames), each = totalBins),
        "cluster" = rep(clusters, length(replicateNames)),
        "concordance" = concordances
    )

    dfDistances <- data.table::data.table(
        "chromosome" = chromosomeName,
        "index" = rep(indices, 2 * length(replicateNames)),
        "condition" = conditionName,
        "replicate" = rep(rep(sort(replicateNames), each = totalBins), 2),
        "cluster" = rep(c(1, 2), each = length(replicateNames) * totalBins),
        "distance" = c(t(distances))
    )

    dfCentroids <- data.table::data.table(
        "chromosome" = chromosomeName,
        "condition" = conditionName,
        "cluster" = c(1, 2),
        "centroid" = centroids
    )

    return(
        list(
            "compartments" = dfCompartments,
            "concordances" = dfConcordances,
            "distances" = dfDistances,
            "centroids" = dfCentroids
        )
    )
}

#' @description
#' Runs the clustering to detect compartments in each chromosome and condition.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param parallel
#' Whether or not to parallelize the processing. Defaults to FALSE.
#'
#' @return
#' A \code{\link{HiCDOCDataSet}} with compartments, concordances, distances
#' and centroids.
#'
#' @keywords internal
#' @noRd
.clusterize <- function(object, parallel = FALSE) {
    conditionsPerChromosome <- lapply(
        object@chromosomes,
        FUN = function(x) {
            data.frame(
                "chromosome" = x,
                "condition" = sort(unique(
                    object$condition[object@validAssay[[x]]]
                ))
            )
        }
    )
    conditionsPerChromosome <- Reduce(rbind, conditionsPerChromosome)

    reducedObjects <- mapply(
        function(x, y) {
            reduceHiCDOCDataSet(
                object,
                chromosomes = x,
                conditions = y,
                dropLevels = TRUE
            )
        },
        conditionsPerChromosome$chromosome,
        conditionsPerChromosome$condition
    )

    result <- .internalLapply(
        parallel,
        reducedObjects,
        .clusterizeChromosomeCondition
    )

    compartments <- lapply(
        result,
        function(x) x[["compartments"]]
    )
    concordances <- lapply(
        result,
        function(x) x[["concordances"]]
    )
    distances <- lapply(
        result,
        function(x) x[["distances"]]
    )
    centroids <- lapply(
        result,
        function(x) x[["centroids"]]
    )

    object@compartments <- data.table::rbindlist(compartments)
    object@concordances <- data.table::rbindlist(concordances)
    object@distances <- data.table::rbindlist(distances)
    object@centroids <- data.table::rbindlist(centroids)

    return(object)
}

#' @description
#' Computes the ratio of self interaction vs median of other interactions for
#' each position.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param chromosomeName
#' The name of a chromosome.
#' @param conditionName
#' The name of a condition.
#' @param replicateName
#' The name of a replicate.
#'
#' @return
#' A data.table
#'
#' @keywords internal
#' @noRd
.computeSelfInteractionRatios <- function(object) {
    ids <- InteractionSet::anchors(object, id = FALSE)
    diagonal <- (ids$first == ids$second)
    ids <- lapply(ids, as.data.table)
    columnNames <- paste(object$condition, object$replicate)

    matrixAssay <- SummarizedExperiment::assay(object)
    colnames(matrixAssay) <- columnNames

    # Values on diagonal
    onDiagonal <- data.table::data.table(
        ids$first[diagonal,.(chromosome = seqnames, index)],
        matrixAssay[diagonal, , drop=FALSE]
    )
    onDiagonal <- data.table::melt.data.table(
        onDiagonal,
        id.vars = c("chromosome", "index"),
        value.name = "ratio",
        na.rm = TRUE
    )
    # Compute median by bin, out of diagonal
    offDiagonal <- data.table::data.table(
        "chromosome" = c(ids$first$seqnames[!diagonal], 
                         ids$second$seqnames[!diagonal]),
        "index" = c(ids$first$index[!diagonal], 
                    ids$second$index[!diagonal]),
        matrixAssay[!diagonal, , drop=FALSE]
    )
    offDiagonal <- data.table::melt.data.table(
        offDiagonal,
        id.vars = c("chromosome", "index"),
        value.name = "offDiagonal",
        variable.name = "variable",
        na.rm = TRUE
    )
    offDiagonal <- offDiagonal[!is.na(offDiagonal)]
    offDiagonal <- offDiagonal[
        ,
        .(offDiagonal = sum(offDiagonal, na.rm=TRUE)),
        by = c("chromosome", "index", "variable")
    ]
    # Ratio is value on diagonal - median (off diagonal), by bin
    onDiagonal <- data.table::merge.data.table(
        onDiagonal,
        offDiagonal,
        all = TRUE,
        by = c("chromosome", "index", "variable"),
        sort = FALSE
    )
    #Shoudn't happen after normalizations
    onDiagonal[is.na(ratio) & !is.na(offDiagonal),ratio := 0] 
    onDiagonal[, c("condition", "replicate") := data.table::tstrsplit(
        variable,
        " ",
        fixed = TRUE
    )]
    onDiagonal <- onDiagonal[, .(
        chromosome,
        index,
        condition,
        replicate,
        ratio,
        offDiagonal
    )]

    return(onDiagonal)
}

#' @description
#' Uses ratio between self interactions and other interactions to determine
#' which clusters correspond to compartments A and B.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#' @param parallel
#' Whether or not to parallelize the processing. Defaults to FALSE.
#'
#' @return
#' A \code{\link{HiCDOCDataSet}}, with selfInteractionRatios, and with A and B
#' labels replacing cluster numbers in compartments, concordances, distances,
#' and centroids.
#'
#' @keywords internal
#' @noRd
.predictCompartmentsAB <- function(object, parallel = FALSE) {
    ratios <- .computeSelfInteractionRatios(object)
    object@selfInteractionRatios <- ratios

    compartments <- data.table::merge.data.table(
        object@compartments,
        object@selfInteractionRatios,
        by = c("chromosome", "index", "condition"),
        all.x = TRUE,
        sort = FALSE
    )
    compartments[, offDiagonal := NULL]
    compartments[, ratio := as.numeric(ratio)]
    compartments <- compartments[
        ,
        .(ratio = stats::median(ratio, na.rm = TRUE)),
        by = .(chromosome, cluster)
    ]
    compartments <- data.table::dcast(
        compartments,
        chromosome ~ cluster,
        value.var = "ratio",
        fill = 0
    )

    compartments[, A := data.table::fifelse(`1` >= `2`, 1, 2)]
    compartments <- compartments[, .(chromosome, A)]

    object@compartments <- data.table::merge.data.table(
        object@compartments,
        compartments,
        by = "chromosome",
        all.x = TRUE
    )

    object@compartments[, compartment := factor(
        data.table::fifelse(cluster == A, "A", "B"), 
        levels=c("A", "B"))]
    object@compartments[, A := NULL]

    object@concordances <- data.table::merge.data.table(
        object@concordances,
        compartments,
        by = "chromosome",
        all.x = TRUE
    )
    object@concordances[, change := data.table::fifelse(A == 1, 1,-1)]
    object@concordances[, concordance :=  change * concordance]
    object@concordances[, compartment := factor(
        data.table::fifelse(cluster == A, "A", "B"),
        levels = c("A", "B")
    )]
    object@concordances[, change := NULL]
    object@concordances[, A := NULL]

    object@distances <- data.table::merge.data.table(
        object@distances,
        compartments,
        by = "chromosome",
        all.x = TRUE
    )
    object@distances[, compartment := factor(
        data.table::fifelse(cluster == A, "A", "B"),
        levels = c("A", "B")
    )]
    object@distances[, A := NULL]

    object@centroids <- data.table::merge.data.table(
        object@centroids,
        compartments,
        by = "chromosome",
        all.x = TRUE
    )
    object@centroids[, compartment := factor(
        data.table::fifelse(cluster == A, "A", "B"),
        levels = c("A", "B")
    )]
    object@centroids[, A := NULL]

    return(object)
}

#' @description
#' Computes p-values for genomic positions whose assigned compartment switches
#' between two conditions.
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#'
#' @return
#' A \code{\link{HiCDOCDataSet}}, with differences and their p-values.
#'
#' @keywords internal
#' @noRd
.computePValues <- function(object) {
    # Compute median of differences between pairs of concordances
    # N.b. median of differences != difference of medians
    totalReplicates <- length(object$replicate)
    concordances <- object@concordances
    concordances[, condition := factor(
        condition,
        levels = sort(unique(object$condition))
    )]
    data.table::setorder(concordances, chromosome, index, condition, replicate)
    concordances1 <- concordances[
        rep(seq_len(nrow(concordances)), each = totalReplicates),
        .(
            chromosome,
            index,
            condition.1 = condition,
            concordance.1 = concordance
        )
    ]
    concordances2 <- concordances[
        rep(seq_len(nrow(concordances)), totalReplicates),
        .(
             chromosome = chromosome,
             index = index,
             condition.2 = condition,
             concordance.2 = concordance
        )
    ]
    data.table::setorder(concordances2, chromosome, index)
    concordances2[, `:=`(chromosome = NULL, index = NULL)]
    concordanceDifferences <- base::cbind(concordances1, concordances2)
    rm(concordances1, concordances2)
    concordanceDifferences <- concordanceDifferences[
        as.numeric(condition.1) < as.numeric(condition.2)
    ]
    concordanceDifferences <- concordanceDifferences[,
        .(difference = stats::median(abs(concordance.1 - concordance.2))),
        by = .(chromosome, index, condition.1, condition.2)
    ]

    # Format compartments per pair of conditions
    # Join medians of differences and pairs of conditions
    totalConditions <- length(unique(object$condition))
    compartments <- object@compartments
    compartments[, condition := factor(
        condition,
        levels = sort(unique(object$condition))
    )]
    data.table::setorder(compartments, chromosome, index, condition)
    compartments1 <- compartments[
        rep(seq_len(nrow(compartments)), each = totalConditions),
        .(
            chromosome,
            index,
            condition.1 = condition,
            compartment.1 = compartment
        )
    ]
    compartments2 <- compartments[
        rep(seq_len(nrow(compartments)), totalConditions),
        .(
            chromosome,
            index,
            condition.2 = condition,
            compartment.2 = compartment
        )
    ]
    data.table::setorder(compartments2, chromosome, index)
    compartments2[, `:=`(chromosome = NULL, index = NULL)]
    comparisons <- base::cbind(compartments1, compartments2)
    rm(compartments1, compartments2)
    comparisons <- comparisons[
        as.numeric(condition.1) < as.numeric(condition.2)
    ]
    if(nrow(comparisons) == 0){
        object@comparisons <- comparisons[,.(chromosome,
                                             index, 
                                             condition.1, 
                                             condition.2,
                                             compartment.1,
                                             compartment.2,
                                             difference = index)]
        object@differences <- comparisons[,.(chromosome, 
                                             index, 
                                             condition.1,
                                             condition.2,
                                             pvalue = index,
                                             pvalue.adjusted = index,
                                             direction = compartment.1)]
        return(object)
    }
    comparisons <- data.table::merge.data.table(
        comparisons,
        concordanceDifferences,
        by = c("chromosome", "index", "condition.1", "condition.2")
    )
    data.table::setcolorder(
        comparisons,
        c(
            "chromosome",
            "index",
            "condition.1",
            "condition.2",
            "compartment.1",
            "compartment.2",
            "difference"
        )
    )
    object@comparisons <- comparisons

    # Compute p-values for switching positions
    # P-values for a condition pair computed from the whole genome distribution
    differences <- copy(comparisons)
    differences[compartment.1 == compartment.2 , H0_value := difference]
    data.table::setorder(differences, condition.1, condition.2)
    quantiles <- split(
        differences,
        by = c("condition.1", "condition.2")
    )
    quantiles <- lapply(
        quantiles,
        function(x) x[difference > 0]
    )
    quantiles <- lapply(
        quantiles,
        function(x) {
            if (nrow(x) > 0) return(stats::ecdf(x$H0_value)(x$difference))
            return(NULL)
        }
    )
    quantiles <- do.call("c", quantiles)
    differences[difference > 0, quantile := quantiles]

    # Pvalues
    differences <- differences[compartment.1 != compartment.2]
    differences[, pvalue := 1 - quantile]
    differences[pvalue < 0, pvalue := 0]
    differences[pvalue > 1, pvalue := 1]
    pvalueAdjusted <- split(
        differences,
        by = c("condition.1", "condition.2")
    )
    pvalueAdjusted <- lapply(
        pvalueAdjusted,
        function(x) {
            if (nrow(x) > 0) return(stats::p.adjust(x$pvalue, method = "BH"))
            return(NULL)
        }
    )
    pvalueAdjusted <- do.call("c", pvalueAdjusted)
    differences[, pvalue.adjusted := pvalueAdjusted]

    # Changes
    differences[, direction := data.table::fifelse(
        compartment.1 == "A", "A->B", "B->A"
    )]
    differences[, direction := factor(direction, levels = c("A->B", "B->A"))]
    differences <- differences[, .(
        chromosome,
        index,
        condition.1,
        condition.2,
        pvalue,
        pvalue.adjusted,
        direction
    )]
    data.table::setorder(
        differences,
        chromosome,
        index,
        condition.1,
        condition.2
    )
    object@differences <- differences
    return(object)
}

#' @title
#' A and B compartments detection and differences across conditions.
#'
#' @description
#' Detects compartments for each genomic position in each condition, and
#' computes p-values for compartment differences between conditions.
#'
#' @details
#' \subsection{Genomic positions clustering}{
#' To clusterize genomic positions, the algorithm follows these steps:
#'     \enumerate{
#'         \item{
#'             For each chromosome and condition, get the interaction vectors of
#'             each genomic position. Each genomic position can have multiple
#'             interaction vectors, corresponding to the multiple replicates in
#'             that condition.
#'         }
#'         \item{
#'             For each chromosome and condition, use constrained K-means to
#'             clusterize the interaction vectors, forcing replicate interaction
#'             vectors into the same cluster. The euclidean distance between
#'             interaction vectors determines their similarity.
#'         }
#'         \item{
#'             For each interaction vector, compute its concordance, which is
#'             the confidence in its assigned cluster. Mathematically, it is the
#'             log ratio of its distance to each centroid, normalized by the
#'             distance between both centroids, and min-maxed to a [-1,1]
#'             interval.
#'         }
#'         \item{
#'             For each chromosome, compute the distance between all centroids
#'             and the centroids of the first condition. The cross-condition
#'             clusters whose centroids are closest are given the same cluster
#'             label. This results in two clusters per chromosome, spanning all
#'             conditions.
#'         }
#'     }
#' }
#' \subsection{A/B compartments prediction}{
#' To match each cluster with an A or B compartment, the algorithm follows these
#' steps:
#'     \enumerate{
#'         \item{
#'             For each genomic position, compute its self interaction ratio,
#'             which is the difference between its self interaction and the
#'             median of its other interactions.
#'         }
#'         \item{
#'             For each chromosome, for each cluster, get the median self
#'             interaction ratio of the genomic positions in that cluster.
#'         }
#'         \item{
#'             For each chromosome, the cluster with the smallest median self
#'             interaction ratio is matched with compartment A, and the cluster
#'             with the greatest median self interaction ratio is matched with
#'             compartment B. Compartment A being open, there are more overall
#'             interactions between distant genomic positions, so it is assumed
#'             that the difference between self interactions and other
#'             interactions is lower than in compartment B.
#'         }
#'     }
#' }
#' \subsection{Significant differences detection}{
#' To find significant compartment differences across conditions, and compute
#' their p-values, the algorithm follows three steps:
#'     \enumerate{
#'         \item{
#'             For each pair of replicates in different conditions, for each
#'             genomic position, compute the absolute difference between its
#'             concordances.
#'         }
#'         \item{
#'             For each pair of conditions, for each genomic position, compute
#'             the median of its concordance differences.
#'         }
#'         \item{
#'             For each pair of conditions, for each genomic position whose
#'             assigned compartment switches, rank its median against the
#'             empirical cumulative distribution of medians of all non-switching
#'             positions in that condition pair. Adjust the resulting p-value
#'             with the Benjamini–Hochberg procedure.
#'         }
#'     }
#' }
#' \subsection{Parallel processing}{
#' The parallel version of \code{detectCompartments} uses the
#' \code{\link[BiocParallel]{bpmapply}} function. Before to call the
#' function in parallel you should specify the parallel parameters such as:
#'     \itemize{
#'         \item{On Linux:
#'
#'              \code{multiParam <- BiocParallel::MulticoreParam(workers = 10)}
#'          }
#'          \item{On Windows:
#'
#'              \code{multiParam <- BiocParallel::SnowParam(workers = 10)}
#'         }
#'     }
#'     And then you can register the parameters to be used by 
#'     \pkg{BiocParallel}:
#'
#'     \code{BiocParallel::register(multiParam, default = TRUE)}
#'
#'     You should be aware that using MulticoreParam, reproducibility of the
#'     detectCompartments function using a RNGseed may not work. See this
#'     \href{https://github.com/Bioconductor/BiocParallel/issues/122}{issue}
#'     for more details.
#' }
#'
#' @param object
#' A \code{\link{HiCDOCDataSet}}.
#'
#' @param parallel
#' Whether or not to parallelize the processing. Defaults to FALSE
#' See 'Details'.
#'
#' @param kMeansDelta
#' The convergence stop criterion for the clustering. When the centroids'
#' distances between two iterations is lower than this value, the clustering
#' stops. Defaults to \code{object$kMeansDelta} which is originally set to
#' \code{defaultHiCDOCParameters$kMeansDelta} = 0.0001.
#'
#' @param kMeansIterations
#' The maximum number of iterations during clustering. Defaults to
#' \code{object$kMeansIterations} which is originally set to
#' \code{defaultHiCDOCParameters$kMeansIterations} = 50.
#'
#' @param kMeansRestarts
#' The amount of times the clustering is restarted. For each restart, the
#' clustering iterates until convergence or reaching the maximum number of
#' iterations. The clustering that minimizes inner-cluster variance is selected.
#' Defaults to \code{object$kMeansRestarts} which is originally set to
#' \code{defaultHiCDOCParameters$kMeansRestarts} = 20.
#'
#' @param PC1CheckThreshold
#' The minimum percentage of variance that should be explained by
#' the first principal component of centroids to pass sanity check.
#' Defaults to \code{object$PC1CheckThreshold} which is originally set to
#' \code{defaultHiCDOCParameters$PC1CheckThreshold} = 0.75
#'
#' @return
#' A \code{\link{HiCDOCDataSet}}, with compartments, concordances, distances,
#' centroids, and differences.
#'
#' @examples
#' data(exampleHiCDOCDataSet)

#' ## Run all filtering and normalization steps (not run for timing reasons)
#' # object <- filterSmallChromosomes(exampleHiCDOCDataSet)
#' # object <- filterSparseReplicates(object)
#' # object <- filterWeakPositions(object)
#' # object <- normalizeTechnicalBiases(object)
#' # object <- normalizeBiologicalBiases(object)
#' # object <- normalizeDistanceEffect(object)
#'
#' # Detect compartments and differences across conditions
#' object <- detectCompartments(exampleHiCDOCDataSet)
#'
#' @usage
#' detectCompartments(
#'     object,
#'     parallel = FALSE,
#'     kMeansDelta = NULL,
#'     kMeansIterations = NULL,
#'     kMeansRestarts = NULL,
#'     PC1CheckThreshold = NULL
#' )
#'
#' @export
detectCompartments <- function(
    object,
    parallel = FALSE,
    kMeansDelta = NULL,
    kMeansIterations = NULL,
    kMeansRestarts = NULL,
    PC1CheckThreshold = NULL
) {

    .validateSlots(
        object,
        slots = c(
            "chromosomes",
            "validAssay",
            "parameters"
        )
    )

    if (!is.null(kMeansDelta)) {
        object@parameters$kMeansDelta <- kMeansDelta
    }
    if (!is.null(kMeansIterations)) {
        object@parameters$kMeansIterations <- kMeansIterations
    }
    if (!is.null(kMeansRestarts)) {
        object@parameters$kMeansRestarts <- kMeansRestarts
    }
    if (!is.null(PC1CheckThreshold)) {
        object@parameters$PC1CheckThreshold <- PC1CheckThreshold
    }
    object@parameters <- .validateParameters(object@parameters)

    message("Clustering genomic positions.")
    object <- .clusterize(object, parallel)
    object <- .tieCentroids(object)

    message("Predicting A/B compartments.")
    object <- .predictCompartmentsAB(object, parallel)
    object <- .checkResults(object)
    message("Detecting significant differences.")
    object <- .computePValues(object)

    # Reformating outputs
    object <- .formatDetectCompartment(object)

    return(object)
}
