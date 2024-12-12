#' @title
#' A/B compartment detection and differential analysis
#' @docType package
#' @import methods
#' @import zlibbioc
#' @import ggplot2
#' @import InteractionSet
#' @importFrom GenomicRanges start end distance GRanges match union
#' @importFrom Rcpp evalCpp sourceCpp
#' @importFrom stats loess loess.control ecdf predict median update optimize prcomp p.adjust quantile wilcox.test
#' @importFrom S4Vectors DataFrame mcols split runLength Rle %in%
#' @importFrom SummarizedExperiment assay colData
#' @importFrom gtools mixedsort
#' @importFrom pbapply pbmapply
#' @importFrom BiocParallel bpparam bpmapply bplapply
#' @importFrom BiocGenerics cbind width
#' @importFrom cowplot ggdraw get_plot_component
#' @importFrom grid textGrob gpar
#' @importFrom gridExtra arrangeGrob
#' @importFrom multiHiCcompare make_hicexp cyclic_loess hic_table
#' @importFrom GenomeInfoDb seqlevels seqnames
#' @import data.table
#' @useDynLib HiCDOC
#' @aliases HiCDOC-package
"_PACKAGE"
