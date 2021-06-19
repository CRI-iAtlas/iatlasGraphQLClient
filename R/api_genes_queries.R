
#' Query Genes
#'
#' @param cohorts A vector of strings
#' @param samples A vector of strings
#' @param entrez A vector of integers
#' @param gene_types A vector of strings
#' @param max_rnaseq_expr A double
#' @param min_rnaseq_expr A double
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
query_genes <- function(
  cohorts = NA,
  samples = NA,
  entrez = NA,
  gene_types = NA,
  max_rnaseq_expr = NA,
  min_rnaseq_expr = NA,
  paging = NA,
  ...
){
  create_result_from_cursor_paginated_api_query(
    query_args = list(
      "cohort" = cohorts,
      "sample" = samples,
      "entrez" = entrez,
      "geneType" = gene_types,
      "maxRnaSeqExpr" = max_rnaseq_expr,
      "minRnaSeqExpr" = min_rnaseq_expr,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "genes.txt",
    default_tbl = dplyr::tibble(
      "hgnc" = character(),
      "entrez" = integer(),
      "description" = character(),
      "friendly_name" = character(),
      "io_landscape_name" = character(),
      "gene_family" = character(),
      "gene_function" = character(),
      "immune_checkpoint" = character(),
      "pathway" = character(),
      "super_category" = character()
    ),
    select_cols = c(
      "hgnc",
      "entrez",
      "description",
      "friendly_name" = "friendlyName",
      "io_landscape_name" = "ioLandscapeName",
      "gene_family" = "geneFamily",
      "gene_function" = "geneFunction",
      "immune_checkpoint" = "immuneCheckpoint",
      "pathway",
      "super_category" = "superCategory"
    ),
    arrange_cols = "hgnc",
    ...
  )
}

#' Query Immunomodulators
#'
#' @param type A vector of strings
#' @param entrez A vector of integers
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
query_immunomodulators <- function(
  type = "immunomodulator",
  entrez = NA,
  paging = NA,
  ...
){
  create_result_from_cursor_paginated_api_query(
    query_args = list(
      "geneType" = type,
      "entrez" = entrez,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "immunomodulators.txt",
    default_tbl = dplyr::tibble(
      "entrez" = integer(),
      "hgnc" = character(),
      "friendly_name" = character(),
      "description" = character(),
      "gene_family" = character(),
      "gene_function" = character(),
      "immune_checkpoint" = character(),
      "super_category" = character(),
      "publications" = character()
    ),
    select_cols = c(
      "entrez",
      "hgnc",
      "friendly_name" = "friendlyName",
      "description",
      "gene_family" = "geneFamily",
      "gene_function" = "geneFunction",
      "immune_checkpoint" = "immuneCheckpoint",
      "super_category" = "superCategory",
      "publications"
    ),
    arrange_cols = "hgnc",
    ...
  )
}

#' Query IO Targets
#'
#' @param gene_types A vector of strings
#' @param entrez A vector of integers
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
query_io_targets <- function(
  gene_types = "io_target",
  entrez = NA,
  paging = NA,
  ...
){
  create_result_from_cursor_paginated_api_query(
    query_args = list(
      "geneType" = gene_types,
      "entrez" = entrez,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "io_targets.txt",
    default_tbl = dplyr::tibble(
      "entrez" = integer(),
      "hgnc" = character(),
      "friendly_name" = character(),
      "description" = character(),
      "io_landscape_name" = character(),
      "pathway" = character(),
      "therapy_type" = character()
    ),
    select_cols = c(
      "entrez",
      "hgnc",
      "description",
      "io_landscape_name" = "ioLandscapeName",
      "pathway",
      "therapy_type" = "therapyType"
    ),
    arrange_cols = "hgnc",
    ...
  )
}

#' Query Gene Expression
#'
#' @param cohorts A vector of strings
#' @param samples A vector of strings
#' @param entrez A vector of integers
#' @param gene_types A vector of strings
#' @param max_rnaseq_expr A double
#' @param min_rnaseq_expr A double
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
query_gene_expression <- function(
  cohorts = NA,
  samples = NA,
  entrez = NA,
  gene_types = NA,
  max_rnaseq_expr = NA,
  min_rnaseq_expr = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args = list(
      "cohort" = cohorts,
      "sample" = samples,
      "entrez" = entrez,
      "geneType" = gene_types,
      "maxRnaSeqExpr" = max_rnaseq_expr,
      "minRnaSeqExpr" = min_rnaseq_expr,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "gene_expression.txt",
    default_tbl = dplyr::tibble(
      "sample" = character(),
      "entrez" = character(),
      "hgnc" = character(),
      "rna_seq_expr" = double()
    ),
    select_cols = c(
      "samples",
      "entrez",
      "hgnc"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "sample" = "name",
        "entrez",
        "hgnc",
        "rna_seq_expr" = "rnaSeqExpr"
      )
  }
}
