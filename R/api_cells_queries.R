#' Query Cells
#'
#' @param cohort A vector of characters
#' @param cell A vector of characters
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
query_cells <- function(
  cohort = NA,
  cell = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "cohort" = cohort,
      "cell" = cell,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "cells.txt",
    default_tbl = dplyr::tibble(
      "type" = character(),
      "name" = character(),
    ),
    select_cols = c(
      "type",
      "name"
    ),
    ...
  )
  return(tbl)
}

#' Query SingleCellSeq
#'
#' @param entrez A vector integers
#' @param cohort A vector of characters
#' @param cell A vector of characters
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
query_single_cell_seq <- function(
    entrez = NA,
    cohort = NA,
    cell = NA,
    paging = NA,
    ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "entrez" = entrez,
      "cohort" = cohort,
      "cell" = cell,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "single_cell_seq.txt",
    default_tbl = dplyr::tibble(
      "cell_type" = character(),
      "cell_name" = character(),
      "gene_entrez" = integer(),
      "gene_hgnc" = character(),
      "gene_single_cell_seq" = double()
    ),
    select_cols = c(
      "cell_type" = "type",
      "cell_name" = "name",
      "genes"
    ),
    ...
  )
  if (nrow(tbl) == 0) return(tbl)
  else {
    tbl <- tbl %>%
      tidyr::unnest(cols = "genes", keep_empty = T) %>%
      dplyr::select(
        "cell_type",
        "cell_name",
        "gene_entrez" = "entrez",
        "gene_hgnc" = "hgnc",
        "gene_single_cell_seq" = "singleCellSeq"
      )
  }
  return(tbl)
}


#' Query SingleCellFeature
#'
#' @param feature A vector of characters
#' @param cohort A vector of characters
#' @param cell A vector of characters
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
query_single_cell_feature <- function(
    feature = NA,
    cohort = NA,
    cell = NA,
    paging = NA,
    ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "feature" = feature,
      "cohort" = cohort,
      "cell" = cell,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "single_cell_feature.txt",
    default_tbl = dplyr::tibble(
      "cell_type" = character(),
      "cell_name" = character(),
      "feature_name" = character(),
      "feature_display" = character(),
      "feature_value" = double()
    ),
    select_cols = c(
      "cell_type" = "type",
      "cell_name" = "name",
      "features"
    ),
    ...
  )
  if (nrow(tbl) == 0) return(tbl)
  else {
    tbl <- tbl %>%
      tidyr::unnest(cols = "features", keep_empty = T) %>%
      dplyr::select(
        "cell_type",
        "cell_name",
        "feature_name" = "name",
        "feature_display" = "display",
        "feature_value" = "value",
      )
  }
  return(tbl)
}
