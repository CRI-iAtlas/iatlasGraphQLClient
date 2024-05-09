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