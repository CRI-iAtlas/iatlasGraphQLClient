#' Query Cell Stats
#'
#' @param entrez A vector integers
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
query_cell_stats <- function(
  entrez = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "entrez" = entrez,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "cell_stats.txt",
    default_tbl = dplyr::tibble(
      "type" = character(),
      "count" = integer(),
      "avg_expr" = double(),
      "perc_expr" = double(),
      "dataset_name"  = character(),
      "gene_entrez" = integer()
    ),
    select_cols = c(
      "type",
      "count",
      "avg_expr" = "avgExpr",
      "perc_expr" = "percExpr",
      "dataset_name"  = "dataSet.name",
      "gene_entrez" = "gene.entrez"
    ),
    ...
  )
  return(tbl)
}
