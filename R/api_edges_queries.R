utils::globalVariables(".")

#' Query Edges
#'
#' @param node1 A vector of strings
#' @param node2 A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_paginated_api_query
#' @param max_score A double
#' @param min_score A double
#'
#' @export
query_edges <- function(
  max_score = NA,
  min_score = NA,
  node1 = NA,
  node2 = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_paginated_api_query2(
    query_args =  list(
      "maxScore" = max_score,
      "minScore" = min_score,
      "node1" = node1,
      "node2" = node2,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "edges.txt",
    default_tbl = dplyr::tibble(
      "label" = character(),
      "name" = character(),
      "score" = double(),
      "node1" = character(),
      "node2" = character()
    ),
    select_cols = c(
      "label",
      "name",
      "score",
      "node1" = "node1.name",
      "node2" = "node2.name"
    ),
    ...
  )
}
