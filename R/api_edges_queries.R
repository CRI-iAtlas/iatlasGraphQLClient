utils::globalVariables(".")

#' Query Edges
#'
#' @param node1 A vector of strings
#' @param node2 A vector of strings
#' @param page An integer
#' @param ... Arguments to create_result_from_paginated_api_query
#'
#' @export
query_edges <- function(
  node1 = NA,
  node2 = NA,
  page = NA,
  ...
){
  tbl <- create_result_from_paginated_api_query(
    query_args =  list(
      node1 = node1,
      node2 = node2,
      page = page
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
