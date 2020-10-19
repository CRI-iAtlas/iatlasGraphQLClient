utils::globalVariables(".")

#' Query Gene Nodes
#'
#' @param datasets A vector of strings
#' @param tags A vector of strings
#' @param page An integer
#' @param max_score A float
#' @param min_score A float
#' @param parent_tags A vector of strings
#' @param network A vector of strings
#' @param entrez A vector of integers
#' @param ... Arguments to create_result_from_paginated_api_query
#'
#' @export
query_gene_nodes <- function(
  datasets = NA,
  entrez = NA,
  max_score = NA,
  min_score = NA,
  parent_tags = NA,
  network = NA,
  tags = NA,
  page = NA,
  ...
){
  tbl <- create_result_from_paginated_api_query(
    query_args =  list(
      dataSet = datasets,
      entrez = entrez,
      maxScore = max_score,
      minScore = min_score,
      related = parent_tags,
      network = network,
      tag = tags,
      page = page
    ),
    query_file = "gene_nodes.txt",
    default_tbl = dplyr::tibble(
      "label" = character(),
      "name" = character(),
      "score" = double(),
      "tags" = list(),
      "x"  = character(),
      "y"  = character(),
      "entrez"  = character(),
      "hgnc"  = character()
    ),
    select_cols = c(
      "label",
      "name",
      "score",
      "tags",
      "x",
      "y",
      "entrez"  = "gene.entrez",
      "hgnc"  = "gene.hgnc"
    ),
    ...
  )
}

#' Query Feature Nodes
#'
#' @param datasets A vector of strings
#' @param tags A vector of strings
#' @param features A vector of strings
#' @param page An integer
#' @param max_score A float
#' @param min_score A float
#' @param parent_tags A vector of strings
#' @param network A vector of strings
#' @param ... Arguments to create_result_from_paginated_api_query
#'
#' @export
query_feature_nodes <- function(
  datasets = NA,
  features = NA,
  max_score = NA,
  min_score = NA,
  parent_tags = NA,
  network = NA,
  tags = NA,
  page = NA,
  ...
){
  tbl <- create_result_from_paginated_api_query(
    query_args =  list(
      dataSet = datasets,
      feature = features,
      maxScore = max_score,
      minScore = min_score,
      related = parent_tags,
      network = network,
      tag = tags,
      page = page
    ),
    query_file = "feature_nodes.txt",
    default_tbl = dplyr::tibble(
      "label" = character(),
      "name" = character(),
      "score" = double(),
      "tags" = list(),
      "x"  = character(),
      "y"  = character(),
      "feature_name"  = character(),
      "feature_display"  = character()
    ),
    select_cols = c(
      "label",
      "name",
      "score",
      "tags",
      "x",
      "y",
      "feature_name" = "feature.name",
      "feature_display" = "feature.display"
    ),
    ...
  )
}
