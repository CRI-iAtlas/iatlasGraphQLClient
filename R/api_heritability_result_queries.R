
#' Query Heritability Results
#'
#' @param datasets A vector of strings
#' @param features A vector of strings
#' @param clusters A vector of strings
#' @param min_p_value A double
#' @param max_p_value A double
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
query_heritability_results <- function(
  datasets = NA,
  features = NA,
  clusters = NA,
  min_p_value = NA,
  max_p_value = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "feature" = features,
      "cluster" = clusters,
      "minPValue" = min_p_value,
      "maxPValue" = max_p_value,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "heritability_results.txt",
    default_tbl = dplyr::tibble(
      "dataset_name" = character(),
      "dataset_display" = character(),
      "feature_name" = character(),
      "feature_display" = character(),
      "feature_germline_module" = character(),
      "feature_germline_category" = character(),
      "cluster" = character(),
      "p_value" = double(),
      "fdr" = double(),
      "se" = double(),
      "variance" = double(),
    ),
    select_cols = c(
      "dataset_name" = "dataSet.name",
      "dataset_display" = "dataSet.display",
      "feature_name" = "feature.name",
      "feature_display" = "feature.display",
      "feature_germline_module" = "feature.germlineModule",
      "feature_germline_category" = "feature.germlineCategory",
      "cluster" = "cluster",
      "p_value" = "pValue",
      "fdr" = "fdr",
      "se" = "se",
      "variance" = "variance"
    ),
    ...
  )
}
