#' Query Features
#'
#' @param cohorts A vector of strings
#' @param samples A vector of strings
#' @param features A vector of strings
#' @param feature_classes A vector of strings
#' @param max_value A numeric
#' @param min_value A numeric
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
query_features <- function(
  cohorts = NA,
  samples = NA,
  features = NA,
  feature_classes = NA,
  max_value = NA,
  min_value = NA,
  paging = NA,
  ...
){
  create_result_from_cursor_paginated_api_query(
    query_args = list(
      "cohort" = cohorts,
      "sample" = samples,
      "feature" = features,
      "featureClass" = feature_classes,
      "maxValue" = max_value,
      "minValue" = min_value,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "features.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
      "display" = character(),
      "class" = character(),
      "order" = integer(),
      "unit" =  character(),
      "method_tag" = character()
    ),
    select_cols = c(
      "name",
      "display",
      "class",
      "order",
      "unit",
      "method_tag" = "methodTag"
    ),
    arrange_cols = "display",
    ...
  )
}


#' Query Features Range
#'
#' @param cohorts A vector of strings
#' @param samples A vector of strings
#' @param features A vector of strings
#' @param feature_classes A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
#' @importFrom magrittr %>%
query_features_range <- function(
  cohorts = NA,
  samples = NA,
  features = NA,
  feature_classes = NA,
  paging = NA,
  ...
){
  create_result_from_cursor_paginated_api_query(
    query_args = list(
      "cohort" = cohorts,
      "sample" = samples,
      "feature" = features,
      "featureClass" = feature_classes,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "features_range.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
      "display" = character(),
      "value_min" = double(),
      "value_max" = double()
    ),
    select_cols = c(
      "name",
      "display",
      "value_min" = "valueMin",
      "value_max" = "valueMax"
    ),
    arrange_cols = "display",
    ...
  )
}
