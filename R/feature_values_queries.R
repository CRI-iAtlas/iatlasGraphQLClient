#' Query Feature Values
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
#' @importFrom magrittr %>%
query_feature_values <- function(
  cohorts = NA,
  samples = NA,
  features = NA,
  feature_classes = NA,
  max_value = NA,
  min_value = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "cohort" = cohorts,
      "sample" = samples,
      "feature" = features,
      "featureClass" = feature_classes,
      "maxValue" = max_value,
      "minValue" = min_value,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "feature_values.txt",
    default_tbl = dplyr::tibble(
      "value" = double(),
      "sample_name" = character(),
      "feature_name" = character(),
      "feature_display" = character(),
      "feature_order" = integer(),
      "feature_class" = character(),
    ),
    select_cols = c(
      "value",
      "sample_name" = "sample.name",
      "feature_name" = "feature.name",
      "feature_display" = "feature.display",
      "feature_order" = "feature.order",
      "feature_class" = "feature.class"
    ),
    ...
  )
}
