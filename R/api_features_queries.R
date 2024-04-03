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
      "sample" = character(),
      "feature_name" = character(),
      "feature_display" = character(),
      "feature_value" = double(),
      "feature_order" = integer(),
      "feature_class" = character(),
    ),
    select_cols = c(
      "feature_name" = "name",
      "feature_display" = "display",
      "feature_order" = "order",
      "feature_class" = "class",
      "samples"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "sample" = "name",
        "feature_name",
        "feature_display",
        "feature_value" = "value",
        "feature_order",
        "feature_class"
      )
  }
}


#' Query PseudoBulk Feature Values
#'
#' @param cohorts A vector of strings
#' @param features A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
#' @importFrom magrittr %>%
query_pseudobulk_feature_values <- function(
    cohorts = NA,
    features = NA,
    paging = NA,
    ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "cohort" = cohorts,
      "feature" = features,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "pseudobulk_feature_values.txt",
    default_tbl = dplyr::tibble(
      "feature_name" = character(),
      "feature_display" = character(),
      "feature_order" = integer(),
      "feature_class" = character(),
      "sample_name" = character(),
      "cell_type"= character(),
      "value" = double()
    ),
    select_cols = c(
      "feature_name" = "name",
      "feature_display" = "display",
      "feature_order" = "order",
      "feature_class" = "class",
      "cellTypeSamples"
    ),
    ...
  )
  if (nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "cellTypeSamples", keep_empty = T) %>%
      dplyr::select(
        "feature_name",
        "feature_display",
        "feature_order",
        "feature_class",
        "sample_name" = "name",
        "cell_type" = "cellType",
        "value"
      )
  }
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
