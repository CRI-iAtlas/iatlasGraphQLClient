#' Query Datasets
#'
#' @param datasets A vector of strings that are names of datasets
#' @param samples A vector of strings
#' @param types A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
query_datasets <- function(
  datasets = NA,
  samples = NA,
  types = NA,
  paging = NA,
  ...
){
  create_result_from_cursor_paginated_api_query(
    query_args = list(
      "dataSet" = datasets,
      "sample" = samples,
      "dataSetType" = types,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "datasets.txt",
    default_tbl = dplyr::tibble(
      "display" = character(),
      "name" = character(),
      "type" = character()
    ),
    select_cols = c("display", "name", "type"),
    arrange_cols = "display",
    ...
  )
}

#' Query Dataset Samples
#'
#' @param datasets A vector of strings that are names of datasets
#' @param samples A vector of strings
#' @param types A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_dataset_samples <- function(
  datasets = NA,
  samples = NA,
  types = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args = list(
      "dataSet" = datasets,
      "sample" = samples,
      "dataSetType" = types,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "dataset_samples.txt",
    default_tbl = dplyr::tibble(
      "sample_name" = character(),
      "dataset_display" = character(),
      "dataset_name" = character(),
      "dataset_type" = character()
    ),
    select_cols = c(
      "samples",
      "dataset_display" = "display",
      "dataset_name" = "name",
      "dataset_type" = "type"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "sample_name" = "name",
        "dataset_display",
        "dataset_name",
        "dataset_type"
      )
  }
}

#' Query Dataset Tags
#'
#' @param datasets A vector of strings that are names of datasets
#' @param samples A vector of strings
#' @param types A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_dataset_tags <- function(
  datasets = NA,
  samples = NA,
  types = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args = list(
      "dataSet" = datasets,
      "sample" = samples,
      "dataSetType" = types,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "dataset_tags.txt",
    default_tbl = dplyr::tibble(
      "tag_name" = character(),
      "tag_long_display" = character(),
      "tag_short_display"  = character(),
      "tag_color" = character(),
      "tag_characteristics" = character(),
      "dataset_display" = character(),
      "dataset_name" = character(),
      "dataset_type" = character()
    ),
    select_cols = c(
      "tags",
      "dataset_display" = "display",
      "dataset_name" = "name",
      "dataset_type" = "type"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "tags", keep_empty = T) %>%
      dplyr::select(
        "tag_name" = "name",
        "tag_long_display" = "longDisplay",
        "tag_short_display" = "shortDisplay",
        "tag_color" = "color",
        "tag_characteristics" = "characteristics",
        "dataset_display",
        "dataset_name",
        "dataset_type"
      )
  }
}
