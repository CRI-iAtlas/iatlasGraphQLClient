#' Query Cohorts
#'
#' @param cohorts A vector of strings
#' @param datasets A vector of strings
#' @param tags A vector of strings
#' @param clinical A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
query_cohorts <- function(
  cohorts = NA,
  datasets = NA,
  tags = NA,
  clinical = NA,
  paging = NA,
  ...
){
  create_result_from_cursor_paginated_api_query(
    query_args = list(
      "name" = cohorts,
      "dataSet" = datasets,
      "tag" = tags,
      "clinical" = clinical,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "cohorts.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
      "clinical" = character()
    ),
    select_cols = c(
      "name",
      "dataset_name" = "dataSet.name",
      "dataset_display" = "dataSet.display",
      "tag_name" = "tag.name",
      "tag_long_display" = "tag.longDisplay",
      "tag_short_display" = "tag.shortDisplay",
      "clinical"
    ),
    arrange_cols = "name",
  )
}

#' Query Cohorts
#'
#' @param name A string
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
query_full_cohort <- function(
  cohort,
  paging = NA,
  ...
){
  query_args = list(
    "name" = cohort,
    "paging" = paging,
    "distinct" = F
  )
  query_file = "cohort_full.txt"
  result <- do_cursor_paginated_api_query(query_args, query_file)
  result <- result$`1`
  cohort <- list()
  if ("clinical" %in% names(result)) {
    cohort$clinical <- result$clinical
  }
  if ("dataSet" %in% names(result)) {
    cohort$dataset <- result$dataSet
    cohort$dataSet <- NULL
  }
  if ("features" %in% names(result)) {
    cohort$features <- result$features[[1]]
  }
  if ("genes" %in% names(result)) {
    cohort$genes <- result$genes[[1]]
  }
  if ("name" %in% names(result)) {
    cohort$name <- result$name
  }
  if ("samples" %in% names(result)) {
    samples <- jsonlite::flatten(result$samples[[1]])
    if("tag.name" %in% names(samples)){
      cohort$samples <- samples %>%
        dplyr::select(
          "clinical_value",
          "name",
          "tag_long_display" = "tag.longDisplay",
          "tag_name" = "tag.name",
          "tag_short_display" = "tag.shortDisplay"
        )
    } else{
      cohort$samples <- samples
    }
  }
  if ("tag" %in% names(result)) {
    if(is.data.frame(result$tag)){
      tag <- result$tag %>%
          dplyr::select(
            "long_display" = "longDisplay",
            "name",
            "short_display" = "shortDisplay"
          )
      cohort$tag <- tag
    } else {
      cohort$tag <- NA
    }
  }
  return(cohort)
}
