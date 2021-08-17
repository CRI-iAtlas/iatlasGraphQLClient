#' Query Tags
#'
#' @param cohorts a vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param type A vector of strings
#' @param samples A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tags <- function(
  cohorts = NA,
  samples = NA,
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  type = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "cohort" = cohorts,
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "type" = type,
      "sample" = samples,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "tags.txt",
    default_tbl = purrr::invoke(
      .f = dplyr::tibble,
      .x = get_tag_empty_values()
    ),
    select_cols = get_tag_json_names(),
    arrange_cols = "tag_name",
    ...
  )
}

#' Query Tag Samples
#'
#' @param cohorts a vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param type A vector of strings
#' @param samples A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tag_samples <- function(
  cohorts = NA,
  samples = NA,
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  type = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "cohort" = cohorts,
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "type" = type,
      "sample" = samples,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "tag_samples.txt",
    default_tbl = purrr::invoke(
      .f = dplyr::tibble,
      .x = c(
        list("sample_name" = character()),
        get_tag_empty_values()
      )
    ),
    select_cols = c("samples", get_tag_json_names()),
    arrange_cols = "tag_name",
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "sample_name" = "name",
        get_tag_field_names()
      )
  }
}

#' Query Tag Samples Parents
#'
#' @param cohorts a vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param type A vector of strings
#' @param samples A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tag_samples_parents <- function(
  cohorts = NA,
  samples = NA,
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  type = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "cohort" = cohorts,
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "type" = type,
      "sample" = samples,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "tag_samples_related.txt",
    default_tbl = purrr::invoke(
      .f = dplyr::tibble,
      .x = c(
        list("sample_name" = character()),
        get_tag_empty_values(prefix = "parent_tag_"),
        get_tag_empty_values()
      )
    ),
    select_cols = c("samples", "related", get_tag_json_names()),
    arrange_cols = "tag_name",
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "sample_name" = "name",
        "related",
        get_tag_field_names()
      ) %>%
      tidyr::unnest(cols = "related", keep_empty = T) %>%
      dplyr::select(
        "sample_name",
        get_tag_json_names(prefix = "parent_tag_"),
        get_tag_field_names()
      )
  }
}

#' Query Tag Sample Count
#'
#' @param cohorts a vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param type A vector of strings
#' @param samples A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tag_sample_count <- function(
  cohorts = NA,
  samples = NA,
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  type = NA,
  paging = NA,
  ...
){
  create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "cohort" = cohorts,
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "type" = type,
      "sample" = samples,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "tag_sample_count.txt",
    default_tbl = purrr::invoke(
      .f = dplyr::tibble,
      .x = c(
        get_tag_empty_values(),
        list("sample_count" = integer())
      )
    ),
    select_cols = c(get_tag_json_names(), "sample_count" = "sampleCount"),
    arrange_cols = "tag_name",
    ...
  )
}

#' Query Tag Publications
#'
#' @param cohorts a vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param type A vector of strings
#' @param samples A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tag_publications <- function(
  cohorts = NA,
  samples = NA,
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  type = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "cohort" = cohorts,
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "type" = type,
      "sample" = samples,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "tag_publications.txt",
    default_tbl = purrr::invoke(
      .f = dplyr::tibble,
      .x = c(
        list(
          "publication_do_id" = integer(),
          "publication_first_author_last_name" = character(),
          "publication_journal" = character(),
          "publication_name" = character(),
          "publication_pubmed_id" = integer(),
          "publication_title" = character()
        ),
        get_tag_empty_values()
      )
    ),
    select_cols = c("publications", get_tag_json_names()),
    arrange_cols = "tag_name",
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "publications", keep_empty = T) %>%
      dplyr::select(
        "publication_do_id" = "doId",
        "publication_first_author_last_name" = "firstAuthorLastName",
        "publication_journal" = "journal",
        "publication_name" = "name",
        "publication_pubmed_id" = "pubmedId",
        "publication_title" = "title",
        get_tag_field_names()
      )
  }
}

#' Query Tags With Parent Tags
#'
#' @param cohorts a vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param type A vector of strings
#' @param samples A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tags_with_parent_tags <- function(
  cohorts = NA,
  samples = NA,
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  type = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "cohort" = cohorts,
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "type" = type,
      "sample" = samples,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "tag_related.txt",
    default_tbl = purrr::invoke(
      .f = dplyr::tibble,
      .x = c(
        get_tag_empty_values(prefix = "parent_tag_"),
        get_tag_empty_values()
      )
    ),
    select_cols = c("related", get_tag_json_names()),
    arrange_cols = "tag_name",
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "related", keep_empty = T) %>%
      dplyr::select(
        get_tag_json_names(prefix = "parent_tag_"),
        get_tag_field_names()
      )
  }
}

#tag_helpers ----

get_tag_column_tbl <- function(prefix = "tag_"){
  dplyr::tribble(
    ~name,             ~json_name,         ~empty_val,
    "name",            "name",             character(),
    "long_display",    "longDisplay",      character(),
    "short_display",   "shortDisplay",     character(),
    "characteristics", "characteristics",  character(),
    "color",           "color",            character(),
    "order",           "order",            integer(),
    "type",            "type",             character(),
  ) %>%
    dplyr::mutate("name" = stringr::str_c(prefix, .data$name))
}

get_tag_json_names <- function(prefix = "tag_"){
  lst <-
    get_tag_column_tbl(prefix) %>%
    dplyr::select("name", "json_name") %>%
    tibble::deframe()
}

get_tag_empty_values <- function(prefix = "tag_"){
  lst <-
    get_tag_column_tbl(prefix) %>%
    dplyr::select("name", "empty_val") %>%
    tibble::deframe()
}

get_tag_field_names <- function(prefix = "tag_"){
  lst <-
    get_tag_column_tbl(prefix) %>%
    dplyr::pull("name")
}


