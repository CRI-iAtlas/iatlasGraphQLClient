#' Query Tags
#'
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param datasets A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tags <- function(
  tags = NA,
  parent_tags = NA,
  datasets = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "tags.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
      "long_display" = character(),
      "short_display" = character(),
      "characteristics" = character(),
      "color" = character()
    ),
    select_cols = c(
      "name",
      "long_display" =  "longDisplay",
      "short_display" =  "shortDisplay",
      "characteristics",
      "color"
    ),
    arrange_cols = "name",
    ...
  )
}

#' Query Tag Samples
#'
#' @param cohorts a vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
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
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "cohort" = cohorts,
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "sample" = samples,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "tag_samples.txt",
    default_tbl = dplyr::tibble(
      "sample_name" = character(),
      "tag_name" = character(),
      "tag_long_display" = character(),
      "tag_short_display" = character(),
      "tag_characteristics" = character(),
      "tag_color" = character()
    ),
    select_cols = c(
      "samples",
      "tag_name" = "name",
      "tag_long_display" =  "longDisplay",
      "tag_short_display" =  "shortDisplay",
      "tag_characteristics" = "characteristics",
      "tag_color" = "color"
    ),
    arrange_cols = "tag_name",
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "sample_name" = "name",
        "tag_name",
        "tag_long_display",
        "tag_short_display",
        "tag_characteristics",
        "tag_color"
      )
  }
}

#' Query Tag Sample Count
#'
#' @param cohorts a vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
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
  paging = NA,
  ...
){
  create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "cohort" = cohorts,
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "sample" = samples,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "tag_sample_count.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
      "long_display" = character(),
      "short_display" = character(),
      "characteristics" = character(),
      "color" = character(),
      "sample_count" = integer()
    ),
    select_cols = c(
      "name",
      "long_display" =  "longDisplay",
      "short_display" =  "shortDisplay",
      "characteristics",
      "color",
      "sample_count" = "sampleCount"
    ),
    arrange_cols = "name",
    ...
  )
}

#' Query Tag Publications
#'
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param datasets A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tag_publications <- function(
  tags = NA,
  parent_tags = NA,
  datasets = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "tag_publications.txt",
    default_tbl = dplyr::tibble(
      "publication_do_id" = integer(),
      "publication_first_author_last_name" = character(),
      "publication_journal" = character(),
      "publication_name" = character(),
      "publication_pubmed_id" = integer(),
      "publication_title" = character(),
      "tag_name" = character(),
      "tag_long_display" = character(),
      "tag_short_display" = character(),
      "tag_characteristics" = character(),
      "tag_color" = character()
    ),
    select_cols = c(
      "publications",
      "tag_name" = "name",
      "tag_long_display" =  "longDisplay",
      "tag_short_display" =  "shortDisplay",
      "tag_characteristics" = "characteristics",
      "tag_color" = "color"
    ),
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
        "tag_name",
        "tag_long_display",
        "tag_short_display",
        "tag_characteristics",
        "tag_color"
      )
  }
}

#' Query Tags With Parent Tags
#'
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tags_with_parent_tags <- function(
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "tag_related.txt",
    default_tbl = dplyr::tibble(
      "parent_tag_name" = character(),
      "parent_tag_long_display" = character(),
      "parent_tag_short_display" = character(),
      "parent_tag_characteristics" = character(),
      "parent_tag_color" = character(),
      "tag_name" = character(),
      "tag_long_display" = character(),
      "tag_short_display" = character(),
      "tag_characteristics" = character(),
      "tag_color" = character()
    ),
    select_cols = c(
      "related",
      "tag_name" = "name",
      "tag_long_display" =  "longDisplay",
      "tag_short_display" =  "shortDisplay",
      "tag_characteristics" = "characteristics",
      "tag_color" = "color"
    ),
    arrange_cols = "tag_name",
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "related", keep_empty = T) %>%
      dplyr::select(
        "parent_tag_name" = "name",
        "parent_tag_long_display" =  "longDisplay",
        "parent_tag_short_display" =  "shortDisplay",
        "parent_tag_characteristics" = "characteristics",
        "parent_tag_color" = "color",
        "tag_name",
        "tag_long_display",
        "tag_short_display",
        "tag_characteristics",
        "tag_color"
      )
  }
}

