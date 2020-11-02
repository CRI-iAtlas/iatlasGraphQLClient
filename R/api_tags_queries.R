format_parent_tag <- function(x){
  if(length(x) == 0){
    return(x)
  } else {
    tbl <- dplyr::select(
      x,
      "name",
      "long_display" =  "longDisplay",
      "short_display" =  "shortDisplay",
      "characteristics",
      "color"
    )
    return(tbl)
  }
}

format_parent_tag2 <- function(x){
  if(length(x) == 0){
    return(x)
  } else {
    tbl <- dplyr::select(
      x,
      "parent_tag_name" = "name",
    )
    return(tbl)
  }
}

format_publication <- function(x){
  if(length(x) == 0){
    return(x)
  } else {
    tbl <- dplyr::select(
      x,
      "name",
      "title",
      "do_id" = "doId",
      "pubmed_id" = "pubmedId",
      "journal",
      "first_author_last_name" = "firstAuthorLastName",
      "year"
    )
    return(tbl)
  }
}


#' Query Tags
#'
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param datasets A vector of strings
#' @param features A vector of strings
#' @param feature_classes A vector of strings
#' @param samples A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tags <- function(
  tags = NA,
  parent_tags = NA,
  datasets = NA,
  features = NA,
  feature_classes = NA,
  samples = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "feature" = features,
      "featureClass" = feature_classes,
      "sample" = samples
    ),
    query_file = "tags.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
      "long_display" = character(),
      "short_display" = character(),
      "characteristics" = character(),
      "color" = character(),
      "parent_tags" = list(),
      "publications" = list()

    ),
    select_cols = c(
      "name",
      "long_display" =  "longDisplay",
      "short_display" =  "shortDisplay",
      "characteristics",
      "color",
      "parent_tags" = "related",
      "publications"
    ),
    arrange_cols = "name",
    ...
  )
  tbl %>%
    dplyr::mutate(
      "parent_tags" = purrr::map(.data$parent_tags, format_parent_tag),
      "publications" = purrr::map(.data$publications, format_publication)
    )
}

#' Tag Samples
#'
#'
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param features A vector of strings
#' @param feature_classes A vector of strings
#' @param samples A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tag_samples <- function(
  datasets,
  parent_tags,
  tags = NA,
  features = NA,
  feature_classes = NA,
  samples = NA,
  ...
){
  create_result_from_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "feature" = features,
      "featureClass" = feature_classes,
      "sample" = samples
    ),
    query_file = "tag_samples.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
      "long_display" = character(),
      "short_display" = character(),
      "characteristics" = character(),
      "color" = character(),
      "size" = integer(),
      "samples" = list()
    ),
    select_cols = c(
      "name",
      "long_display" =  "longDisplay",
      "short_display" =  "shortDisplay",
      "characteristics",
      "color",
      "size" = "sampleCount",
      "samples"
    ),
    arrange_cols = "name",
    ...
  )
}

#' Tag Samples 2
#'
#'
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param features A vector of strings
#' @param feature_classes A vector of strings
#' @param samples A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tag_samples2 <- function(
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  features = NA,
  feature_classes = NA,
  samples = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "feature" = features,
      "featureClass" = feature_classes,
      "sample" = samples
    ),
    query_file = "tag_samples2.txt",
    default_tbl = dplyr::tibble(
      "sample" = character()
    ),
    select_cols = c(
      "tag_name" = "name",
      "sample" = "samples",
      "parent_tags" = "related"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  tbl <- tbl %>%
    dplyr::mutate(
      "parent_tags" = purrr::map(.data$parent_tags, format_parent_tag2),
    ) %>%
    tidyr::unnest("parent_tags") %>%
    dplyr::filter(.data$parent_tag_name %in% parent_tags) %>%
    tidyr::unnest("sample") %>%
    tidyr::pivot_wider(
      names_from = "parent_tag_name", values_from = "tag_name"
    ) %>%
    dplyr::select(c("sample", parent_tags))
}


