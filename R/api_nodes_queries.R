#' Query Nodes
#'
#' @param datasets A vector of strings
#' @param tags A vector of strings
#' @param max_score A float
#' @param min_score A float
#' @param parent_tags A vector of strings
#' @param network A vector of strings
#' @param entrez A vector of integers
#' @param features A vector of strings
#' @param n_tags An integer
#' @param paging A named list
#' @param ... Arguments to create_result_from_paginated_api_query
#'
#' @export
query_nodes <- function(
  datasets = NA,
  features = NA,
  entrez = NA,
  max_score = NA,
  min_score = NA,
  parent_tags = NA,
  network = NA,
  tags = NA,
  n_tags = NA,
  paging = NA,
  ...
){
  has_features <- !(length(features) == 1 && is.na(features))
  has_genes    <- !(length(entrez) == 1 && is.na(entrez))

  if(has_features & has_genes){
    stop("Can not query for both entrez and features at a the same time")
  } else if(has_features) {
    query_file <- "feature_nodes.txt"
    select_cols <- c(
      get_node_json_names(),
      "feature_name" = "feature.name",
      "feature_display" = "feature.display"
    )
  } else if(has_genes) {
    query_file <- "gene_nodes.txt"
    select_cols <- c(
      get_node_json_names(),
      "gene_entrez"  = "gene.entrez",
      "gene_hgnc"  = "gene.hgnc",
      "gene_friendly_name" = "gene.friendlyName"
    )
  }
  query_args <- list(
    "dataSet" = datasets,
    "feature" = features,
    "entrez" = entrez,
    "maxScore" = max_score,
    "minScore" = min_score,
    "related" = parent_tags,
    "network" = network,
    "tag" = tags,
    "nTags"= n_tags,
    "paging" = paging,
    "distinct" = F
  )
  default_tbl <- purrr::invoke(
    .f = dplyr::tibble,
    .x = c(
      get_node_empty_values(),
      list(
        "feature_name" = character(),
        "feature_display" = character(),
        "gene_entrez" = integer(),
        "gene_hgnc" = character(),
        "gene_friendly_name" = character()
      )
    )
  )

  tbl <-create_result_from_cursor_paginated_api_query(
    query_args = query_args,
    query_file = query_file,
    default_tbl = default_tbl,
    select_cols = select_cols,
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  tbl %>%
    dplyr::mutate("node_tags" = purrr::map(
      .data$node_tags,
      ~dplyr::select(.x, get_tag_json_names())
    ))
  if(!has_features){
    tbl <- tbl %>%
      dplyr::mutate(
        "feature_name" = NA_character_,
        "feature_display" = NA_character_,
      )
  }
  if(!has_genes){
    tbl <- tbl %>%
      dplyr::mutate(
        "gene_entrez" = NA_integer_,
        "gene_hgnc" = NA_character_,
        "gene_friendly_name" = NA_character_
      )
  }
  return(tbl)

}

# helpers ----

get_node_column_tbl <- function(prefix = "node_"){
  dplyr::tribble(
    ~name,             ~json_name,        ~empty_val,
    "label",           "label",           character(),
    "name",            "name",            character(),
    "score",           "score",           double(),
    "tags",            "tags",            list(),
    "x",               "x",               character(),
    "y",               "y",               character()

  ) %>%
    dplyr::mutate("name" = stringr::str_c(prefix, .data$name))
}

get_node_json_names <- function(prefix = "node_"){
  lst <-
    get_node_column_tbl(prefix) %>%
    dplyr::select("name", "json_name") %>%
    tibble::deframe()
}

get_node_empty_values <- function(prefix = "node_"){
  lst <-
    get_node_column_tbl(prefix) %>%
    dplyr::select("name", "empty_val") %>%
    tibble::deframe()
}

get_node_field_names <- function(prefix = "node_"){
  lst <-
    get_node_column_tbl(prefix) %>%
    dplyr::pull("name")
}
