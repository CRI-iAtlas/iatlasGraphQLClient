
#' Query Mutations
#'
#' @param datasets A vector of strings
#' @param ids A vector of integers
#' @param entrez A vector of integers
#' @param codes A vector of strings
#' @param types A vector of strings
#' @param parent_tags A vector of strings
#' @param samples A vector of strings
#' @param status A string
#' @param tags A vector of strings
#' @param ... Arguments to create_result_from_api_query
#' @param paging A named list
#'
#' @export
#' @importFrom magrittr %>%
query_mutations <- function(
  datasets = NA,
  ids = NA,
  entrez = NA,
  codes = NA,
  types = NA,
  parent_tags = NA,
  samples = NA,
  status = NA,
  tags = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "mutationId" = ids,
      "entrez" = entrez,
      "mutationCode" = codes,
      "mutationType" = types,
      "related" = parent_tags,
      "sample" = samples,
      "status" = status,
      "tag" = tags,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "mutations.txt",
    default_tbl = dplyr::tibble(
      "id" = character(),
      "entrez" = integer(),
      "hgnc" = character(),
      "code" =  character(),
      "mutation_type_name" = character(),
      "mutation_type_display" = character(),
      "mutation_name" = character()
    ),
    select_cols = c(
      "id",
      "entrez" = "gene.entrez",
      "hgnc" = "gene.hgnc",
      "code" = "mutationCode",
      "mutation_type_name" = "mutationType.name",
      "mutation_type_display" = "mutationType.display"
    ),
    ...
  ) %>%
    dplyr::mutate("mutation_name" = stringr::str_c(.data$hgnc, ":", .data$code))
}
