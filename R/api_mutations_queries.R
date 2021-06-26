
#' Query Mutations
#'
#' @param ids A vector of integers
#' @param entrez A vector of integers
#' @param codes A vector of strings
#' @param types A vector of strings
#' @param cohorts A vector of strings
#' @param samples A vector of strings
#' @param status A string
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_mutations <- function(
  ids = NA,
  entrez = NA,
  codes = NA,
  types = NA,
  cohorts = NA,
  samples = NA,
  status = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "mutationId" = ids,
      "entrez" = entrez,
      "mutationCode" = codes,
      "mutationType" = types,
      "cohort" = cohorts,
      "sample" = samples,
      "status" = status,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "mutations.txt",
    default_tbl = dplyr::tibble(
      "mutation_id" = character(),
      "gene_entrez" = integer(),
      "gene_hgnc" = character(),
      "mutation_code" =  character(),
      "mutation_type_name" = character(),
      "mutation_type_display" = character(),
      "mutation_name" = character()
    ),
    select_cols = c(
      "mutation_id" = "id",
      "gene_entrez" = "gene.entrez",
      "gene_hgnc" = "gene.hgnc",
      "mutation_code" = "mutationCode",
      "mutation_type_name" = "mutationType.name",
      "mutation_type_display" = "mutationType.display"
    ),
    ...
  ) %>%
    dplyr::mutate("mutation_name" = stringr::str_c(.data$gene_hgnc, ":", .data$mutation_code))
}


#' Query Mutation Statuses
#'
#' @param ids A vector of integers
#' @param entrez A vector of integers
#' @param codes A vector of strings
#' @param types A vector of strings
#' @param cohorts A vector of strings
#' @param samples A vector of strings
#' @param status A string
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_mutation_statuses <- function(
  ids = NA,
  entrez = NA,
  codes = NA,
  types = NA,
  cohorts = NA,
  samples = NA,
  status = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "mutationId" = ids,
      "entrez" = entrez,
      "mutationCode" = codes,
      "mutationType" = types,
      "cohort" = cohorts,
      "sample" = samples,
      "status" = status,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "mutation_statuses.txt",
    default_tbl = dplyr::tibble(
      "mutation_id" = character(),
      "mutation_name" = character(),
      "gene_entrez" = integer(),
      "gene_hgnc" = character(),
      "mutation_code" =  character(),
      "mutation_type_name" = character(),
      "mutation_type_display" = character(),
      "sample_name" = character(),
      "mutation_status" = character()
    ),
    select_cols = c(
      "mutation_id" = "id",
      "gene_entrez" = "gene.entrez",
      "gene_hgnc" = "gene.hgnc",
      "mutation_code" = "mutationCode",
      "mutation_type_name" = "mutationType.name",
      "mutation_type_display" = "mutationType.display",
      "samples"
    ),
    ...
  ) %>%
    dplyr::mutate("mutation_name" = stringr::str_c(.data$gene_hgnc, ":", .data$mutation_code))
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "mutation_id",
        "mutation_name",
        "gene_entrez",
        "gene_hgnc",
        "mutation_code",
        "mutation_type_name",
        "mutation_type_display",
        "sample_name" = "name",
        "mutation_status" = "status"
      )
  }
}
