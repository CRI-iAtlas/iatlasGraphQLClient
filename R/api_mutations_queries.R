
#' Query Mutations
#'
#' @param mutations A vector of integers
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
  mutations = NA,
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
      "mutation" = mutations,
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
      "mutation_name" =  character(),
      "mutation_code" =  character(),
      "mutation_type_name" = character(),
      "mutation_type_display" = character(),
      "gene_entrez" = integer(),
      "gene_hgnc" = character()
    ),
    select_cols = c(
      "mutation_name" = "name",
      "mutation_code" = "mutationCode",
      "mutation_type_name" = "mutationType.name",
      "mutation_type_display" = "mutationType.display",
      "gene_entrez" = "gene.entrez",
      "gene_hgnc" = "gene.hgnc"
    ),
    ...
  )
}


#' Query Mutation Statuses
#'
#' @param mutations A vector of integers
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
  mutations = NA,
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
      "mutation" = mutations,
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
      "mutation_name" =  character(),
      "mutation_code" =  character(),
      "mutation_type_name" = character(),
      "mutation_type_display" = character(),
      "gene_entrez" = integer(),
      "gene_hgnc" = character(),
      "sample_name" = character(),
      "mutation_status" = character()
    ),
    select_cols = c(
      "mutation_name" = "name",
      "mutation_code" = "mutationCode",
      "mutation_type_name" = "mutationType.name",
      "mutation_type_display" = "mutationType.display",
      "gene_entrez" = "gene.entrez",
      "gene_hgnc" = "gene.hgnc",
      "samples"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "mutation_name",
        "mutation_code",
        "mutation_type_name",
        "mutation_type_display",
        "gene_entrez",
        "gene_hgnc",
        "sample_name" = "name",
        "mutation_status" = "status"
      )
  }
}
