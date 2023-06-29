#' Query Neoantigens
#'
#' @param pmhcs A vector of strings
#' @param entrez A vector of integers
#' @param patients A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
query_neoantigens <- function(
    pmhcs = NA,
    entrez = NA,
    patients = NA,
    paging = NA,
    ...
){
  create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "pmhc" = pmhcs,
      "entrez" = entrez,
      "patient" = patients,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "neoantigens.txt",
    default_tbl = dplyr::tibble(
      "tpm" = double(),
      "pmhc" = character(),
      "freq_pmhc" = integer(),
      "patient" =  character(),
      "gene_entrez" = integer(),
      "gene_hgnc" = character(),
    ),
    select_cols = c(
      "tpm" = "tpm",
      "pmhc" = "pmhc",
      "freq_pmhc" = "freqPmhc",
      "patient" = "patient.barcode",
      "gene_entrez" = "gene.entrez",
      "gene_hgnc" = "gene.hgnc"
    ),
    ...
  )
}