utils::globalVariables(".")

# gene types ------------------------------------------------------------------

#' Query Gene Types
#'
#' @param gene_types A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
query_gene_types <- function(gene_types = NA, ...){
  create_result_from_api_query(
    query_args =  list("name" = gene_types),
    query_file = "gene_types.txt",
    default_tbl = dplyr::tibble(
      "display" = character(),
      "name" = character()
    ),
    select_cols = c("display", "name"),
    arrange_cols = "display",
    ...
  )
}

#' Query Genes By Gene Types
#'
#' @param gene_types A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
query_genes_by_gene_types <- function(gene_types = NA, ...){
  tbl <- create_result_from_api_query(
    query_args =  list("name" = gene_types),
    query_file = "genes_by_gene_type.txt",
    default_tbl = dplyr::tibble(
      "entrez" = integer(),
      "hgnc" = character(),
      "gene_type_name" = character(),
      "gene_type_display" = character()
    ),
    select_cols = c("name", "display", "genes"),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "genes", keep_empty = T) %>%
      dplyr::select(
        "entrez",
        "hgnc",
        "gene_type_name" = "name",
        "gene_type_display" = "display",
      )
  }
}
