utils::globalVariables(".")

#' Query Snps
#'
#' @param name A vector of strings
#' @param rsid A vector of strings
#' @param chr A vector of strings
#' @param min_bp An integer
#' @param max_bp An
#' @param paging A named list
#' @param ... Arguments to create_result_from_paginated_api_query
#'
#' @export
query_snps <- function(
  name = NA,
  rsid = NA,
  chr = NA,
  min_bp = NA,
  max_bp = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_paginated_api_query(
    query_args =  list(
      "name" = name,
      "rsid" = rsid,
      "chr" = chr,
      "minBP" = min_bp,
      "maxBP" = max_bp,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "snps.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
      "rsid" = character(),
      "chr" = character(),
      "bp" = integer()
    ),
    select_cols = c(
      "name",
      "rsid",
      "chr",
      "bp"
    ),
    ...
  )
}
