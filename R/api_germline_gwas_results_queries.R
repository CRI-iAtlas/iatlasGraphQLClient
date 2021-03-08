
#' Query Germline Gwas Results
#'
#' @param datasets A vector of strings
#' @param features A vector of strings
#' @param snps A vector of strings
#' @param min_p_value A double
#' @param max_p_value A double
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
query_germline_gwas_results <- function(
  datasets = NA,
  features = NA,
  snps = NA,
  min_p_value = NA,
  max_p_value = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_paginated_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "feature" = features,
      "snp" = snps,
      "minPValue" = min_p_value,
      "maxPValue" = max_p_value,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "germline_gwas_results.txt",
    default_tbl = dplyr::tibble(
      "dataset_name" = character(),
      "dataset_display" = character(),
      "feature_name" = character(),
      "feature_display" = character(),
      "feature_germline_module" = character(),
      "feature_germline_category" = character(),
      "snp_name" = character(),
      "snp_rsid" = character(),
      "snp_chr" = character(),
      "snp_bp" = integer(),
      "p_value" = double(),
      "maf" = character()
    ),
    select_cols = c(
      "dataset_name" = "dataSet.name",
      "dataset_display" = "dataSet.display",
      "feature_name" = "feature.name",
      "feature_display" = "feature.display",
      "feature_germline_module" = "feature.germline_module",
      "feature_germline_category" = "feature.germline_category",
      "snp_name" = "snp.name",
      "snp_rsid" = "snp.rsid",
      "snp_chr" = "snp.chr",
      "snp_bp" = "snp.bp",
      "p_value" = "pValue",
      "maf"
    ),
    ...
  )
}
