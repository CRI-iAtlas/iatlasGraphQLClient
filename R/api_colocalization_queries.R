
#' Query Colocalizations
#'
#' @param datasets A vector of strings
#' @param features A vector of strings
#' @param coloc_datasets A vector of strings
#' @param entrez A vector of integers
#' @param snps A vector of strings
#' @param qtl_type A string
#' @param ecaviar_pp A string
#' @param plot_type A string
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
query_colocalizations <- function(
  datasets = NA,
  coloc_datasets = NA,
  features = NA,
  entrez = NA,
  snps = NA,
  qtl_type = NA,
  ecaviar_pp = NA,
  plot_type = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_paginated_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "colocDataSet" = coloc_datasets,
      "feature" = features,
      "entrez" = entrez,
      "snp" = snps,
      "qtlType" = qtl_type,
      "eCaviarPP" = ecaviar_pp,
      "plotType" = plot_type,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "colocalizations.txt",
    default_tbl = dplyr::tibble(
      "dataset_name" = character(),
      "dataset_display" = character(),
      "coloc_dataset_name" = character(),
      "coloc_dataset_display" = character(),
      "feature_name" = character(),
      "feature_display" = character(),
      "feature_germline_module" = character(),
      "feature_germline_category" = character(),
      "snp_name" = character(),
      "snp_rsid" = character(),
      "snp_chr" = character(),
      "snp_bp" = integer(),
      "gene_entrez" = integer(),
      "gene_hgnc" = character(),
      "qtl_type" = character(),
      "ecaviar_pp" = character(),
      "tissue" = character(),
      "plot_type" = character(),
      "splice_loc" = character(),
      "plot_link" = character()
    ),
    select_cols = c(
      "dataset_name" = "dataSet.name",
      "dataset_display" = "dataSet.display",
      "coloc_dataset_name" = "colocDataSet.name",
      "coloc_dataset_display" = "colocDataSet.display",
      "feature_name" = "feature.name",
      "feature_display" = "feature.display",
      "feature_germline_module" = "feature.germline_module",
      "feature_germline_category" = "feature.germline_category",
      "snp_name" = "snp.name",
      "snp_rsid" = "snp.rsid",
      "snp_chr" = "snp.chr",
      "snp_bp" = "snp.bp",
      "gene_entrez" = "gene.entrez",
      "gene_hgnc" = "gene.hgnc",
      "qtl_type" = "qtlType",
      "ecaviar_pp" = "eCaviarPP",
      "tissue",
      "plot_type" = "plotType",
      "splice_loc" = "spliceLoc",
      "plot_link" = "plotLink"
    ),
    ...
  )
}
