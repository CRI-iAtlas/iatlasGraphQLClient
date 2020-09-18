utils::globalVariables(".")

#' Query Driver Results
#'
#' @param datasets A vector of strings
#' @param tags A vector of strings
#' @param features A vector of strings
#' @param entrez A vector of strings
#' @param mutation_codes A vector of strings
#' @param min_p_value A double
#' @param max_p_value A double
#' @param min_log10_p_value A double
#' @param max_log10_p_value A double
#' @param min_fold_change A double
#' @param min_log10_fold_change A double
#' @param min_num_wild_types An integer
#' @param min_num_mutants An integer
#' @param page An integer
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
query_driver_results <- function(
  datasets = NA,
  tags = NA,
  features = NA,
  entrez = NA,
  mutation_codes = NA,
  min_p_value = NA,
  max_p_value = NA,
  min_log10_p_value = NA,
  max_log10_p_value = NA,
  min_fold_change = NA,
  min_log10_fold_change = NA,
  min_num_wild_types = NA,
  min_num_mutants = NA,
  page = NA,
  ...
){
  tbl <- create_result_from_paginated_api_query(
    query_args =  list(
      dataSet = datasets,
      tag = tags,
      feature = features,
      entrez = entrez,
      mutationCode = mutation_codes,
      minPValue = min_p_value,
      maxPValue = max_p_value,
      minLog10PValue = min_log10_p_value,
      maxLog10PValue = max_log10_p_value,
      minFoldChange = min_fold_change,
      minLog10FoldChange = min_log10_fold_change,
      minNumWildTypes = min_num_wild_types,
      minNumMutants = min_num_mutants,
      page = page
    ),
    query_file = "driver_results.txt",
    default_tbl = dplyr::tibble(
      "dataset_name" = character(),
      "dataset_display" = character(),
      "feature_name" = character(),
      "feature_display" = character(),
      "tag_name" = character(),
      "tag_display" = character(),
      "entrez" = integer(),
      "hgnc" = character(),
      "mutation_code" =  character(),
      "p_value" = double(),
      "fold_change" = double(),
      "log10_p_value" = double(),
      "log10_fold_change" = double(),
      "num_mutant" = integer(),
      "num_wild_types" = integer(),
    ),
    select_cols = c(
      "dataset_name" = "dataSet.name",
      "dataset_display" = "dataSet.display",
      "feature_name" = "feature.name",
      "feature_display" = "feature.display",
      "tag_name" = "tag.name",
      "tag_display" = "tag.display",
      "entrez" = "gene.entrez",
      "hgnc" = "gene.hgnc",
      "mutation_code" = "mutationCode",
      "p_value" = "pValue",
      "fold_change" = "foldChange",
      "log10_p_value" = "log10PValue",
      "log10_fold_change" = "log10FoldChange",
      "num_mutant" = "numMutants",
      "num_wild_types" = "numWildTypes"
    ),
    ...
  )
}
