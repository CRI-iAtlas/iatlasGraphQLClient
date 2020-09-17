utils::globalVariables(".")

#' Query Copy Number Results
#'
#' @param datasets A vector of strings
#' @param tags A vector of strings
#' @param features A vector of strings
#' @param entrez A vector of strings
#' @param direction A string, either Amp' or 'Del'
#' @param min_p_value A double
#' @param max_p_value A double
#' @param min_log10_p_value A double
#' @param max_log10_p_value A double
#' @param min_mean_normal A double
#' @param min_mean_cnv A double
#' @param min_t_stat A double
#' @param page An integer
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
query_copy_number_results <- function(
  datasets = NA,
  tags = NA,
  features = NA,
  entrez = NA,
  direction = NA,
  min_p_value = NA,
  max_p_value = NA,
  min_log10_p_value = NA,
  max_log10_p_value = NA,
  min_mean_normal = NA,
  min_mean_cnv = NA,
  min_t_stat = NA,
  page = NA,
  ...
){
  tbl <- create_result_from_paginated_api_query(
    query_args =  list(
      dataSet = datasets,
      tag = tags,
      feature = features,
      entrez = entrez,
      direction = direction,
      minPValue = min_p_value,
      maxPValue = max_p_value,
      minLog10PValue = min_log10_p_value,
      maxLog10PValue = max_log10_p_value,
      minMeanNormal = min_mean_normal,
      minMeanCnv = min_mean_cnv,
      minTStat = min_t_stat,
      page = page
    ),
    query_file = "copy_number_results.txt",
    default_tbl = dplyr::tibble(
      "direction" = character(),
      "mean_normal" = double(),
      "mean_cnv" = double(),
      "p_value" = double(),
      "log10_p_value" = double(),
      "t_stat" = double(),
      "dataset_name" = character(),
      "dataset_display" = character(),
      "feature_name" = character(),
      "feature_display" = character(),
      "tag_name" = character(),
      "tag_display" = character(),
      "entrez" = integer(),
      "hgnc" = character()
    ),
    select_cols = c(
      "direction",
      "mean_normal" = "meanNormal",
      "mean_cnv" = "meanCnv",
      "p_value" = "pValue",
      "log10_p_value" = "log10PValue",
      "t_stat" = "tStat",
      "dataset_name" = "dataSet.name",
      "dataset_display" = "dataSet.display",
      "feature_name" = "feature.name",
      "feature_display" = "feature.display",
      "tag_name" = "tag.name",
      "tag_display" = "tag.display",
      "entrez" = "gene.entrez",
      "hgnc" = "gene.hgnc"
    ),
    ...
  )
}

#' Query Copy Number Results2
#'
#' @param datasets A vector of strings
#' @param tags A vector of strings
#' @param features A vector of strings
#' @param entrez A vector of strings
#' @param direction A string, either Amp' or 'Del'
#' @param min_p_value A double
#' @param max_p_value A double
#' @param min_log10_p_value A double
#' @param max_log10_p_value A double
#' @param min_mean_normal A double
#' @param min_mean_cnv A double
#' @param min_t_stat A double
#' @param page An integer
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
query_copy_number_results2 <- function(
  datasets = NA,
  tags = NA,
  features = NA,
  entrez = NA,
  direction = NA,
  min_p_value = NA,
  max_p_value = NA,
  min_log10_p_value = NA,
  max_log10_p_value = NA,
  min_mean_normal = NA,
  min_mean_cnv = NA,
  min_t_stat = NA,
  page = NA,
  ...
){
  tbl <- create_result_from_paginated_api_query(
    query_args =  list(
      dataSet = datasets,
      tag = tags,
      feature = features,
      entrez = entrez,
      direction = direction,
      minPValue = min_p_value,
      maxPValue = max_p_value,
      minLog10PValue = min_log10_p_value,
      maxLog10PValue = max_log10_p_value,
      minMeanNormal = min_mean_normal,
      minMeanCnv = min_mean_cnv,
      minTStat = min_t_stat,
      page = page
    ),
    query_file = "copy_number_results2.txt",
    default_tbl = dplyr::tibble(
      "direction" = character(),
      "mean_normal" = double(),
      "mean_cnv" = double(),
      "p_value" = double(),
      "log10_p_value" = double(),
      "t_stat" = double(),
      "tag_name" = character(),
      "tag_display" = character(),
      "hgnc" = character()
    ),
    select_cols = c(
      "direction",
      "mean_normal" = "meanNormal",
      "mean_cnv" = "meanCnv",
      "p_value" = "pValue",
      "log10_p_value" = "log10PValue",
      "t_stat" = "tStat",
      "tag_name" = "tag.name",
      "tag_display" = "tag.display",
      "hgnc" = "gene.hgnc"
    ),
    ...
  )
}

#' Query Copy Number Result Genes
#'
#' @param datasets A vector of strings
#' @param tags A vector of strings
#' @param features A vector of strings
#' @param entrez A vector of strings
#' @param direction A string, either Amp' or 'Del'
#' @param min_p_value A double
#' @param max_p_value A double
#' @param min_log10_p_value A double
#' @param max_log10_p_value A double
#' @param min_mean_normal A double
#' @param min_mean_cnv A double
#' @param min_t_stat A double
#' @param page An integer
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
query_copy_number_result_genes <- function(
  datasets = NA,
  tags = NA,
  features = NA,
  entrez = NA,
  direction = NA,
  min_p_value = NA,
  max_p_value = NA,
  min_log10_p_value = NA,
  max_log10_p_value = NA,
  min_mean_normal = NA,
  min_mean_cnv = NA,
  min_t_stat = NA,
  page = NA,
  ...
){
  tbl <- create_result_from_paginated_api_query(
    query_args =  list(
      dataSet = datasets,
      tag = tags,
      feature = features,
      entrez = entrez,
      direction = direction,
      minPValue = min_p_value,
      maxPValue = max_p_value,
      minLog10PValue = min_log10_p_value,
      maxLog10PValue = max_log10_p_value,
      minMeanNormal = min_mean_normal,
      minMeanCnv = min_mean_cnv,
      minTStat = min_t_stat,
      page = page
    ),
    query_file = "copy_number_result_genes.txt",
    default_tbl = dplyr::tibble("entrez" = character(), "hgnc" = character()),
    select_cols = c("entrez" = "gene.entrez", "hgnc" = "gene.hgnc"),
    ...
  )
}
