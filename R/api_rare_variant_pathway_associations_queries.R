
#' Query Rare Variant Pathway Associations
#'
#' @param datasets A vector of strings
#' @param features A vector of strings
#' @param pathways A vector of strings
#' @param min_p_value A Double
#' @param max_p_value A Double
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
query_rare_variant_pathway_associations <- function(
  datasets = NA,
  features = NA,
  pathways = NA,
  min_p_value = NA,
  max_p_value = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "feature" = features,
      "pathway" = pathways,
      "minPValue" = min_p_value,
      "maxPValue" = max_p_value,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "rare_variant_pathway_associations.txt",
    default_tbl = dplyr::tibble(
      "dataset_name" = character(),
      "dataset_display" = character(),
      "feature_name" = character(),
      "feature_display" = character(),
      "feature_germline_module" = character(),
      "feature_germline_category" = character(),
      "pathway" = character(),
      "p_value" = double(),
      "min" = double(),
      "max" = double(),
      "mean"= double(),
      "q1" = double(),
      "q2" = double(),
      "q3" = double(),
      "n_mutants" = integer(),
      "n_total" = integer()
    ),
    select_cols = c(
      "dataset_name" = "dataSet.name",
      "dataset_display" = "dataSet.display",
      "feature_name" = "feature.name",
      "feature_display" = "feature.display",
      "feature_germline_module" = "feature.germlineModule",
      "feature_germline_category" = "feature.germlineCategory",
      "pathway",
      "p_value" = "pValue",
      "min",
      "max",
      "mean",
      "q1",
      "q2",
      "q3",
      "n_mutants" = "nMutants",
      "n_total" = "nTotal"
    ),
    ...
  )
}
