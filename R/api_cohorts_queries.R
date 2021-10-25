#' Query Cohorts
#'
#' @param cohorts A vector of strings
#' @param datasets A vector of strings
#' @param tags A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
query_cohorts <- function(
  cohorts = NA,
  datasets = NA,
  tags = NA,
  paging = NA,
  ...
){
  create_result_from_cursor_paginated_api_query(
    query_args = list(
      "cohort" = cohorts,
      "dataSet" = datasets,
      "tag" = tags,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "cohorts.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
    ),
    select_cols = c(
      "name",
      "dataset_name" = "dataSet.name",
      "dataset_display" = "dataSet.display",
      "tag_name" = "tag.name",
      "tag_long_display" = "tag.longDisplay",
      "tag_short_display" = "tag.shortDisplay"
    ),
    arrange_cols = "name",
  )
}

#' Query Cohort Features
#'
#' @param cohorts A vector of strings
#' @param datasets A vector of strings
#' @param tags A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
query_cohort_features <- function(
  cohorts = NA,
  datasets = NA,
  tags = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args = list(
      "cohort" = cohorts,
      "dataSet" = datasets,
      "tag" = tags,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "cohort_features.txt",
    default_tbl = dplyr::tibble(
      "cohort_name" = character(),
      "feature_name" = character(),
      "feature_display" = character()
    ),
    select_cols = c(
      "cohort_name" = "name",
      "features"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl <- tbl %>%
      tidyr::unnest(cols = "features", keep_empty = T) %>%
      dplyr::select(
        "cohort_name",
        "feature_name" = "name",
        "feature_display" = "display"
      )
    return(tbl)
  }
}

#' Query Cohort Genes
#'
#' @param cohorts A vector of strings
#' @param datasets A vector of strings
#' @param tags A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
query_cohort_genes <- function(
  cohorts = NA,
  datasets = NA,
  tags = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args = list(
      "cohort" = cohorts,
      "dataSet" = datasets,
      "tag" = tags,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "cohort_genes.txt",
    default_tbl = dplyr::tibble(
      "cohort_name" = character(),
      "gene_entrez" = integer(),
      "gene_hgnc" = character()
    ),
    select_cols = c(
      "cohort_name" = "name",
      "genes"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl <- tbl %>%
      tidyr::unnest(cols = "genes", keep_empty = T) %>%
      dplyr::select(
        "cohort_name",
        "gene_entrez" = "entrez",
        "gene_hgnc" = "hgnc"
      )
    return(tbl)
  }
}

#' Query Cohort Mutations
#'
#' @param cohorts A vector of strings
#' @param datasets A vector of strings
#' @param tags A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
query_cohort_mutations <- function(
  cohorts = NA,
  datasets = NA,
  tags = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args = list(
      "cohort" = cohorts,
      "dataSet" = datasets,
      "tag" = tags,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "cohort_mutations.txt",
    default_tbl = dplyr::tibble(
      "cohort_name" = character(),
      "mutation_code" = character(),
      "mutation_gene_entrez" = integer()
    ),
    select_cols = c(
      "cohort_name" = "name",
      "mutations"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl <- tbl %>%
      tidyr::unnest(cols = "mutations", keep_empty = T) %>%
      dplyr::select(
        "cohort_name",
        "mutation_code" = "mutationCode",
        "mutation_gene" = "gene"
      )
    if(typeof(tbl$mutation_gene) == "list"){
      tbl <- unnest_df_column(tbl, "mutation_gene")
    } else {
      tbl <-
        dplyr::rename(tbl, "mutation_gene_entrez" = "mutation_gene")
    }
    return(tbl)
  }
}

#' Query Cohort Samples
#'
#' @param cohorts A vector of strings
#' @param datasets A vector of strings
#' @param tags A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
query_cohort_samples <- function(
  cohorts = NA,
  datasets = NA,
  tags = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args = list(
      "cohort" = cohorts,
      "dataSet" = datasets,
      "tag" = tags,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "cohort_samples.txt",
    default_tbl = dplyr::tibble(
      "cohort_name" = character(),
      "sample_name" = character(),
      "dataset_name" = character(),
      "dataset_display" = character(),
      "tag_name" = character()
    ),
    select_cols = c(
      "cohort_name" = "name",
      "samples",
      "dataset_name" = "dataSet.name",
      "dataset_display" = "dataSet.display"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl <- tbl %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "cohort_name",
        "sample_name" = "name",
        "dataset_name",
        "dataset_display",
        "tag"
      )
    if(typeof(tbl$tag) == "list"){
      tbl <- unnest_df_column(tbl, "tag")
    } else {
      tbl <- dplyr::rename(tbl, "tag_name" = "tag")
    }
    return(tbl)
  }
}
