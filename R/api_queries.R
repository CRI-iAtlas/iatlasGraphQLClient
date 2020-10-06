utils::globalVariables(".")

# datasets --------------------------------------------------------------------

#' Query Datasets
#'
#' @param datasets A vector of strings that are names of datasets
#' @param ... Arguments to create_result_from_api_query
#' @export
query_datasets <- function(datasets = NA, ...){
  create_result_from_api_query(
    query_args = list("dataSet" = datasets),
    query_file = "datasets.txt",
    default_tbl = dplyr::tibble(
      "display" = character(), "name" = character()
    ),
    select_cols = c("display", "name"),
    arrange_cols = "display",
    ...
  )
}

#' Query Dataset Samples
#'
#' @param datasets A vector of strings that are names of datasets
#' @param ... Arguments to create_result_from_api_query
#' @export
#' @importFrom magrittr %>%
query_dataset_samples <- function(datasets, ...){
  tbl <- create_result_from_api_query(
    query_args =  list("dataSet" = datasets),
    query_file = "dataset_samples.txt",
    default_tbl = dplyr::tibble("name" = character()),
    select_cols = c("samples"),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select("name")
  }
}

# features --------------------------------------------------------------------

#' Query Features
#'
#' @param features A vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param feature_classes A vector of strings
#' @param samples A vector of strings
#' @param max_value A numeric
#' @param min_value A numeric
#' @param ... Arguments to create_result_from_api_query
#' @export
query_features <- function(
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  features = NA,
  feature_classes = NA,
  samples = NA,
  max_value = NA,
  min_value = NA,
  ...
){
  create_result_from_api_query(
    query_args = list(
      dataSet = datasets,
      related = parent_tags,
      tag = tags,
      feature = features,
      featureClass = feature_classes,
      sample = samples,
      maxValue = max_value,
      minValue = min_value
    ),
    query_file = "features.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
      "display" = character(),
      "class" = character(),
      "order" = integer(),
      "unit" =  character(),
      "method_tag" = character()
    ),
    select_cols = c(
      "name",
      "display",
      "class",
      "order",
      "unit",
      "method_tag" = "methodTag"
    ),
    arrange_cols = "display",
    ...
  )
}

#' Query Feature Values
#'
#' @param features A vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param feature_classes A vector of strings
#' @param samples A vector of strings
#' @param max_value A numeric
#' @param min_value A numeric
#' @param ... Arguments to create_result_from_api_query
#' @export
#' @importFrom magrittr %>%
query_feature_values <- function(
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  features = NA,
  feature_classes = NA,
  samples = NA,
  max_value = NA,
  min_value = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      dataSet = datasets,
      related = parent_tags,
      tag = tags,
      feature = features,
      featureClass = feature_classes,
      sample = samples,
      maxValue = max_value,
      minValue = min_value
    ),
    query_file = "feature_values.txt",
    default_tbl = dplyr::tibble(
      "sample" = character(),
      "feature_name" = character(),
      "feature_display" = character(),
      "feature_value" = double(),
      "feature_order" = integer()
    ),
    select_cols = c(
      "feature_name" = "name",
      "feature_display" = "display",
      "feature_order" = "order",
      "samples"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "sample" = "name",
        "feature_name",
        "feature_display",
        "feature_value" = "value",
        "feature_order"
      )
  }
}

#' Query Features Range
#'
#' @param features A vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param feature_classes A vector of strings
#' @param samples A vector of strings
#' @param ... Arguments to create_result_from_api_query
#' @export
#' @importFrom magrittr %>%
query_features_range <- function(
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  features = NA,
  feature_classes = NA,
  samples = NA,
  ...
){
  create_result_from_api_query(
    query_args = list(
      dataSet = datasets,
      related = parent_tags,
      tag = tags,
      feature = features,
      featureClass = feature_classes,
      sample = samples
    ),
    query_file = "features_range.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
      "display" = character(),
      "value_min" = double(),
      "value_max" = double()
    ),
    select_cols = c(
      "name",
      "display",
      "value_min" = "valueMin",
      "value_max" = "valueMax"
    ),
    arrange_cols = "display",
    ...
  )
}

# features_by_tag --------------------------------------------------------------

#' Query Feature Values By Tag
#'
#' @param features A vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param feature_classes A vector of strings
#' @param samples A vector of strings
#' @param max_value A numeric
#' @param min_value A numeric
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_feature_values_by_tag <- function(
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  features = NA,
  feature_classes = NA,
  samples = NA,
  max_value = NA,
  min_value = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      dataSet = datasets,
      related = parent_tags,
      tag = tags,
      feature = features,
      featureClass = feature_classes,
      sample = samples,
      maxValue = max_value,
      minValue = min_value
    ),
    query_file = "feature_values_by_tag.txt",
    default_tbl = dplyr::tibble(
      "tag_name" = character(),
      "tag_short_display"  = character(),
      "tag_long_display"  = character(),
      "tag_color"  = character(),
      "tag_characteristics" = character(),
      "feature_display" = character(),
      "feature_name" = character(),
      "sample" = character(),
      "value" = double()
    ),
    select_cols = c(
      "tag_name" = "tag",
      "tag_short_display"  = "shortDisplay",
      "tag_long_display"  = "longDisplay",
      "tag_color" = "color",
      "tag_characteristics" = "characteristics",
      "features"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "features", keep_empty = T) %>%
      dplyr::rename(
        "feature_name" = "name",
        "feature_display" = "display"
      ) %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::rename(
        "sample" = "name"
      )
  }
}


# features_by_class -----------------------------------------------------------

#' Query Features By Class
#'
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param features A vector of strings
#' @param feature_classes A vector of strings
#' @param ... Arguments to create_result_from_api_query
#' @export
#' @importFrom magrittr %>%
query_features_by_class <- function(
  datasets = NA,
  parent_tags = NA,
  features = NA,
  feature_classes = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      dataSet = datasets,
      related = parent_tags,
      feature = features,
      featureClass = feature_classes
    ),
    query_file = "features_by_class.txt",
    default_tbl = dplyr::tibble(
      "class" = character(),
      "display" = character(),
      "name" = character(),
      "order"  = integer(),
      "unit"  = character(),
      "method_tag"  = character()
    ),
    select_cols = c("class", "features"),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = c("features"), keep_empty = T) %>%
      dplyr::select(
        "class",
        "display",
        "name",
        "order",
        "unit",
        "method_tag" = "methodTag"
      )
  }
}

# genes -----------------------------------------------------------------------

#' Query Genes
#'
#' @param gene_types A vector of strings
#' @param entrez A vector of integers
#' @param samples A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
query_genes <- function(
  gene_types = NA,
  entrez = NA,
  samples = NA,
  ...
){
  create_result_from_api_query(
    query_args = list(
      "geneType" = gene_types,
      "entrez" = entrez,
      "sample" = samples
    ),
    query_file = "genes.txt",
    default_tbl = dplyr::tibble(
      "hgnc" = character(),
      "entrez" = integer(),
      "description" = character(),
      "friendly_name" = character(),
      "io_landscape_name" = character(),
      "gene_family" = character(),
      "gene_function" = character(),
      "immune_checkpoint" = character(),
      "pathway" = character(),
      "super_category" = character()
    ),
    select_cols = c(
      "hgnc",
      "entrez",
      "description",
      "friendly_name" = "friendlyName",
      "io_landscape_name" = "ioLandscapeName",
      "gene_family" = "geneFamily",
      "gene_function" = "geneFunction",
      "immune_checkpoint" = "immuneCheckpoint",
      "pathway",
      "super_category" = "superCategory"
    ),
    arrange_cols = "hgnc",
    ...
  )
}

#' Query Immunomodulators
#'
#' @param type A vector of strings
#' @param entrez A vector of integers
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
query_immunomodulators <- function(
  type = "immunomodulator",
  entrez = NA,
  ...
){
  create_result_from_api_query(
    query_args = list(
      geneType = type,
      entrez = entrez
    ),
    query_file = "immunomodulators.txt",
    default_tbl = dplyr::tibble(
      "entrez" = integer(),
      "hgnc" = character(),
      "friendly_name" = character(),
      "description" = character(),
      "gene_family" = character(),
      "gene_function" = character(),
      "immune_checkpoint" = character(),
      "super_category" = character(),
      "publications" = character()
    ),
    select_cols = c(
      "entrez",
      "hgnc",
      "friendly_name" = "friendlyName",
      "description",
      "gene_family" = "geneFamily",
      "gene_function" = "geneFunction",
      "immune_checkpoint" = "immuneCheckpoint",
      "super_category" = "superCategory",
      "publications"
    ),
    arrange_cols = "hgnc",
    ...
  )
}

#' Query IO Targets
#'
#' @param gene_types A vector of strings
#' @param entrez A vector of integers
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
query_io_targets <- function(gene_types = "io_target", entrez = NA, ...){
  create_result_from_api_query(
    query_args = list(geneType = gene_types, entrez = entrez),
    query_file = "io_targets.txt",
    default_tbl = dplyr::tibble(
      "entrez" = integer(),
      "hgnc" = character(),
      "friendly_name" = character(),
      "description" = character(),
      "io_landscape_name" = character(),
      "pathway" = character(),
      "therapy_type" = character()
    ),
    select_cols = c(
      "entrez",
      "hgnc",
      "description",
      "io_landscape_name" = "ioLandscapeName",
      "pathway",
      "therapy_type" = "therapyType"
    ),
    arrange_cols = "hgnc",
    ...
  )
}

#' Query Expression By Genes
#'
#' @param gene_types A vector of strings
#' @param entrez A vector of integers
#' @param samples A vector of strings
#' @param ... Arguments to create_result_from_api_query
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
query_expression_by_genes <- function(
  gene_types = NA,
  entrez = NA,
  samples = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      "geneType" = gene_types,
      "entrez" = entrez,
      "sample" = samples
    ),
    query_file = "expression_by_genes.txt",
    default_tbl = dplyr::tibble(
      "sample" = character(),
      "entrez" = character(),
      "hgnc" = character(),
      "rna_seq_expr" = double()
    ),
    select_cols = c("entrez", "hgnc", "samples"),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "sample" = "name",
        "entrez",
        "hgnc",
        "rna_seq_expr" = "rnaSeqExpr"
      )
  }
}

# genes expression by tag -----------------------------------------------------

#' Query Genes Expression By Tag
#'
#' @param features A vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param feature_classes A vector of strings
#' @param samples A vector of strings
#' @param entrez A vector of integers
#' @param gene_types A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_genes_expression_by_tag <- function(
  datasets,
  parent_tags,
  tags = NA,
  entrez = NA,
  gene_types = NA,
  features = NA,
  feature_classes = NA,
  samples = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      dataSet = datasets,
      related = parent_tags,
      tag = tags,
      entrez = entrez,
      geneType = gene_types,
      feature = features,
      featureClass = feature_classes,
      sample = samples
    ),
    query_file = "genes_expression_by_tag.txt",
    default_tbl = dplyr::tibble(
      "tag_name" = character(),
      "tag_long_display"  = character(),
      "tag_short_display"  = character(),
      "tag_color"  = character(),
      "tag_characteristics"  = character(),
      "entrez" = integer(),
      "hgnc" = character(),
      "sample" = character(),
      "rna_seq_expr" = character()
    ),
    select_cols = c(
      "tag_name" = "tag",
      "tag_long_display" = "longDisplay",
      "tag_short_display" = "shortDisplay",
      "tag_color" = "color",
      "tag_characteristics" = "characteristics",
      "genes"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "genes", keep_empty = T) %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::rename(
        "sample" = "name",
        "rna_seq_expr" = "rnaSeqExpr"
      )
  }
}

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
