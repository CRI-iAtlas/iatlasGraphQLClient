utils::globalVariables(".")

# copy number results ---------------------------------------------------------

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
  tbl <- create_result_from_api_query(
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
    default_tbl = dplyr::tibble("name" = character()),
    # select_cols = c("")
    ...
  )

}

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
#' @param feature A vector of strings
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param samples A vector of strings
#' @param ... Arguments to create_result_from_api_query
#' @export
#' @importFrom magrittr %>%
query_feature_values_by_tag <- function(
  feature,
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  samples = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      dataSet = datasets,
      related = parent_tags,
      tag = tags,
      feature = feature,
      sample = samples
    ),
    query_file = "feature_values_by_tag.txt",
    default_tbl = dplyr::tibble(
      "tag_name" = character(),
      "tag_display"  = character(),
      "tag_color"  = character(),
      "tag_characteristics"  = character(),
      "sample" = character(),
      "value" = double()
    ),
    select_cols = c(
      "tag_name" = "tag",
      "tag_display" = "display",
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
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "tag_name",
        "tag_display",
        "tag_color",
        "tag_characteristics",
        "sample" = "name",
        "value"
      )
  }
}

#' Query Features Values By Tag
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
query_features_values_by_tag <- function(
  datasets = NA,
  parent_tags = NA,
  tags = NA,
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
      feature = features,
      featureClass = feature_classes,
      sample = samples
    ),
    query_file = "features_values_by_tag.txt",
    default_tbl = dplyr::tibble(
      "tag_name" = character(),
      "tag_display"  = character(),
      "tag_color"  = character(),
      "tag_characteristics"  = character(),
      "sample" = character(),
      "feature_name" = character(),
      "feature_display" = character(),
      "feature_value" = double(),
      "feature_order" = integer()
    ),
    select_cols = c(
      "tag_name" = "tag",
      "tag_display" = "display",
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
      dplyr::select(
        "tag_name",
        "tag_display",
        "tag_color",
        "tag_characteristics",
        "feature_name" = "name",
        "feature_display" = "display",
        "feature_order" = "order",
        "samples"
      ) %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "tag_name",
        "tag_display",
        "tag_color",
        "tag_characteristics",
        "sample" = "name",
        "feature_name",
        "feature_display",
        "feature_value" = "value",
        "feature_order"
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
      "tag_display"  = character(),
      "tag_color"  = character(),
      "tag_characteristics"  = character(),
      "sample" = character(),
      "entrez" = integer(),
      "hgnc" = character(),
      "rna_seq_expr" = character()
    ),
    select_cols = c(
      "tag_name" = "tag",
      "tag_display" = "display",
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
      dplyr::select(
        "tag_name",
        "tag_display",
        "tag_color",
        "tag_characteristics",
        "entrez",
        "hgnc",
        "samples"
      ) %>%
      tidyr::unnest(cols = "samples", keep_empty = T) %>%
      dplyr::select(
        "tag_name",
        "tag_display",
        "tag_color",
        "tag_characteristics",
        "sample" = "name",
        "entrez",
        "hgnc",
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

# mutations -------------------------------------------------------------------

#' query_mutations
#'
#' @param ids A vector of integers
#' @param entrez A vector of integers
#' @param codes A vector of strings
#' @param types A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_mutations <- function(ids = NA, entrez = NA, codes = NA, types = NA, ...){
  tbl <- create_result_from_api_query(
    query_args =  list(
      "mutationId" = ids,
      "entrez" = entrez,
      "mutationCode" = codes,
      "mutationType" = types
    ),
    query_file = "mutations.txt",
    default_tbl = dplyr::tibble(
      "id" = character(),
      "code" =  character(),
      "entrez" = integer(),
      "hgnc" = character()
    ),
    select_cols = c(
      "id",
      "code" = "mutationCode",
      "entrez" = "gene.entrez",
      "hgnc" = "gene.hgnc"
    ),
    ...
  )
}

# patients --------------------------------------------------------------------

#TODO: Improve: https://gitlab.com/cri-iatlas/iatlas-api/-/issues/28, https://gitlab.com/cri-iatlas/iatlas-api/-/issues/29
#' Query Patients
#'
#' @param patients A list of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_patients <- function(patients, ...){
  tbl <- create_result_from_api_query(
    query_args =  list("barcode" = patients),
    query_file = "patients.txt",
    default_tbl = dplyr::tibble(
      "patient" = character(),
      "age_at_diagnosis" = numeric(),
      "ethnicity" = character(),
      "gender" = character(),
      "height" = numeric(),
      "race" = character(),
      "weight" = numeric()
    ),
    select_cols = c(
      "patient" = "barcode",
      "age_at_diagnosis" = "ageAtDiagnosis",
      "ethnicity",
      "gender",
      "height",
      "race",
      "weight"
    ),
    ...
  )
}

# related ---------------------------------------------------------------------

#' Query Dataset Tags
#'
#' @param dataset A string
#' @param ... Arguments to create_result_from_api_query
#' @export
#' @importFrom magrittr %>%
query_dataset_tags <- function(dataset, ...){
  tbl <- create_result_from_api_query(
    query_args =  list("dataSet" = dataset, "related" = NA),
    query_file = "dataset_tags.txt",
    default_tbl = dplyr::tibble("name" = character(), "display" = character()),
    select_cols = c("related"),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = c("related"), keep_empty = T) %>%
      dplyr::select("name", "display")
  }
}

# samples ---------------------------------------------------------------------

#TODO: Improve: https://gitlab.com/cri-iatlas/iatlas-api/-/issues/29
#' Query Samples
#'
#' @param samples A list of strings
#' @param patients A list of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_samples <- function(samples = NA, patients = NA, ...){
  tbl <- create_result_from_api_query(
    query_args =  list("name" = samples, "patient" = patients),
    query_file = "samples.txt",
    default_tbl = dplyr::tibble("name" = character()),
    select_cols = c("name"),
    ...
  )
}

#TODO: Improve: https://gitlab.com/cri-iatlas/iatlas-api/-/issues/29
#' Query Sample Patients
#'
#' @param samples A list of strings
#' @param patients A list of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_sample_patients <- function(samples = NA, patients = NA, ...){
  tbl <- create_result_from_api_query(
    query_args =  list("name" = samples, "patient" = patients),
    query_file = "sample_patients.txt",
    default_tbl = dplyr::tibble(
      "sample" = character(),
      "patient" = character(),
      "age_at_diagnosis" = numeric(),
      "ethnicity" = character(),
      "gender" = character(),
      "height" = numeric(),
      "race" = character(),
      "weight" = numeric()
    ),
    select_cols = c(
      "sample" = "name",
      "patient" = "patient.barcode",
      "age_at_diagnosis" = "patient.ageAtDiagnosis",
      "ethnicity" = "patient.ethnicity",
      "gender" = "patient.gender",
      "height" = "patient.height",
      "race" = "patient.race",
      "weight" = "patient.weight"
    ),
    ...
  )
}

#TODO: add query: https://gitlab.com/cri-iatlas/iatlas-api/-/issues/30

# samples by mutation status --------------------------------------------------

#' query_samples_by_mutation_status
#'
#' @param ids A vector of integers
#' @param status A string, either "Wt" or "Mut"
#' @param samples A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_samples_by_mutation_status <- function(
  ids = NA,
  status = NA,
  samples = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      "mutationId" = ids,
      "mutationStatus"= status,
      "sample" = samples
    ),
    query_file = "samples_by_mutation_status.txt",
    default_tbl = dplyr::tibble("sample" = character(), "status" = character()),
    select_cols = c("status", "samples"),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest("samples") %>%
      dplyr::select("sample" = "name", "status")
  }
}

# samples by tag --------------------------------------------------------------

#' Query Samples By Tag
#'
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param features A vector of strings
#' @param feature_classes A vector of strings
#' @param samples A vector of strings
#' @param patients A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_samples_by_tag <- function(
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  features = NA,
  feature_classes = NA,
  samples = NA,
  patients = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "feature" = features,
      "featureClass" = feature_classes,
      "name" = samples,
      "patient" = patients
    ),
    query_file = "samples_by_tag.txt",
    default_tbl = dplyr::tibble(
      "tag_name" = character(),
      "tag_display" = character(),
      "tag_characteristics" = character(),
      "tag_color" = character(),
      "sample" = character()
    ),
    select_cols = c(
      "tag_name" = "tag",
      "tag_display" = "display",
      "tag_characteristics" = "characteristics",
      "tag_color" = "color",
      "samples"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest("samples") %>%
      dplyr::select(
        "tag_name",
        "tag_display",
        "tag_characteristics",
        "tag_color",
        "sample" = "name"
      )
  }
}

#' Query Tag Samples
#'
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param features A vector of strings
#' @param feature_classes A vector of strings
#' @param samples A vector of strings
#' @param patients A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tag_samples <- function(
  datasets = NA,
  parent_tags = NA,
  tags = NA,
  features = NA,
  feature_classes = NA,
  samples = NA,
  patients = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "feature" = features,
      "featureClass" = feature_classes,
      "name" = samples,
      "patient" = patients
    ),
    query_file = "tag_samples.txt",
    default_tbl = dplyr::tibble("sample" = character()),
    select_cols = c("samples"),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest("samples") %>%
      dplyr::select("sample" = "name")
  }
}

# slides ----------------------------------------------------------------------

#' query_slides
#'
#' @param slides A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_slides <- function(slides = NA, ...){
  tbl <- create_result_from_api_query(
    query_args =  list("name" = slides),
    query_file = "slides.txt",
    default_tbl = dplyr::tibble(
      "slide" = character(),
      "description" = character(),
      "patient" = character()
    ),
    select_cols = c(
      "slide" = "name",
      "description",
      "patient" = "patient.barcode"
    ),
    ...
  )
}

# tags ------------------------------------------------------------------------

#' Query Tags
#'
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param features A vector of strings
#' @param feature_classes A vector of strings
#' @param samples A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_tags <- function(
  datasets,
  parent_tags,
  tags = NA,
  features = NA,
  feature_classes = NA,
  samples = NA,
  ...
){
  create_result_from_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "feature" = features,
      "featureClass" = feature_classes,
      "sample" = samples
    ),
    query_file = "tags.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
      "display" = character(),
      "characteristics" = character(),
      "color" = character(),
      "sample_count" = integer()
    ),
    select_cols = c(
      "name",
      "display",
      "characteristics",
      "color",
      "sample_count" = "sampleCount"
    ),
    arrange_cols = "name",
    ...
  )
}

#' query_cohort_selector
#'
#'
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param features A vector of strings
#' @param feature_classes A vector of strings
#' @param samples A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_cohort_selector <- function(
  datasets,
  parent_tags,
  tags = NA,
  features = NA,
  feature_classes = NA,
  samples = NA,
  ...
){
  create_result_from_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "related" = parent_tags,
      "tag" = tags,
      "feature" = features,
      "featureClass" = feature_classes,
      "sample" = samples
    ),
    query_file = "cohort_selection.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
      "display" = character(),
      "characteristics" = character(),
      "color" = character(),
      "size" = integer(),
      "samples" = list()
    ),
    select_cols = c(
      "name",
      "display",
      "characteristics",
      "color",
      "size" = "sampleCount",
      "samples"
    ),
    arrange_cols = "name",
    ...
  )
}
