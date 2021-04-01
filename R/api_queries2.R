
# mutations by sample --------------------------------------------------------

#' Query Mutations BY Sample
#'
#' @param entrez A vector of integers
#' @param samples A vector of strings
#' @param mutation_codes A vector of strings
#' @param mutation_types A vector of strings
#' @param mutation_ids A string
#' @param mutation_status An integer
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_mutations_by_sample <- function(
  entrez = NA,
  mutation_codes = NA,
  mutation_types = NA,
  mutation_ids = NA,
  mutation_status = NA,
  samples = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "entrez" = entrez,
      "mutationCode" = mutation_codes,
      "mutationType" = mutation_types,
      "mutationId" = mutation_ids,
      "status" = mutation_status,
      "sample" = samples,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "mutations_by_sample.txt",
    default_tbl = dplyr::tibble(
      "sample" = character(),
      "mutation_id" = integer(),
      "entrez" = integer(),
      "hgnc" = character(),
      "code" = character(),
      "mutation_type_name"  = character(),
      "mutation_type_display" = character(),
      "status" = character(),
      "mutation_name" = character()
    ),
    unnest_cols = "mutations",
    select_cols = c(
      "sample" = "name",
      "mutation_id" = "id",
      "entrez" = "gene.entrez",
      "hgnc" = "gene.hgnc",
      "code" = "mutationCode",
      "mutation_type_name" = "mutationType.name",
      "mutation_type_display" = "mutationType.display",
      "status"
    ),
    ...
  ) %>%
    dplyr::mutate("mutation_name" = stringr::str_c(.data$hgnc, ":", .data$code))
}

# patients --------------------------------------------------------------------

#' Query Patients
#'
#' @param patients A list of strings
#' @param datasets A vector of strings
#' @param ethnicities A vector of strings
#' @param genders A vector of strings
#' @param races A vector of strings
#' @param samples A vector of strings
#' @param slides A vector of strings
#' @param max_age_at_diagnosis An integer
#' @param max_height A float
#' @param max_weight A float
#' @param min_age_at_diagnosis An integer
#' @param min_height A float
#' @param min_weight A float
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_patients <- function(
  patients = NA,
  datasets = NA,
  ethnicities  = NA,
  genders  = NA,
  races  = NA,
  samples  = NA,
  slides  = NA,
  max_age_at_diagnosis  = NA,
  max_height = NA,
  max_weight  = NA,
  min_age_at_diagnosis  = NA,
  min_height  = NA,
  min_weight = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      "barcode" = patients,
      "dataSet" = datasets,
      "ethnicity" = ethnicities,
      "gender" = genders,
      "race" = races,
      "sample" = samples,
      "slide" = slides,
      "maxAgeAtDiagnosis" = max_age_at_diagnosis,
      "maxHeight" = max_height,
      "maxWeight" = max_weight,
      "minAgeAtDiagnosis" = min_age_at_diagnosis,
      "minHeight" = min_height,
      "minWeight" = min_weight
    ),
    query_file = "patients.txt",
    default_tbl = dplyr::tibble(
      "age_at_diagnosis" = numeric(),
      "patient" = character(),
      "ethnicity" = character(),
      "gender" = character(),
      "height" = numeric(),
      "race" = character(),
      "weight" = numeric()
    ),
    select_cols = c(
      "age_at_diagnosis" = "ageAtDiagnosis",
      "patient" = "barcode",
      "ethnicity",
      "gender",
      "height",
      "race",
      "weight"
    ),
    ...
  )
}

#' Query Patient Slides
#'
#' @param patients A list of strings
#' @param datasets A vector of strings
#' @param ethnicities A vector of strings
#' @param genders A vector of strings
#' @param races A vector of strings
#' @param samples A vector of strings
#' @param slides A vector of strings
#' @param max_age_at_diagnosis An integer
#' @param max_height A float
#' @param max_weight A float
#' @param min_age_at_diagnosis An integer
#' @param min_height A float
#' @param min_weight A float
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_patient_slides <- function(
  patients = NA,
  datasets = NA,
  ethnicities  = NA,
  genders  = NA,
  races  = NA,
  samples  = NA,
  slides  = NA,
  max_age_at_diagnosis  = NA,
  max_height = NA,
  max_weight  = NA,
  min_age_at_diagnosis  = NA,
  min_height  = NA,
  min_weight = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      "barcode" = patients,
      "dataSet" = datasets,
      "ethnicity" = ethnicities,
      "gender" = genders,
      "race" = races,
      "sample" = samples,
      "slide" = slides,
      "maxAgeAtDiagnosis" = max_age_at_diagnosis,
      "maxHeight" = max_height,
      "maxWeight" = max_weight,
      "minAgeAtDiagnosis" = min_age_at_diagnosis,
      "minHeight" = min_height,
      "minWeight" = min_weight
    ),
    query_file = "patient_slides.txt",
    default_tbl = dplyr::tibble(
      "patient" = character(),
      "slide" = character(),
      "slide_description" = character()
    ),
    unnest_cols = "slides",
    select_cols = c(
      "patient" = "barcode",
      "slide" = "name",
      "slide_description" = "description"
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
    default_tbl = dplyr::tibble(
      "long_display" = character(),
      "name" = character(),
      "short_display" = character()
    ),
    select_cols = c("related"),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = c("related"), keep_empty = T) %>%
      dplyr::rename(
        "long_display" = "longDisplay",
        "short_display" = "shortDisplay"
      )
  }
}

# samples ---------------------------------------------------------------------

#' Query Samples
#'
#' @param samples A vector of strings
#' @param patients A list of strings
#' @param ethnicities A vector of strings
#' @param genders A vector of strings
#' @param races A vector of strings
#' @param max_age_at_diagnosis An integer
#' @param max_height A float
#' @param max_weight A float
#' @param min_age_at_diagnosis An integer
#' @param min_height A float
#' @param min_weight A float
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_samples <- function(
  samples = NA,
  patients = NA,
  ethnicities  = NA,
  genders  = NA,
  races  = NA,
  max_age_at_diagnosis  = NA,
  max_height = NA,
  max_weight  = NA,
  min_age_at_diagnosis  = NA,
  min_height  = NA,
  min_weight = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      "name" = samples,
      "patient" = patients,
      "ethnicity" = ethnicities,
      "gender" = genders,
      "race" = races,
      "maxAgeAtDiagnosis" = max_age_at_diagnosis,
      "maxHeight" = max_height,
      "maxWeight" = max_weight,
      "minAgeAtDiagnosis" = min_age_at_diagnosis,
      "minHeight" = min_height,
      "minWeight" = min_weight
    ),
    query_file = "samples.txt",
    default_tbl = dplyr::tibble("sample" = character()),
    select_cols = c("sample" = "name"),
    ...
  )
}

#' Query Sample Patients
#'
#' @param samples A vector of strings
#' @param patients A list of strings
#' @param ethnicities A vector of strings
#' @param genders A vector of strings
#' @param races A vector of strings
#' @param max_age_at_diagnosis An integer
#' @param max_height A float
#' @param max_weight A float
#' @param min_age_at_diagnosis An integer
#' @param min_height A float
#' @param min_weight A float
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_sample_patients <- function(
  samples = NA,
  patients = NA,
  ethnicities  = NA,
  genders  = NA,
  races  = NA,
  max_age_at_diagnosis  = NA,
  max_height = NA,
  max_weight  = NA,
  min_age_at_diagnosis  = NA,
  min_height  = NA,
  min_weight = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      "name" = samples,
      "patient" = patients,
      "ethnicity" = ethnicities,
      "gender" = genders,
      "race" = races,
      "maxAgeAtDiagnosis" = max_age_at_diagnosis,
      "maxHeight" = max_height,
      "maxWeight" = max_weight,
      "minAgeAtDiagnosis" = min_age_at_diagnosis,
      "minHeight" = min_height,
      "minWeight" = min_weight
    ),
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

# samples by mutation status --------------------------------------------------

#' query_samples_by_mutation_status
#'
#' @param samples A vector of strings
#' @param ... Arguments to create_result_from_api_query
#' @param datasets A vector of strings
#' @param ethnicities A vector of strings
#' @param features A vector of strings
#' @param feature_classes A vector of strings
#' @param genders A vector of strings
#' @param max_age_at_diagnosis An integer
#' @param max_height A double
#' @param max_weight A double
#' @param min_age_at_diagnosis An integer
#' @param min_height A double
#' @param min_weight A double
#' @param mutation_ids A vector of integers
#' @param mutation_statuses A vector of strings
#' @param patients A vector of strings
#' @param races A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#'
#' @export
#' @importFrom magrittr %>%
query_samples_by_mutation_status <- function(
  datasets = NA,
  ethnicities  = NA,
  features = NA,
  feature_classes = NA,
  genders  = NA,
  max_age_at_diagnosis  = NA,
  max_height = NA,
  max_weight  = NA,
  min_age_at_diagnosis  = NA,
  min_height  = NA,
  min_weight = NA,
  mutation_ids = NA,
  mutation_statuses = NA,
  patients = NA,
  races  = NA,
  parent_tags = NA,
  samples = NA,
  tags = NA,
  ...
){
  tbl <- create_result_from_api_query(
    query_args =  list(
      "dataSet" = datasets,
      "ethnicity" = ethnicities,
      "feature" = features,
      "featureClass" = feature_classes,
      "gender" = genders,
      "maxAgeAtDiagnosis" = max_age_at_diagnosis,
      "maxHeight" = max_height,
      "maxWeight" = max_weight,
      "minAgeAtDiagnosis" = min_age_at_diagnosis,
      "minHeight" = min_height,
      "minWeight" = min_weight,
      "mutationId" = mutation_ids,
      "mutationStatus" = mutation_statuses,
      "patient" = patients,
      "race" = races,
      "related" = parent_tags,
      "sample" = samples,
      "tag" = tags
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
