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
#' @param paging A named list
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
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
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
      "minWeight" = min_weight,
      "paging" = paging,
      "distinct" = F
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
#' @param paging A named list
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
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
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
      "minWeight" = min_weight,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "sample_patients.txt",
    default_tbl = dplyr::tibble(
      "sample_name" = character(),
      "patient_name" = character(),
      "patient_age_at_diagnosis" = numeric(),
      "patient_ethnicity" = character(),
      "patient_gender" = character(),
      "patient_height" = numeric(),
      "patient_race" = character(),
      "patient_weight" = numeric()
    ),
    select_cols = c(
      "sample_name" = "name",
      "patient_name" = "patient.barcode",
      "patient_age_at_diagnosis" = "patient.ageAtDiagnosis",
      "patient_ethnicity" = "patient.ethnicity",
      "patient_gender" = "patient.gender",
      "patient_height" = "patient.height",
      "patient_race" = "patient.race",
      "patient_weight" = "patient.weight"
    ),
    ...
  )
}
