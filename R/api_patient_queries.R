
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
#' @param paging A named list
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
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
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
      "minWeight" = min_weight,
      "paging" = paging,
      "distinct" = F
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
#' @param paging A named list
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
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
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
      "minWeight" = min_weight,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "patient_slides.txt",
    default_tbl = dplyr::tibble(
      "slide_name" = character(),
      "slide_description" = character(),
      "patient_age_at_diagnosis" = numeric(),
      "patient_name" = character(),
      "patient_ethnicity" = character(),
      "patient_gender" = character(),
      "patient_height" = numeric(),
      "patient_race" = character(),
      "patient_weight" = numeric()
    ),
    select_cols = c(
      "slides",
      "patient_age_at_diagnosis" = "ageAtDiagnosis",
      "patient_name" = "barcode",
      "patient_ethnicity" = "ethnicity",
      "patient_gender" = "gender",
      "patient_height" = "height",
      "patient_race" = "race",
      "patient_weight" = "weight"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "slides", keep_empty = T) %>%
      dplyr::select(
        "slide_name" = "name",
        "slide_description" = "description",
        "patient_age_at_diagnosis",
        "patient_name",
        "patient_ethnicity",
        "patient_gender",
        "patient_height",
        "patient_race",
        "patient_weight"
      )
  }
}
