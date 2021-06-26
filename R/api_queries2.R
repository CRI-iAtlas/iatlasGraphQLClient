
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
