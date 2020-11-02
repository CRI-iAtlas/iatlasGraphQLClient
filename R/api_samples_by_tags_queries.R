#' Query Samples By Tag
#'
#' @param datasets A vector of strings
#' @param parent_tags A vector of strings
#' @param tags A vector of strings
#' @param features A vector of strings
#' @param feature_classes A vector of strings
#' @param samples A vector of strings
#' @param patients A vector of strings
#' @param max_age_at_diagnosis An integer
#' @param max_height A float
#' @param max_weight A float
#' @param min_age_at_diagnosis An integer
#' @param min_height A float
#' @param min_weight A float
#' @param ethnicities A vector of strings
#' @param genders A vector of strings
#' @param races A vector of strings
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_samples_by_tag <- function(
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
  samples = NA,
  patients = NA,
  races  = NA,
  parent_tags = NA,
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
      "name" = samples,
      "patient" = patients,
      "race" = races,
      "related" = parent_tags,
      "tag" = tags
    ),
    query_file = "samples_by_tag.txt",
    default_tbl = dplyr::tibble(
      "tag_name" = character(),
      "tag_long_display" = character(),
      "tag_short_display" = character(),
      "tag_characteristics" = character(),
      "tag_color" = character(),
      "sample" = character()
    ),
    select_cols = c(
      "tag_name" = "tag",
      "tag_long_display" = "longDisplay",
      "tag_short_display" = "shortDisplay",
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
      dplyr::rename("sample" = "name")
  }
}

#' Query Samples By Tag 2
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
query_samples_by_tag2 <- function(
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
    query_file = "samples_by_tag2.txt",
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
