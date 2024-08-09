#' Query Feature Values
#'
#' @param cohorts A vector of strings
#' @param cells A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#' @export
#' @importFrom magrittr %>%
query_cell_feature_values <- function(
    cohorts = NA,
    cells = NA,
    paging = NA,
    ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "cohort" = cohorts,
      "cell" = cells,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "cell_feature_values.txt",
    default_tbl = dplyr::tibble(
      "cell_name" = character(),
      "cell_type" = character(),
      "feature_name" = character(),
      "feature_value" = double(),
    ),
    select_cols = c(
      "cell_name" = "name",
      "cell_type" = "type",
      "features"
    ),
    ...
  )
  if(nrow(tbl) == 0) return(tbl)
  else {
    tbl %>%
      tidyr::unnest(cols = "features", keep_empty = T) %>%
      dplyr::select(
        "cell_name",
        "cell_type",
        "feature_name" = "name",
        "feature_value" = "value"
      )
  }
}