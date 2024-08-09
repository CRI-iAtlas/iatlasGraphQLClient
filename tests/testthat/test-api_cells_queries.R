test_that("query_cells", {
  expected_columns <- c(
    "type",
    "name"
  )

  result1 <- query_cells(cell = "RU1065C_239381811150236")
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) == 1)

  result2 <- query_cells(cell = "not_a_cell")
  expect_named(result2, expected_columns)
  expect_true(nrow(result2) == 0)

})

test_that("query_cell_feature_values", {
  expected_columns <- c(
    "cell_name",
    "cell_type",
    "feature_name",
    "feature_value"
  )
  
  result1 <- query_cell_feature_values(
    cells = c('RU1311A_T_1_165945547864806'),
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)
  
  result2 <- query_cell_feature_values(
    cells = c('not_a_cell'),
    query_dir = query_dir
  )
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})

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