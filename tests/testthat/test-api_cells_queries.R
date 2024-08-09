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
