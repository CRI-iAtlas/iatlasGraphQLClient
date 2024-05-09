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