test_that("query_cell_stats", {
  expected_columns <- c(
    "type",
    "count",
    "avg_expr",
    "perc_expr",
    "dataset_name",
    "gene_entrez"
  )
  
  result1 <- query_cell_stats(entrez = 3001L)
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 1)
  
  result2 <- query_cell_stats(entrez = 0L)
  expect_named(result2, expected_columns)
  expect_true(nrow(result2) == 0)
  
})