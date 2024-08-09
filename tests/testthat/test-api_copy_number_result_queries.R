test_that("query_copy_number_result_genes",{
  expected_columns <- c("entrez", "hgnc")
  result1 <- query_copy_number_result_genes(
    datasets = "TCGA",
    tags = "C1",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_copy_number_result_genes(
    datasets = "none",
    tags = "C1",
    entrez = 1,
    query_dir = query_dir
  )

  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})


