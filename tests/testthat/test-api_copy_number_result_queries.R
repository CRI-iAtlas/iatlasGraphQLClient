
test_that("query_copy_number_results",{
  expected_columns <- c(
    "direction",
    "mean_normal",
    "mean_cnv",
    "p_value",
    "log10_p_value",
    "t_stat",
    "dataset_name",
    "dataset_display",
    "feature_name",
    "feature_display",
    "tag_name",
    "tag_long_display",
    "tag_short_display",
    "entrez",
    "hgnc"
  )
  result1 <- query_copy_number_results(
    datasets = "TCGA",
    tags = "C1",
    max_p_value = 0.000000000000000000000000000000000000000000000001,
    entrez =  1,
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_copy_number_results(
    datasets = "none", tags = "C1",  entrez = c(1), query_dir = query_dir
  )
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})


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


