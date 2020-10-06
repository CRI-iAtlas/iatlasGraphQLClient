query_dir  <- system.file("queries", package = "iatlas.api.client")

test_that("query_copy_number_result_genes",{
  expected_columns <- c("entrez", "hgnc")
  result1 <- query_copy_number_result_genes(
    datasets = "TCGA", tags = "C1",  entrez = c(1), query_dir = query_dir
  )
  result2 <- query_copy_number_result_genes(
    datasets = "none", tags = "C1",  entrez = c(1), query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

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
    datasets = "TCGA", tags = "C1",  entrez = c(1), query_dir = query_dir
  )
  result2 <- query_copy_number_results(
    datasets = "none", tags = "C1",  entrez = c(1), query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_copy_number_results2",{
  expected_columns <- c(
    "direction",
    "mean_normal",
    "mean_cnv",
    "p_value",
    "log10_p_value",
    "t_stat",
    "tag_name",
    "tag_long_display",
    "tag_short_display",
    "hgnc"
  )
  result1 <- query_copy_number_results2(
    datasets = "TCGA", tags = "C1", features = "leukocyte_fraction", query_dir = query_dir
  )
  result2 <- query_copy_number_results2(
    datasets = "none", tags = "C1", features = "leukocyte_fraction", query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})
