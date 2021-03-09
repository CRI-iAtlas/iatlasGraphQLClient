
test_that("query_heritability_results",{
  expected_columns <- c(
    "dataset_name",
    "dataset_display",
    "feature_name",
    "feature_display",
    "feature_germline_module",
    "feature_germline_category" ,
    "cluster",
    "p_value",
    "fdr",
    "se",
    "variance"
  )
  result1 <- query_heritability_results(
    datasets = "TCGA",
    features = "leukocyte_fraction",
    query_dir = query_dir
  )
  result2 <- query_heritability_results(
    datasets = "none",
    features = "leukocyte_fraction",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})
