
# TODO: fix tests
test_that("query_driver_results",{
  expected_columns <- c(
    "dataset_name",
    "dataset_display",
    "feature_name",
    "feature_display",
    "tag_name",
    "tag_long_display",
    "tag_short_display",
    "entrez",
    "hgnc",
    "mutation_code",
    "p_value",
    "fold_change",
    "log10_p_value",
    "log10_fold_change",
    "num_mutant",
    "num_wild_types"
  )
  result1 <- query_driver_results(
    datasets = "TCGA",
    tags = "C1",
    features = "leukocyte_fraction",
    query_dir = query_dir
  )
  result2 <- query_driver_results(
    datasets = "none",
    tags = "C1",
    features = "leukocyte_fraction",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})
