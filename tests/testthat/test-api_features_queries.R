test_that("query_features", {
  expected_columns <- c(
    "name",
    "display",
    "class",
    "order",
    "unit",
    "method_tag"
  )

  result1 <- query_features(
    features = "Lymphocytes_Aggregate1",
    query_dir = query_dir
  )
  result2 <- query_features(
    features = "not_a_feature",
    query_dir = query_dir
  )

  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)

  expect_equal(nrow(result1), 1)
  expect_equal(nrow(result2), 0)
})

test_that("query_feature_values", {
  expected_columns <- c(
    "sample",
    "feature_name",
    "feature_display",
    "feature_value",
    "feature_order",
    "feature_class"
  )

  result1 <- query_feature_values(
    features = "Lymphocytes_Aggregate1",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_feature_values(
    cohorts = "PCAWG_Immune_Subtype",
    features = "not_a_feature",
    query_dir = query_dir
  )
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})

test_that("query_pseudobulk_feature_values", {
  expected_columns <- c(
    "feature_name",
    "feature_display",
    "feature_order",
    "feature_class",
    "cell_name",
    "cell_type",
    "value"
  )
  
  result1 <- query_pseudobulk_feature_values(
    features = "Th1_cells",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)
  
  result2 <- query_pseudobulk_feature_values(
    features = "not_a_feature",
    query_dir = query_dir
  )
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})

test_that("query_features_range", {
  expected_columns <- c("name", "display", "value_min", "value_max")
  result1 <- query_features_range(
    cohorts = "TCGA_Immune_Subtype",
    features = "Lymphocytes_Aggregate1",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_true(result1$value_min <= result1$value_max)

  result2 <- query_features_range(
    cohorts = "TCGA_Immune_Subtype",
    features = "not_a_feature",
    query_dir = query_dir
  )
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})
