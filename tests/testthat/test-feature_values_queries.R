test_that("query_feature_values", {
  expected_columns <- c(
    "value",
    "sample_name",
    "feature_name",
    "feature_display",
    "feature_order",
    "feature_class"
  )

  result1 <- query_feature_values(
    cohorts = "PCAWG_Immune_Subtype",
    features = "Lymphocytes_Aggregate1",
    query_dir = query_dir
  )
  result2 <- query_feature_values(
    cohorts = "PCAWG_Immune_Subtype",
    features = "not_a_feature",
    query_dir = query_dir
  )

  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)

  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})
