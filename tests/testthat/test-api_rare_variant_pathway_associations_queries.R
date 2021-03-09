
test_that("query_rare_variant_pathway_associations",{
  expected_columns <- c(
    "dataset_name",
    "dataset_display",
    "feature_name",
    "feature_display",
    "feature_germline_module",
    "feature_germline_category",
    "pathway",
    "p_value",
    "min",
    "max",
    "mean",
    "q1",
    "q2",
    "q3",
    "n_mutants" ,
    "n_total"
  )
  result1 <- query_rare_variant_pathway_associations(
    datasets = "TCGA",
    query_dir = query_dir
  )

  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_rare_variant_pathway_associations(
    datasets = "TCGA",
    features = "none",
    query_dir = query_dir
  )
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})
