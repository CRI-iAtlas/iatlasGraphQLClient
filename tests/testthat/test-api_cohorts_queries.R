test_that("query_cohorts", {
  expected_columns1 <- c(
    "name",
    "dataset_name",
    "dataset_display",
    "tag_name",
    "tag_long_display",
    "tag_short_display"
  )

  expected_columns2 <- c(
    "name",
    "dataset_name",
    "dataset_display"
  )

  result1 <- query_cohorts(
    cohort = "TCGA_Immune_Subtype",
    query_dir = query_dir
  )

  expect_named(result1, expected_columns1)
  expect_equal(nrow(result1), 1)


  result2 <- query_cohorts(
    cohort = "TCGA",
    query_dir = query_dir
  )

  expect_named(result2, expected_columns2)
  expect_equal(nrow(result2), 1)

  result3 <- query_cohorts(
    cohort = "not_a_cohort",
    query_dir = query_dir
  )

  expect_named(result3, c('name'))
  expect_equal(nrow(result3), 0)
})

test_that("query_cohort_features", {
  expected_columns <- c(
    "cohort_name",
    "feature_name",
    "feature_display"
  )

  result1 <- query_cohort_features(
    cohorts = "TCGA_Immune_Subtype",
    query_dir = query_dir
  )

  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 1)

  result3 <- query_cohort_features(
    cohort = "TCGA",
    query_dir = query_dir
  )

  expect_named(result3, expected_columns)
  expect_true(nrow(result3) > 1)

  result4 <- query_cohort_features(
    cohort = "not_a_cohort",
    query_dir = query_dir
  )

  expect_named(result4, expected_columns)
  expect_equal(nrow(result4), 0)
})


test_that("query_cohort_samples", {
  expected_columns1 <- c(
    "cohort_name",
    "sample_name",
    "dataset_name",
    "dataset_display",
    'tag_characteristics',
    'tag_color',
    "tag_long_display",
    "tag_name",
    "tag_order",
    "tag_short_display"
  )

  expected_columns2 <- c(
    "cohort_name",
    "sample_name",
    "dataset_name",
    "dataset_display",
    "tag_name"
  )

  result1 <- query_cohort_samples(
    cohorts = "TCGA_Immune_Subtype",
    query_dir = query_dir
  )

  expect_named(result1, expected_columns1)
  expect_true(nrow(result1) > 1)

  result3 <- query_cohort_samples(
    cohort = "TCGA",
    query_dir = query_dir
  )

  expect_named(result3, expected_columns2)
  expect_true(nrow(result3) > 1)

  result4 <- query_cohort_samples(
    cohort = "not_a_cohort",
    query_dir = query_dir
  )

  expect_named(result4, expected_columns2)
  expect_equal(nrow(result4), 0)
})
