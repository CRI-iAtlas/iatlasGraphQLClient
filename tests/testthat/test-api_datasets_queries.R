test_that("query_datasets", {
  expected_names <- c("display", "name", "type")

  result1 <- query_datasets(datasets = "PCAWG", query_dir = query_dir)
  expect_named(result1, expected_names)
  expect_true(nrow(result1) > 0)

  result2 <- query_datasets(datasets = "non_dataset", query_dir = query_dir)
  expect_named(result2, expected_names)
  expect_equal(nrow(result2), 0)
})

test_that("query_dataset_samples", {
  expected_names <- c(
    "sample_name", "dataset_display", "dataset_name", "dataset_type"
  )

  result1 <- query_dataset_samples(datasets = "PCAWG", query_dir = query_dir)
  expect_named(result1, expected_names)
  expect_true(nrow(result1) > 0)

  result2 <- query_dataset_samples(datasets = "non_dataset", query_dir = query_dir)
  expect_named(result2, expected_names)
  expect_equal(nrow(result2), 0)
})

test_that("query_dataset_tags", {
  expected_names <- c(
    "tag_name",
    "tag_long_display",
    "tag_short_display",
    "tag_color",
    "tag_characteristics",
    "dataset_display",
    "dataset_name",
    "dataset_type"
  )

  result1 <- query_dataset_tags(datasets = "PCAWG", query_dir = query_dir)
  expect_named(result1, expected_names)
  expect_true(nrow(result1) > 0)

  result2 <- query_dataset_tags(datasets = "non_dataset", query_dir = query_dir)
  expect_named(result2, expected_names)
  expect_equal(nrow(result2), 0)
})
