test_that("query_samples_by_tag", {
  expected_names <- c(
    "tag_name",
    "tag_long_display",
    "tag_short_display",
    "tag_characteristics",
    "tag_color",
    "sample"
  )
  result1 <- query_samples_by_tag(
    datasets = "PCAWG", parent_tags = "Immune_Subtype", query_dir = query_dir
  )
  result2 <- query_samples_by_tag(
    datasets = "PCAWG", parent_tags = "not_a_tag", query_dir = query_dir
  )
  expect_named(result1, expected_names)
  expect_named(result2, expected_names)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_samples_by_tag2", {
  expected_names <- c("sample")
  result1 <- query_samples_by_tag2(
    datasets = "PCAWG", tags = "C1", query_dir = query_dir
  )
  result2 <- query_samples_by_tag2(
    datasets = "PCAWG", tags = "not_a_tag", query_dir = query_dir
  )
  expect_named(result1, expected_names)
  expect_named(result2, expected_names)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})
