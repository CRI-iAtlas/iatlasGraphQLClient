test_that("query_tags", {
  expected_columns <- c(
    "name",
    "long_display",
    "short_display",
    "characteristics",
    "color",
    "parent_tags",
    "publications"
  )

  result1 <- query_tags(tags = "ACC_", query_dir = query_dir)
  result2 <- query_tags(tags = "not_a_tag", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_equal(nrow(result1), 1)
  expect_equal(nrow(result2), 0)

  expect_named(
    result1$parent_tags[[1]],
    c(
      "name",
      "long_display",
      "short_display",
      "characteristics",
      "color"
    )
  )
  expect_named(
    result1$publications[[1]],
    c(
      "name",
      "title",
      "do_id",
      "pubmed_id",
      "journal",
      "first_author_last_name",
      "year"
    )
  )

  result3 <- query_tags(datasets = "TCGA", parent_tags = "Immune_Subtype")
  expect_named(result3, expected_columns)
  expect_equal(nrow(result3), 6)
  expect_equal(result3$publications[[1]], list())

  result4 <- query_tags(tags = "parent_group")
  expect_named(result4, expected_columns)
  expect_equal(nrow(result4), 1)
  expect_equal(result4$parent_tags[[1]], list())
  expect_equal(result4$publications[[1]], list())
})

test_that("query_tag_samples", {
  expected_columns <- c(
    "name",
    "long_display",
    "short_display",
    "characteristics",
    "color",
    "size",
    "samples"
  )

  result1 <- query_tag_samples("PCAWG", "Immune_Subtype", query_dir = query_dir)
  result2 <- query_tag_samples("PCAWG", "not_a_tag", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_tag_samples2", {
  expected_columns1 <- c(
    "sample",
    "Immune_Subtype",
    "PCAWG_Study"
  )
  expected_columns2 <- c("sample")
  result1 <- query_tag_samples2(
    datasets = "PCAWG",
    parent_tags = c("Immune_Subtype", "PCAWG_Study"),
    query_dir = query_dir
  )
  expect_named(result1, expected_columns1)
  expect_true(nrow(result1) > 0)
  result2 <- query_tag_samples2(
    datasets = "not_a_dataset",
    parent_tags = c("Immune_Subtype", "PCAWG_Study"),
    query_dir = query_dir
  )
  expect_named(result2, expected_columns2)
  expect_true(nrow(result2) == 0)
})



