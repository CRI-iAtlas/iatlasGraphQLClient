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

  result1 <- query_tags(tags = "ACC.", query_dir = query_dir)
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

test_that("query_tag_samples2", {
  expected_columns <- c(
    "name",
    "long_display",
    "short_display",
    "characteristics",
    "color",
    "size",
    "samples"
  )

  result1 <- query_tag_samples2("PCAWG", "Immune_Subtype", query_dir = query_dir)
  result2 <- query_tag_samples2("PCAWG", "not_a_tag", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})
