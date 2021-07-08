test_that("query_tags", {
  expected_columns <- c(
    "name",
    "long_display",
    "short_display",
    "characteristics",
    "color"
  )

  result1 <- query_tags(tags = "ACC_")
  expect_named(result1, expected_columns)
  expect_equal(nrow(result1), 1)

  result2 <- query_tags(tags = "not_a_tag")
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)

  result3 <- query_tags(parent_tags = "Immune_Subtype")
  expect_named(result3, expected_columns)
  expect_equal(nrow(result3), 6)

  result4 <- query_tags(tags = "parent_group")
  expect_named(result4, expected_columns)
  expect_equal(nrow(result4), 1)

  result5 <- query_tags(parent_tags = "gender")
  expect_named(result5, expected_columns)
  expect_equal(nrow(result5), 2)
})

test_that("query_tag_samples", {
  expected_columns <- c(
    "sample_name",
    "tag_name",
    "tag_long_display",
    "tag_short_display",
    "tag_characteristics",
    "tag_color"
  )

  result1 <- query_tag_samples(cohorts = "TCGA_Immune_Subtype", tag = "C1")
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_tag_samples(cohorts = "TCGA_Immune_Subtype", tag = "not_a_tag")
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})

test_that("query_tag_sample_count", {
  expected_columns <- c(
    "name",
    "long_display",
    "short_display",
    "characteristics",
    "color",
    "sample_count"
  )

  result1 <- query_tag_sample_count(cohorts = "TCGA_Immune_Subtype", tag = "C1")
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_tag_sample_count(cohorts = "TCGA_Immune_Subtype", tag = "not_a_tag")
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})

test_that("query_tag_publications", {
  expected_columns <- c(
    "publication_do_id",
    "publication_first_author_last_name",
    "publication_journal",
    "publication_name",
    "publication_pubmed_id",
    "publication_title",
    "tag_name",
    "tag_long_display",
    "tag_short_display",
    "tag_characteristics",
    "tag_color"
  )

  result1 <- query_tag_publications(tag = "ACC_")
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_tag_publications(tag = "not_a_tag")
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})

test_that("query_tags_with_parent_tags", {
  expected_columns <- c(
    "parent_tag_name",
    "parent_tag_long_display",
    "parent_tag_short_display",
    "parent_tag_characteristics",
    "parent_tag_color",
    "tag_name",
    "tag_long_display",
    "tag_short_display",
    "tag_characteristics",
    "tag_color"
  )

  result1 <- query_tags_with_parent_tags(tag = "C1")
  expect_named(result1, expected_columns)
  expect_equal(nrow(result1), 2)

  result2 <- query_tags_with_parent_tags(tag = "not_a_tag")
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})
