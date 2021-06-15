test_that("query_cohorts", {
  expected_columns1 <- c(
    "name",
    "dataset_name",
    "dataset_display",
    "tag_name",
    "tag_long_display",
    "tag_short_display",
    "clinical"
  )

  expected_columns2 <- c(
    "name",
    "dataset_name",
    "dataset_display",
    "clinical"
  )

  result1 <- query_cohorts(
    cohort = "TCGA_Immune_Subtype",
    query_dir = query_dir
  )

  expect_named(result1, expected_columns1)
  expect_equal(nrow(result1), 1)

  result2 <- query_cohorts(
    cohort = "TCGA_Gender",
    query_dir = query_dir
  )

  expect_named(result2, expected_columns2)
  expect_equal(nrow(result2), 1)

  result3 <- query_cohorts(
    cohort = "TCGA",
    query_dir = query_dir
  )

  expect_named(result3, expected_columns2)
  expect_equal(nrow(result3), 1)

  result4 <- query_cohorts(
    cohort = "not_a_cohort",
    query_dir = query_dir
  )

  expect_named(result4, c('name', 'clinical'))
  expect_equal(nrow(result4), 0)
})

test_that("query_full_cohort", {
  expected_names <- c(
    "clinical",
    "dataset",
    "features",
    "genes",
    "name",
    "samples",
    "tag"
  )

  expected_dataset_names <- c("display", "name")
  expected_feature_names <- c("display", "name")
  expected_gene_names <- c("entrez", "hgnc")
  expected_sample_names1  <- c(
    "clinical_value",
    "name",
    "tag_long_display",
    "tag_name",
    "tag_short_display"
  )
  expected_sample_names2  <- c("clinical_value", "name", "tag")
  expected_tag_names <- c("long_display", "name", "short_display")

  result1 <- query_full_cohort(
    cohort = "PCAWG_Immune_Subtype",
    query_dir = query_dir
  )
  expect_named(result1, expected_names)
  expect_named(result1$dataset, expected_dataset_names)
  expect_named(result1$features, expected_feature_names)
  expect_named(result1$genes, expected_gene_names)
  expect_named(result1$samples, expected_sample_names1)
  expect_named(result1$tag, expected_tag_names)


  result2 <- query_full_cohort(
    cohort = "PCAWG_Gender",
    query_dir = query_dir
  )
  expect_named(result2, expected_names)
  expect_named(result2$dataset, expected_dataset_names)
  expect_named(result2$features, expected_feature_names)
  expect_named(result2$genes, expected_gene_names)
  expect_named(result2$samples, expected_sample_names2)
  expect_true(is.na(result2$tag))

})

