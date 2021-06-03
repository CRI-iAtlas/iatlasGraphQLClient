query_dir  <- system.file("queries", package = "iatlas.api.client")

# datasets ------------------------------------------------------------------

test_that("query_datasets", {
  expected_names <- c("display", "name", "type")
  result1 <- query_datasets(query_dir = query_dir)
  expect_named(result1, expected_names)
  expect_true(nrow(result1) > 0)
  result2 <- query_datasets("non_dataset", query_dir = query_dir)
  expect_named(result2, expected_names)
  expect_equal(nrow(result2), 0)
})

test_that("query_dataset_samples", {
  result1 <- query_dataset_samples("PCAWG", query_dir = query_dir)
  expect_named(result1, c("name"))
  expect_true(nrow(result1) > 0)
  result2 <- query_dataset_samples("non_dataset", query_dir = query_dir)
  expect_named(result2, c("name"))
  expect_equal(nrow(result2), 0)
})

# gene types ------------------------------------------------------------------

test_that("query_gene_types", {
  expected_columns <- c("display", "name")

  result1 <- query_gene_types(query_dir = query_dir)
  result2 <- query_gene_types("not_a_type", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_genes_by_gene_types", {
  expected_columns <- c("entrez", "hgnc", "gene_type_name", "gene_type_display")

  result1 <- query_genes_by_gene_types(query_dir = query_dir)
  result2 <- query_genes_by_gene_types("not_a_type", query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})
