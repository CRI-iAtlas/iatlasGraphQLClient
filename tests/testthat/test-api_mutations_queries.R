
test_that("query_mutations", {
  expected_columns <-  c(
    "mutation_id",
    "gene_entrez",
    "gene_hgnc",
    "mutation_code",
    "mutation_type_name",
    "mutation_type_display",
    "mutation_name"
  )
  result1 <- query_mutations(ids = 1, query_dir = query_dir)
  result2 <- query_mutations(entrez = -1, query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_mutation_statuses", {
  expected_columns <-  c(
    "mutation_id",
    "mutation_name",
    "gene_entrez",
    "gene_hgnc",
    "mutation_code",
    "mutation_type_name",
    "mutation_type_display",
    "sample_name",
    "mutation_status"
  )
  result1 <- query_mutation_statuses(ids = 1, query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_mutation_statuses(entrez = -1, query_dir = query_dir)
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})
