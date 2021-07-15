
test_that("query_mutations", {
  expected_columns <-  c(
    "mutation_name",
    "mutation_code",
    "mutation_type_name",
    "mutation_type_display",
    "gene_entrez",
    "gene_hgnc"
  )
  result1 <- query_mutations(mutations = 'ABL1:(NS)', query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_mutations(mutations = 'not_a_mutation', query_dir = query_dir)
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})

test_that("query_mutation_statuses", {
  expected_columns <-  c(
    "mutation_name",
    "mutation_code",
    "mutation_type_name",
    "mutation_type_display",
    "gene_entrez",
    "gene_hgnc",
    "sample_name",
    "mutation_status"
  )
  result1 <- query_mutation_statuses(mutations = 'ABL1:(NS)', query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_mutation_statuses(mutations = 'not_a_mutation', query_dir = query_dir)
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})
