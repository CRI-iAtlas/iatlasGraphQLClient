
test_that("query_mutations", {
  expected_columns <-  c(
    "id",
    "entrez",
    "hgnc",
    "code",
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
