test_that("query_gene_types", {
  expected_columns <- c("display", "name")

  result1 <- query_gene_types(query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)
  
  result2 <- query_gene_types(
    gene_types = result1$name[1], query_dir = query_dir
  )
  expect_named(result2, expected_columns)
  expect_true(nrow(result2) == 1)
  
  result3 <- query_gene_types(gene_types = "not_a_type", query_dir = query_dir)
  expect_named(result3, expected_columns)
  expect_equal(nrow(result3), 0)
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
