test_that("query_cells", {
  expected_columns <- c(
    "type",
    "name"
  )

  result1 <- query_cells(cell = "RU1065C_239381811150236")
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) == 1)

  result2 <- query_cells(cell = "not_a_cell")
  expect_named(result2, expected_columns)
  expect_true(nrow(result2) == 0)

})

test_that("query_single_cell_seq", {
  expected_columns <- c(
    "cell_type",
    "cell_name",
    "gene_entrez",
    "gene_hgnc",
    "gene_single_cell_seq"
  )

  result1 <- query_single_cell_seq(
    cell = "TGAATACCCAGAGCGTAGG-5", entrez = 54890L
  )
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) == 1)

  result2 <- query_single_cell_seq(cell = "not_a_cell", entrez = 0L)
  expect_named(result2, expected_columns)
  expect_true(nrow(result2) == 0)

})

test_that("query_single_cell_feature", {
  expected_columns <- c(
    "cell_type",
    "cell_name",
    "feature_name",
    "feature_display",
    "feature_value"
  )

  result1 <- query_single_cell_feature(
    cell = "RU1311A_T_1_165945547864806", feature = "umap_1"
  )
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) == 1)

  result2 <- query_single_cell_feature(
    cell = "not_a_cell", feature = "not_a_feature"
  )
  expect_named(result2, expected_columns)
  expect_true(nrow(result2) == 0)

})