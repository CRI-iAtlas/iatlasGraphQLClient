test_that("query_genes", {
  expected_columns <- c(
    "hgnc",
    "entrez",
    "description",
    "friendly_name",
    "io_landscape_name",
    "gene_family",
    "gene_function",
    "immune_checkpoint",
    "pathway",
    "super_category"
  )
  result1 <- query_genes(entrez = 1, query_dir = query_dir)
  result2 <- query_genes(entrez = -1, query_dir = query_dir)
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_equal(nrow(result1), 1)
  expect_equal(nrow(result2), 0)
})

test_that("query_immunomodulators", {
  expected_columns <-  c(
    "entrez",
    "hgnc",
    "friendly_name",
    "description",
    "gene_family",
    "gene_function",
    "immune_checkpoint",
    "super_category",
    "publications"
  )
  result1 <- query_immunomodulators(query_dir = query_dir)
  result2 <- query_immunomodulators(entrez = -1, query_dir = query_dir)

  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)

})

test_that("query_io_targets", {
  result1 <- query_io_targets(query_dir = query_dir)
  expect_named(
    result1,
    c(
      "entrez",
      "hgnc",
      "description",
      "io_landscape_name" ,
      "pathway",
      "therapy_type"
    )
  )
})

test_that("query_gene_expression", {
  expected_columns <- c(
    "sample",
    "entrez",
    "hgnc",
    "rna_seq_expr"
  )
  result1 <- query_gene_expression(
    entrez = c(135L, 136L),
    samples = c("TCGA-XF-A9T8", "TCGA-2G-AAFN"),
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_equal(nrow(result1), 4L)

  result2 <- query_gene_expression(
    "entrez" = 0L, samples = "TCGA-XF-A9T8", query_dir = query_dir
  )
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0L)
})

test_that("query_gene_nanostring_expression", {
    expected_columns <- c(
        "sample",
        "entrez",
        "hgnc",
        "nanostring_expr"
    )
    result1 <- query_gene_nanostring_expression(
        entrez = 4282L,
        samples = "Chen_CanDisc_2016-c25-ar-c25_pre",
        query_dir = query_dir
    )
    expect_named(result1, expected_columns)
    expect_equal(nrow(result1), 1L)

    result2 <- query_gene_nanostring_expression(
        "entrez" = 0L, samples = "TCGA-XF-A9T8", query_dir = query_dir
    )
    expect_named(result2, expected_columns)
    expect_equal(nrow(result2), 0L)
})

test_that("query_pseudobulk_expression", {
  expected_columns <- c(
    'gene_entrez', 'gene_hgnc', 'cell_name', 'cell_type', 'single_cell_seq_sum'
  )
  result1 <- query_pseudobulk_expression(
    entrez = c(135L),
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 1)
  
  result2 <- query_pseudobulk_expression(
    "entrez" = 0L, query_dir = query_dir
  )
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0L)
})
