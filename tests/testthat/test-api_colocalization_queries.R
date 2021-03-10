
test_that("query_colocalizations",{
  expected_columns <- c(
    "dataset_name",
    "dataset_display",
    "coloc_dataset_name",
    "coloc_dataset_display",
    "feature_name",
    "feature_display",
    "feature_germline_module",
    "feature_germline_category",
    "snp_name",
    "snp_rsid",
    "snp_chr",
    "snp_bp",
    "gene_entrez",
    "gene_hgnc",
    "qtl_type",
    "ecaviar_pp",
    "tissue",
    "plot_type",
    "splice_loc",
    "plot_link"
  )
  result1 <- query_colocalizations(
    datasets = "TCGA",
    query_dir = query_dir
  )

  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_colocalizations(
    datasets = "TCGA",
    snps = "none",
    query_dir = query_dir
  )
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)
})
