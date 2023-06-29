
test_that("query_germline_gwas_results",{
  expected_columns <- c(
    "dataset_name",
    "dataset_display",
    "feature_name",
    "feature_display",
    "feature_germline_module",
    "feature_germline_category",
    "snp_name",
    "snp_rsid",
    "snp_chr",
    "snp_bp",
    "p_value",
    "maf"
  )
  result1 <- query_germline_gwas_results(
    datasets = "TCGA",
    feature = "Module3_IFN_score",
    snps = "3:133016759:C:G",
    query_dir = query_dir
  )

  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)


  result2 <- query_germline_gwas_results(
    datasets = "TCGA",
    min_p_value = 1.0e-07,
    max_p_value = 9.9e-07,
    query_dir = query_dir
  )
  expect_named(result2, expected_columns)
  expect_true(nrow(result2) > 0)
  expect_true(all(result2$p_value >= 1.0e-07))
  expect_true(all(result2$p_value <= 9.9e-07))

  result3 <- query_germline_gwas_results(
    datasets = "TCGA",
    snps = "none",
    query_dir = query_dir
  )
  expect_named(result3, expected_columns)
  expect_equal(nrow(result3), 0)
})
