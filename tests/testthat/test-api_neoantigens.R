
test_that("query_neoantigens",{
  expected_columns <- c(
    "tpm",
    "pmhc",
    "freq_pmhc",
    "patient",
    "gene_entrez",
    "gene_hgnc"
  )
  result1 <- query_neoantigens(
    pmhcs = list("RLMELQEAV HLA_A*02:01 SPAG9"),
    patients = list("VanAllen_antiCTLA4_2015-p126"),
    query_dir = query_dir
  )
  result2 <- query_neoantigens(
    pmhcs = list("xxx"),
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})
