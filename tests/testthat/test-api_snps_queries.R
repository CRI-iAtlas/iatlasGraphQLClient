test_that("query_snps_nodes",{
  expected_columns <- c(
    "name",
    "rsid",
    "chr",
    "bp"
  )
  result1 <- query_snps(
    name = "7:104003135:C:G",
    rsid = "rs2188491",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_snps(
    chr = "7",
    min_bp = 150000000L
  )
  expect_named(result2, expected_columns)
  expect_true(nrow(result2) > 0)
  expect_true(all(result2$bp > 150000000L))

  result3 <- query_snps(
    chr = "7",
    max_bp = 4400000L
  )
  expect_named(result3, expected_columns)
  expect_true(nrow(result3) > 0)
  expect_true(all(result3$bp < 4400000L))

  result4 <- query_snps(
    chr = "7",
    max_bp = 6000000L,
    min_bp = 5900000L
  )
  expect_named(result4, expected_columns)
  expect_true(nrow(result4) > 0)
  expect_true(all(result4$bp > 5900000L))
  expect_true(all(result4$bp < 6000000L))

  result5 <- query_snps(chr = "none")
  expect_named(result5, expected_columns)
  expect_equal(nrow(result5), 0)
})
