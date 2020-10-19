query_dir  <- system.file("queries", package = "iatlas.api.client")

test_that("query_edges",{
  expected_columns <- c(
    "label",
    "name",
    "score",
    "node1",
    "node2"
  )
  result1 <- query_edges(
    node1 = "tcga_ecn_1",
    node2 = c("tcga_ecn_2553", "tcga_ecn_2571"),
    query_dir = query_dir
  )
  result2 <- query_edges(
    node1 = "tcga_ecn_1",
    node2 = "not_a_node",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})
