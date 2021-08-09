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
    node1 = "PCAWG_cellimage_network_BLCA-US_940",
    node2 = "PCAWG_cellimage_network_BLCA-US_T_cells_CD8_Aggregate2",
    query_dir = query_dir
  )
  result2 <- query_edges(
    node1 = "tcga_ecn_1",
    node2 = "not_a_node",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_equal(nrow(result1), 1)
  expect_equal(nrow(result2), 0)
})
