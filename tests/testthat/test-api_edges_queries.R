test_that("query_edges",{
  expected_columns <- c(
    "label",
    "name",
    "score",
    "node1",
    "node2"
  )
  result1 <- query_edges(
    node1 = "PCAWG_extracellular_network_C2_8754",
    node2 = "PCAWG_extracellular_network_C2_3655",
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
