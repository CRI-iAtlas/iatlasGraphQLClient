test_that("query_gene_nodes",{
  result1 <- query_nodes(
    datasets = "TCGA",
    entrez =  2,
    network = "Extracellular Network",
    query_dir = query_dir
  )
  expect_true(nrow(result1) > 0)

})

test_that("query_feature_nodes",{
  result1 <- query_nodes(
    datasets = "TCGA",
    features =  "B_cells_Aggregate2",
    network = "Extracellular Network",
    query_dir = query_dir
  )
  expect_true(nrow(result1) > 0)
})

