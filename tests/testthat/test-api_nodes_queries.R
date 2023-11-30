test_that("query_gene_nodes",{
  result1 <- query_nodes(
    datasets = "TCGA",
    tag1 = "C1",
    entrez =  2,
    network = "Extracellular Network",
    n_tags = 2,
    query_dir = query_dir
  )
  result2 <- query_nodes(
    datasets = "none",
    tag1 = "C1",
    entrez =  2,
    network = "Extracellular Network",
    n_tags = 2,
    query_dir = query_dir
  )
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_feature_nodes",{
  result1 <- query_nodes(
    datasets = "TCGA",
    tag1 = "C1",
    features =  "B_cells_Aggregate2",
    network = "Extracellular Network",
    n_tags = 2,
    query_dir = query_dir
  )
  expect_true(nrow(result1) > 0)

  result2 <- query_nodes(
    datasets = "TCGA",
    tag1 = "C1",
    features = c(
      "B_cells_Aggregate2",
      "Dendritic_cells_Aggregate2"
    ),
    network = "Extracellular Network",
    min_score = 5,
    n_tags = 2,
    query_dir = query_dir
  )
  expect_equal(nrow(result2), 0)

})

