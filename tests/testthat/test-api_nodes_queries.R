query_dir  <- system.file("queries", package = "iatlas.api.client")

test_that("query_gene_nodes",{
  expected_columns <- c(
    "label",
    "name",
    "score",
    "tags",
    "x",
    "y",
    "entrez",
    "hgnc",
    "gene_friendly_name"
  )
  result1 <- query_gene_nodes(
    datasets = "TCGA",
    tags = "C1",
    entrez =  2,
    network = "Extracellular Network",
    query_dir = query_dir
  )
  result2 <- query_gene_nodes(
    datasets = "none",
    tags = "C1",
    entrez =  2,
    network = "Extracellular Network",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_named(result2, expected_columns)
  expect_true(nrow(result1) > 0)
  expect_equal(nrow(result2), 0)
})

test_that("query_feature_nodes",{
  expected_columns <- c(
    "label",
    "name",
    "score",
    "tags",
    "x",
    "y",
    "feature_name",
    "feature_display"
  )
  result1 <- query_feature_nodes(
    datasets = "TCGA",
    tags = "C1",
    features =  "B_cells_Aggregate2",
    network = "Extracellular Network",
    query_dir = query_dir
  )
  expect_named(result1, expected_columns)
  expect_true(nrow(result1) > 0)

  result2 <- query_feature_nodes(
    datasets = "none",
    tags = "C1",
    features =  "B_cells_Aggregate2",
    network = "Extracellular Network",
    query_dir = query_dir
  )
  expect_named(result2, expected_columns)
  expect_equal(nrow(result2), 0)

})
