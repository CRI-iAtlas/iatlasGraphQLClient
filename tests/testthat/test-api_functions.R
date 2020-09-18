query_dir  <- system.file("queries", package = "iatlas.api.client")
test_query <- stringr::str_c(query_dir,  "/cohort_selection.txt")

test_that("add_ghql_query_from_text_file", {
    ghql_query_object <- ghql::Query$new()
    expect_length(ghql_query_object$queries, 0)
    add_ghql_query_from_text_file(
        "test_query1",
        test_query,
        ghql_query_object
    )
    expect_length(ghql_query_object$queries, 1)
    expect_named(ghql_query_object$queries, "test_query1")
    add_ghql_query_from_text_file(
        "test_query2",
        test_query,
        ghql_query_object
    )
    expect_length(ghql_query_object$queries, 2)
    expect_named(ghql_query_object$queries, c("test_query1", "test_query2"))
})

test_that("perform_api_query", {
    result1 <- perform_api_query(list("name" = NA), "gene_types.txt", query_dir)
    expect_named(result1, "geneTypes")
    expect_named(result1$geneTypes, c("display", "name"))
    result2 <- perform_api_query(list("dataSet" = NA), "datasets.txt", query_dir)
    expect_named(result2, c("dataSets"))
    expect_named(result2$dataSets, c("display", "name"))
    result4 <- perform_api_query(
      list(
        dataSet = "TCGA",
        tag = "C1",
        feature = NA,
        entrez = c(1,2),
        direction = NA,
        minPValue = NA,
        maxPValue = NA,
        minLog10PValue = NA,
        maxLog10PValue = NA,
        minMeanNormal = NA,
        minMeanCnv = NA,
        minTStat = NA,
        page = NA
      ),
      "copy_number_result_genes.txt",
      query_dir,
    )
    expect_named(
      result4$copyNumberResults,
      c("items", "page", "pages", "total")
    )
    expect_named(result4$copyNumberResults$items, "gene")
    expect_named(result4$copyNumberResults$items$gene, c("entrez", "hgnc"))
})

test_that("create_result_from_api_query", {
  result1 <- create_result_from_api_query(
    query_args = list("dataSet" = NA),
    query_file = "datasets.txt",
    default_tbl = dplyr::tibble(
      "display" = character(), "name" = character()
    ),
    select_cols = c("display", "Name" = "name"),
    arrange_cols = c("display", "Name"),
    query_dir = query_dir
  )
  expect_named(result1, c("display", "Name"))

  result2 <- create_result_from_api_query(
    query_args = list("dataSet" = "not_a_dataset"),
    query_file = "datasets.txt",
    default_tbl = dplyr::tibble(
      "display" = character(), "name" = character()
    ),
    select_cols = c("display", "name"),
    arrange_cols = "name",
    query_dir = query_dir
  )
  expect_named(result2, c("display", "name"))

  result3 <- create_result_from_api_query(
    query_args = list(
      entrez = NA, mutationId = NA, mutationCode = NA, mutationType = NA
    ),
    query_file = "mutations.txt",
    default_tbl = dplyr::tibble(
      "id" = character(),
      "code" =  character(),
      "entrez" = integer(),
      "hgnc" = character()
    ),
    query_dir = query_dir
  )
  expect_named(result3, c("id", "mutationCode", "gene.entrez", "gene.hgnc"))

  result4 <- create_result_from_api_query(
    query_args = list(
      entrez = NA, mutationId = NA, mutationCode = NA, mutationType = NA
    ),
    query_file = "mutations.txt",
    default_tbl = dplyr::tibble(
      "id" = character(),
      "code" =  character(),
      "entrez" = integer(),
      "hgnc" = character()
    ),
    select_cols = c(
      "id",
      "code" = "mutationCode",
      "entrez" = "gene.entrez",
      "hgnc" = "gene.hgnc"
    ),
    query_dir = query_dir
  )
  expect_named(result4, c("id", "code", "entrez", "hgnc"))
})

test_that("add_pages_to_query_args",{
  args1 <- list("dataset" = "TCGA", "page" = NA)
  result1 <- add_pages_to_query_args(args1, 2)
  expect_equal(result1, list(list("dataset" = "TCGA", "page" = 2)))
  result2 <- add_pages_to_query_args(args1, 3)
  expect_equal(
    result2,
    list(
      list("dataset" = "TCGA", "page" = 2),
      list("dataset" = "TCGA", "page" = 3)
    )
  )
})

# TODO: uncomment when queries faster
test_that("create_result_from_paginated_api_query", {
  result1 <- create_result_from_paginated_api_query(
    query_args = list(
      dataSet = "TCGA",
      tag = "C1",
      feature = NA,
      entrez = c(1,2),
      direction = NA,
      minPValue = NA,
      maxPValue = NA,
      minLog10PValue = NA,
      maxLog10PValue = NA,
      minMeanNormal = NA,
      minMeanCnv = NA,
      minTStat = NA,
      page = NA
    ),
    query_file = "copy_number_result_genes.txt",
    query_dir = query_dir
  )
  expect_named(result1, c("gene.entrez", "gene.hgnc"))
  expect_true(nrow(result1) > 0)

  result2 <- create_result_from_paginated_api_query(
    query_args = list(
      feature = "not_a_feature",
      page = NA
    ),
    query_file = "cnr_test.txt",
    default_tbl = dplyr::tibble(
      "p_value" = double()
    ),
    query_dir = query_dir
  )
  expect_named(result2, c("p_value"))
  expect_equal(nrow(result2), 0)
  #
  # result3 <- create_result_from_paginated_api_query(
  #   query_args = list(
  #     feature = "frac_altered",
  #     page = NA
  #   ),
  #   query_file = "cnr_test.txt",
  #   default_tbl = dplyr::tibble(
  #     "p_value" = double()
  #   ),
  #   query_dir = query_dir
  # )
  # expect_named(result3, c("pValue"))
  # expect_true(nrow(result3) > 100000)
})
