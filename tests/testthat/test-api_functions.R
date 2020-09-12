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

})

test_that("perform_api_query", {
    result1 <- perform_api_query(list("name" = NA), "gene_types.txt", query_dir)
    expect_named(result1, "geneTypes")
    expect_named(result1$geneTypes, c("display", "name"))
    result2 <- perform_api_query(list("dataSet" = NA), "datasets.txt", query_dir)
    expect_named(result2, "dataSets")
    expect_named(result2$dataSets, c("display", "name"))
    result3 <- perform_api_query(
      list(entrez = NA, mutationCode = NA, mutationType = NA),
      "mutations.txt",
      query_dir,
      flatten_json = T
    )
    expect_named(
      result3$mutations, c("id", "mutationCode", "gene.entrez", "gene.hgnc")
    )
})
