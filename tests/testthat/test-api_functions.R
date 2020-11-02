
test_that("add_ghql_query_from_text_file", {
    ghql_query_object <- ghql::Query$new()
    expect_length(ghql_query_object$queries, 0)
    add_ghql_query_from_text_file(
        "test_query1",
        stringr::str_c(query_dir,  "/pagination_test.txt"),
        ghql_query_object
    )
    expect_length(ghql_query_object$queries, 1)
    expect_named(ghql_query_object$queries, "test_query1")
    add_ghql_query_from_text_file(
        "test_query2",
        stringr::str_c(query_dir,  "/pagination_test.txt"),
        ghql_query_object
    )
    expect_length(ghql_query_object$queries, 2)
    expect_named(ghql_query_object$queries, c("test_query1", "test_query2"))
})

test_that("perform_api_query", {
  result1 <- perform_api_query(list("name" = NA), "gene_types.txt", query_dir)
  expect_named(result1, "geneTypes")
  expect_named(result1$geneTypes, c("display", "name"))

  result3 <- perform_api_query(
    list(
      "paging" = list("first" = 10),
      "feature" = "frac_altered",
      "maxPValue" = 0.1e-170,
      "distinct" = T
    ),
    "pagination_test.txt",
    query_dir
  )
  expect_named(result3, c("copyNumberResults"))
  expect_named(result3$copyNumberResults, c("error", "items", "paging"))
  expect_null(result3$copyNumberResults$error)
  expect_named(
    result3$copyNumberResults$paging,
    c(
      "endCursor",
      "hasNextPage",
      "hasPreviousPage",
      "limit",
      "page",
      "pages",
      "startCursor",
      "total",
      "type"
    )
  )
  expect_named(result3$copyNumberResults$items, "pValue")
  expect_true(length(result3$copyNumberResults$items) > 0)

  result4 <- perform_api_query(
    list(
      "paging" = list("first" = 10),
      "feature" = NA,
      "maxPValue" = NA,
      "distinct" = F
    ),
    "pagination_test.txt",
    query_dir
  )
  expect_named(result4, c("copyNumberResults"))
  expect_named(result4$copyNumberResults, c("error", "items", "paging"))
  expect_null(result4$copyNumberResults$error)
  expect_named(
    result4$copyNumberResults$paging,
    c(
      "endCursor",
      "hasNextPage",
      "hasPreviousPage",
      "limit",
      "page",
      "pages",
      "startCursor",
      "total",
      "type"
    )
  )
  expect_false(is.null(result4$copyNumberResults$paging$hasNextPage))
  expect_false(is.null(result4$copyNumberResults$paging$endCursor))

  expect_named(result4$copyNumberResults$items, "pValue")
  expect_equal(length(result4$copyNumberResults$items$pValue), 10)

  result5 <- perform_api_query(
    list(
      "paging" = list(
        "first" = 1,
        "before" = result4$copyNumberResults$paging$endCursor
      ),
      "feature" = NA,
      "maxPValue" = NA,
      "distinct" = F
    ),
    "pagination_test.txt",
    query_dir
  )

  expect_named(result5$copyNumberResults$items, "pValue")
  expect_equal(length(result5$copyNumberResults$items$pValue), 1)
})


test_that("create_result_from_paginated_api_query", {

  result1 <- create_result_from_paginated_api_query(
    query_args = list(
      paging = list("first" = 100000),
      feature = "frac_altered",
      maxPValue = 0.1e-170,
      distinct = F
    ),
    query_file = "pagination_test.txt",
    default_tbl = dplyr::tibble("pValue" = double()),
    query_dir = query_dir
  )
  expect_named(result1, c("pValue"))
  expect_true(nrow(result1) > 0)
  expect_true(nrow(result1) < 100000)

  result2 <- create_result_from_paginated_api_query(
    query_args = list(
      paging = NA,
      feature = "not_a_feature",
      maxPValue = NA,
      distinct = F
    ),
    query_file = "pagination_test.txt",
    default_tbl = dplyr::tibble("pValue" = double()),
    query_dir = query_dir
  )
  expect_named(result2, c("pValue"))
  expect_true(nrow(result2) == 0)

  result3 <- create_result_from_paginated_api_query(
    query_args = list(
      paging = list(
        "first" = 10
      ),
      feature = "frac_altered",
      maxPValue = 0.1e-170,
      distinct = F
    ),
    query_file = "pagination_test.txt",
    default_tbl = dplyr::tibble(
      "pValue" = double()
    ),
    query_dir = query_dir
  )
  expect_named(result3, c("pValue"))
  expect_true(nrow(result3) > 10)
})

