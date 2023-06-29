
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

test_that("perform_api_query2", {
  result1 <- perform_api_query(list("name" = NA), "gene_types.txt", query_dir)
  expect_named(result1, "geneTypes")
  expect_named(result1$geneTypes, c("display", "name"))

  .GlobalEnv$API_URL <- test_api_url

  result2 <- perform_api_query(list("name" = NA), "gene_types.txt", query_dir)
  expect_named(result2, "geneTypes")
  expect_named(result2$geneTypes, c("display", "name"))

  .GlobalEnv$API_URL <- NULL

  result3 <- perform_api_query(list("name" = NA), "gene_types.txt", query_dir)
  expect_named(result3, "geneTypes")
  expect_named(result3$geneTypes, c("display", "name"))
})


test_that("create_result_from_cursor_paginated_api_query", {
  default_tbl = dplyr::tibble( "pValue" = double())

  result1 <- create_result_from_cursor_paginated_api_query(
    query_args = list(
      paging = list("first" = 100000),
      feature = "frac_altered",
      maxPValue = 0.1e-170,
      distinct = F
    ),
    query_file = "pagination_test.txt",
    default_tbl = default_tbl,
    query_dir = query_dir
  )
  expect_named(result1, c("pValue"))
  expect_true(nrow(result1) > 0)
  expect_true(nrow(result1) < 100000)

  result2 <- create_result_from_cursor_paginated_api_query(
    query_args = list(
      paging = NA,
      feature = "not_a_feature",
      maxPValue = NA,
      distinct = F
    ),
    query_file = "pagination_test.txt",
    default_tbl = default_tbl,
    query_dir = query_dir
  )
  expect_named(result2, c("pValue"))
  expect_true(nrow(result2) == 0)

  result3 <- create_result_from_cursor_paginated_api_query(
    query_args = list(
      paging = list("first" = 10),
      feature = "frac_altered",
      maxPValue = 0.1e-170,
      distinct = F
    ),
    query_file = "pagination_test.txt",
    default_tbl = default_tbl,
    query_dir = query_dir
  )
  expect_named(result3, c("pValue"))
  expect_true(nrow(result3) > 10)
})

test_that("create_result_from_offset_paginated_api_query", {
  default_tbl = dplyr::tibble( "pValue" = double())

  result1 <- create_result_from_offset_paginated_api_query(
    query_args = list(
      paging = list("limit" = 100000),
      feature = "frac_altered",
      maxPValue = 0.1e-170,
      distinct = T
    ),
    query_file = "pagination_test.txt",
    default_tbl = default_tbl,
    query_dir = query_dir
  )
  expect_named(result1, c("pValue"))
  expect_true(nrow(result1) > 0)
  expect_true(nrow(result1) < 100000)

  result2 <- create_result_from_offset_paginated_api_query(
    query_args = list(
      paging = NA,
      feature = "not_a_feature",
      maxPValue = NA,
      distinct = T
    ),
    query_file = "pagination_test.txt",
    default_tbl = default_tbl,
    query_dir = query_dir
  )
  expect_named(result2, c("pValue"))
  expect_true(nrow(result2) == 0)

  result3 <- create_result_from_offset_paginated_api_query(
    query_args = list(
      paging = list("limit" = 10),
      feature = "frac_altered",
      maxPValue = 0.1e-170,
      distinct = T
    ),
    query_file = "pagination_test.txt",
    default_tbl = default_tbl,
    query_dir = query_dir
  )
  expect_named(result3, c("pValue"))
  expect_true(nrow(result3) > 1)
})

test_that("do_cursor_paginated_api_query", {

  result1 <- do_cursor_paginated_api_query(
    query_args = list(
      paging = list("first" = 10),
      feature = "frac_altered",
      maxPValue = 0.1e-170,
      distinct = F
    ),
    query_file = "pagination_test.txt",
    query_dir = query_dir
  )
  expect_type(result1, "list")
  expect_true(length(result1) > 1)

  result2 <- do_cursor_paginated_api_query(
    query_args = list(
      paging = list("first" = 10),
      feature = "not_a_feature",
      maxPValue = 0.1e-170,
      distinct = F
    ),
    query_file = "pagination_test.txt",
    query_dir = query_dir
  )
  expect_type(result2, "list")
  expect_true(length(result2) == 0)
})

test_that("do_offset_paginated_api_query", {

  result2 <- do_offset_paginated_api_query(
    query_args = list(
      paging = list("first" = 10),
      feature = "not_a_feature",
      maxPValue = 0.1e-170,
      distinct = F
    ),
    query_file = "pagination_test.txt",
    query_dir = query_dir
  )
  expect_type(result2, "list")
  expect_true(length(result2) == 0)
})

