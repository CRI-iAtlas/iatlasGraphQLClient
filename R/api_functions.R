#' Add ghql Query From Text File
#'
#' @param query_name A string, that will be the name of the query
#' @param text_file_path A string that is the path to the query file
#' @param query_object A ghql::Query$new object
#' @export
#' @importFrom magrittr %>%
add_ghql_query_from_text_file <- function(
  query_name,
  text_file_path,
  query_object = .GlobalEnv$ghql_query_object
){
  query_text <- text_file_path %>%
    readLines() %>%
    stringr::str_c(collapse = "\n")
  query_object$query(query_name, query_text)
}


#' Perform API Query
#'
#' @param variables A named list
#' @param query_file A string, that is a path to a text file
#' @param query_dir A string, that is to the directory of the query file
#' @param api_url A string, that is the URL of the API server
#'
#' @export
#' @importFrom magrittr %>%
perform_api_query <- function(
  variables,
  query_file,
  query_dir = system.file("queries", package = "iatlas.api.client"),
  api_url = "http://ec2-34-215-55-89.us-west-2.compute.amazonaws.com/api"
){
  ghql_con <- ghql::GraphqlClient$new(api_url)
  ghql_query_obj <- ghql::Query$new()
  query_path <- file.path(query_dir, query_file)
  add_ghql_query_from_text_file(
    "query",
    query_path,
    ghql_query_obj
  )
  query <- ghql_query_obj$queries$query
  result <-
    ghql_con$exec(query, variables) %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("data")
}

#' Format Query Result
#'
#' @param tbl A Tibble
#' @param unnest_cols A vector of strings passed to tidyr::unnest
#' @param select_cols A vector of strings passed to dplyr::select
#' @param arrange_cols A vector fo strings passed to dplyr::arrange
#'
#' @export
#' @importFrom magrittr %>%
format_query_result <- function(
  tbl,
  unnest_cols = NULL,
  select_cols = NULL,
  arrange_cols = NULL
){
  if(!is.null(unnest_cols)) {
    tbl <- tidyr::unnest(tbl, unnest_cols, keep_empty = T)
  }
  tbl <- tbl %>%
    jsonlite::flatten(.) %>%
    dplyr::as_tibble()

  if(!is.null(select_cols)) {
    tbl <- dplyr::select(tbl, select_cols)
  }
  if(!is.null(arrange_cols)) {
    tbl <- dplyr::arrange(tbl, !!!rlang::syms(arrange_cols))
  }
  return(tbl)
}

#' Create Result From API Query
#'
#' @param query_args A named list
#' @param query_file A string, that is a path to a text file
#' @param default_tbl A tibble
#' @param unnest_cols A vector of strings passed to tidyr::unnest
#' @param select_cols A vector of strings passed to dplyr::select
#' @param arrange_cols A vector fo strings passed to dplyr::arrange
#' @param ... Arguments passed to perform_api_query
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
create_result_from_api_query <- function(
  query_args,
  query_file,
  default_tbl,
  unnest_cols = NULL,
  select_cols = NULL,
  arrange_cols = NULL,
  ...
){
  tbl <-
    perform_api_query(query_args, query_file, ...) %>%
    purrr::pluck(1)
  if (is.null(tbl)) {
    return(default_tbl)
  }
  format_query_result(tbl, unnest_cols, select_cols, arrange_cols)
}

#' Add Pages To Query Arguments
#'
#' @param query_args A named list
#' @param n_pages An integer
#'
#' @export
add_pages_to_query_args <- function(query_args, n_pages){
  query_args$page <- NULL
  pages <- 2:n_pages
  query_args_list <- purrr::map(pages, ~c(query_args, list("page" = .x)))
}

#' Create Result From Paginated API Query
#'
#' @param query_args A named list
#' @param query_file A string, that is a path to a text file
#' @param default_tbl A tibble
#' @param unnest_cols A vector of strings passed to tidyr::unnest
#' @param select_cols A vector of strings passed to dplyr::select
#' @param arrange_cols A vector fo strings passed to dplyr::arrange
#' @param ... Arguments passed to perform_api_query
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
create_result_from_paginated_api_query <- function(
  query_args,
  query_file,
  default_tbl,
  unnest_cols = NULL,
  select_cols = NULL,
  arrange_cols = NULL,
  ...
){
  result <-
    perform_api_query(query_args, query_file, ...) %>%
    purrr::pluck(1)
  tbl <- purrr::pluck(result, "items")
  if (is.null(tbl)) {
    return(default_tbl)
  }
  if(is.null(result$pages)){
    stop("Query result has no pages attribute")
  }
  if(result$pages > 1){
    query_args_list <- add_pages_to_query_args(query_args, result$pages)
    tbl <-
      purrr::map(query_args_list, perform_api_query, query_file, ...) %>%
      purrr::map(purrr::pluck, 1, "items") %>%
      dplyr::bind_rows(tbl, .)
  }
  format_query_result(tbl, unnest_cols, select_cols, arrange_cols)
}

#' Create Result From Paginated API Query2
#'
#' @param query_args A named list
#' @param query_file A string, that is a path to a text file
#' @param default_tbl A tibble
#' @param unnest_cols A vector of strings passed to tidyr::unnest
#' @param select_cols A vector of strings passed to dplyr::select
#' @param arrange_cols A vector fo strings passed to dplyr::arrange
#' @param ... Arguments passed to perform_api_query
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang !!!
create_result_from_paginated_api_query2 <- function(
  query_args,
  query_file,
  default_tbl,
  unnest_cols = NULL,
  select_cols = NULL,
  arrange_cols = NULL,
  ...
){
  result <-
    perform_api_query(query_args, query_file, ...) %>%
    purrr::pluck(1)

  items <- result$items
  paging <- result$paging


  empty_result <- any(
    is.null(items),
    is.null(paging),
    length(items) == 0,
    nrow(items) == 0
  )
  if (empty_result) {
    return(default_tbl)
  }
  if(is.null(paging$hasNextPage)){
    return(format_query_result(items, unnest_cols, select_cols, arrange_cols))
  }
  if(paging$hasNextPage){
    query_args$paging <- list("before" = paging$endCursor)
    results1 <- format_query_result(
      items,
      unnest_cols,
      select_cols,
      arrange_cols
    )
    results2 <- create_result_from_paginated_api_query2(
      query_args,
      query_file,
      default_tbl,
      unnest_cols,
      select_cols,
      arrange_cols,
    )
    return(dplyr::bind_rows(results1, results2))
  } else {
    return(format_query_result(items, unnest_cols, select_cols, arrange_cols))
  }
}
