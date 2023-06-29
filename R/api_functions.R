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
  query_dir = system.file("queries", package = "iatlasGraphQLClient"),
  api_url = "https://api.cri-iatlas.org/api"
){
  if(!is.null(.GlobalEnv$API_URL)){
    api_url <- .GlobalEnv$API_URL
  }
  api_url <- "https://api-staging.cri-iatlas.org/api"
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
    tbl <- tidyr::unnest(tbl, dplyr::all_of(unnest_cols), keep_empty = T)
  }
  tbl <- tbl %>%
    jsonlite::flatten(.) %>%
    dplyr::as_tibble()

  if(!is.null(select_cols)) {
    tbl <- dplyr::select(tbl, dplyr::any_of(select_cols))
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


#' Create Result From Cursor Paginated API Query
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
create_result_from_cursor_paginated_api_query <- function(
  query_args,
  query_file,
  default_tbl,
  unnest_cols = NULL,
  select_cols = NULL,
  arrange_cols = NULL,
  ...
){
  items_list <- do_cursor_paginated_api_query(query_args, query_file, ...)
  if(length(items_list) == 0) return(default_tbl)
  results <- items_list %>%
    rev() %>%
    purrr::map(
      format_query_result,
      unnest_cols,
      select_cols,
      arrange_cols
    ) %>%
    dplyr::bind_rows()
}

#' Create Result From Offset Paginated API Query
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
create_result_from_offset_paginated_api_query <- function(
  query_args,
  query_file,
  default_tbl,
  unnest_cols = NULL,
  select_cols = NULL,
  arrange_cols = NULL,
  ...
){
  items_list <- do_offset_paginated_api_query(query_args, query_file, ...)
  if(length(items_list) == 0) return(default_tbl)
  results <- items_list %>%
    rev() %>%
    purrr::map(
      format_query_result,
      unnest_cols,
      select_cols,
      arrange_cols
    ) %>%
    dplyr::bind_rows()
}

do_cursor_paginated_api_query <- function(
  query_args,
  query_file,
  result_n = 1,
  ...
){
  result <-
    perform_api_query(query_args, query_file, ...) %>%
    purrr::pluck(1)

  items <- result$items
  paging <- result$paging
  i <- as.character(result_n)

  empty_result <- any(
    is.null(items),
    is.null(paging),
    length(items) == 0,
    nrow(items) == 0
  )
  if (empty_result) {
    return(list())
  }
  if(!is.null(paging$hasNextPage) && paging$hasNextPage){
    if(length(query_args$paging) == 1 && is.na(query_args$paging)){
      new_paging <- list("after" = paging$endCursor)
    } else {
      new_paging <- query_args$paging
      new_paging$after <- paging$endCursor
    }
    query_args$paging <- new_paging
    items_list <- do_cursor_paginated_api_query(
      query_args,
      query_file,
      result_n = result_n + 1,
      ...
    )
  } else {
    items_list <- list()
  }
  items_list[[i]] <- items
  return(items_list)
}

do_offset_paginated_api_query <- function(
  query_args,
  query_file,
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
    return(list())
  }

  if(paging$page < paging$pages){
    if(length(query_args$paging) == 1 && is.na(query_args$paging)){
      new_paging <- list("page" = paging$page + 1)
    } else {
      new_paging <- query_args$paging
      new_paging$page <- paging$page + 1
    }
    query_args$paging <- new_paging
    items_list <- do_offset_paginated_api_query(
      query_args,
      query_file,
      ...
    )
  } else {
    items_list <- list()
  }
  items_list[[paging$page]] <- items
  return(items_list)
}






