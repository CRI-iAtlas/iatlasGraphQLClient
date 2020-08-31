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
    api_url = "http://ec2-54-190-27-240.us-west-2.compute.amazonaws.com/api"
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
    return(result)
}

#' Create Result From API Query
#'
#' @param query_args A named list
#' @param query_file A string, that is a path to a text file
#' @param default_tbl A tibble
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
    select_cols,
    arrange_cols = NULL,
    ...
){
    tbl <-
        perform_api_query(query_args, query_file, ...) %>%
        purrr::pluck(1) %>%
        dplyr::as_tibble()
    if (nrow(tbl) == 0) {
        tbl <- default_tbl
    } else {
        tbl <- tbl %>%
            dplyr::select(select_cols) %>%
            dplyr::arrange(!!!rlang::syms(arrange_cols))
    }
    return(tbl)
}

#' Separate Combined Column
#'
#' @param tbl A tibble
#' @param column A string that is the name of a column
#'
#' @export
#' @importFrom magrittr %>%
separate_combined_column <- function(tbl, column){
    combined_columns <- tbl %>%
        dplyr::pull(column) %>%
        colnames

    new_tbl <-
        purrr::map(combined_columns, ~get_nested_column(tbl, column, .x)) %>%
        dplyr::bind_cols(dplyr::select(tbl, -column)) %>%
        dplyr::as_tibble()
}

#' Get Nested Column
#'
#' @param tbl A tibble
#' @param column A string that is the name of a column
#' @param subcolumn A string that is the name of a subcolumn in the column
#'
#' @export
#' @importFrom magrittr %>%
get_nested_column <- function(tbl, column, subcolumn){
    tbl %>%
        dplyr::pull(column) %>%
        dplyr::select(subcolumn)
}
