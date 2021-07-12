#' query_slides
#'
#' @param slides A vector of strings
#' @param paging A named list
#' @param ... Arguments to create_result_from_api_query
#'
#' @export
#' @importFrom magrittr %>%
query_slides <- function(
  slides = NA,
  paging = NA,
  ...
){
  tbl <- create_result_from_cursor_paginated_api_query(
    query_args =  list(
      "name" = slides,
      "paging" = paging,
      "distinct" = F
    ),
    query_file = "slides.txt",
    default_tbl = dplyr::tibble(
      "name" = character(),
      "description" = character()
    ),
    select_cols = c("name", "description"),
    ...
  )
}
