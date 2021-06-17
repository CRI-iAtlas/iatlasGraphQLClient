unnest_df_column <- function(tbl, column){
  df_column_tbl <- tbl[[column]] %>%
    dplyr::as_tibble() %>%
    purrr::set_names(
      colnames(.) %>%
        purrr::map(snakecase::to_snake_case) %>%
        stringr::str_c(column, ., sep = "_")
    )
  new_tbl <- tbl %>%
    dplyr::select(-dplyr::all_of(column)) %>%
    dplyr::bind_cols(df_column_tbl)
}
