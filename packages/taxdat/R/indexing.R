#' @description Take an index of a dataframe, and reindex it so it goes from 1 to the number of rows
#' @param df The data frame
#' @param index_column A column with unique values for each index
#' @param new_index_column A column to put the new index in
#' @return A copy of df with an additional column `new_index_column` with the new index
#' @export
reindex <- function(df, index_column, new_index_column = index_column) {
  index_column <- rlang::sym(index_column)
  new_index_column <- rlang::sym(new_index_column)
  df %>%
    dplyr::group_by(!!index_column) %>%
    dplyr::mutate(`:=`(!!new_index_column, dplyr::cur_group_id())) %>%
    dplyr::ungroup() %>%
    return()
  # df %>% dplyr::mutate(`:=`(!!new_index_column, dplyr::group_indices(.,
  # !!index_column))) %>% return
}


#' @description Cast an bit64::integer64 to a normal R integer
#' @param x The integer64 to convert
#' @return The normal R integer
cast_to_int32 <- function(x) {
  if (!is.integer(x)) {
    rc <- as.integer(x)
    if (all(rc == x)) {
      return(rc)
    }
    stop(paste("Conversion failed", x[rc != x], "converted to", rc[rc != x]))
  }
  return(x)
}
