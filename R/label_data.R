#' Join raw data and plate layout tibbles into a single, labeled tibble
#'
#' label_data() joins two tibbles into one: raw data, and the experimental information provided by the plate layout. The single tibble which results, which contains both raw data and experimental conditions associated with each well, allows handling and visualiation of data based on experimental variables.
#'
#' @param data A tibble containg raw data to be joined with the layout tibble, and at least one column also present in the layout tibble, typically "well".
#' @param layout A tibble, containing at least one column also present in the data tibble, typically "well", which will be used to join with the data tibble. Additional columns in the layout tibble contain information associated with each well, typically experimental variables, which will also be joined with the data. Typically, the layout tibble is created by reading in plate layout file using the function read_plate_layout()
#' @param join_by A column shared between the data and layout, identical in name and related in content. Typically "well".
#' @param drop_empties TRUE or FALSE value, determining whether data from particular wells should be dropped from the output tibble. Defaults to TRUE.
#' @param .drop_from If drop_empties is set to TRUE, the unquoted name of the column which will be used to perform value-based filtering.
#' @param .drop_matches A vector containing all values which, if present in the .drop_from column, will trigger the removal of those wells from the output tibble.
#'
#' @return A tibble, containing all columns present in the input data and layout tibbles, joined into a single, labeled tibble.
#' @export
label_data <- function(data,
                       layout,
                       join_by = "well",
                       drop_empties = TRUE,
                       .drop_from = protein,
                       .drop_matches = c("Empty", "empty")) {
  out <- data %>%
    dplyr::left_join( . , layout, by = join_by) %>%
    dplyr::filter()

  if (drop_empties == TRUE) {
    try( out <- out %>% dplyr::filter(! {{.drop_from}} %in%.drop_matches))
  }

  out

}

utils::globalVariables(c("protein"))
