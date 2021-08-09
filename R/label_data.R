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
#' @param .make_numeric A vector containing the names of any columns from the layout which, if not already numeric, should be coerced to numeric. Numeric-type columns are automatically detected using readr::parse_guess() in read_plate_layout, so this argument is necessary only if a user wants to override the decision made by readr::parse_guess().
#'@param .make_character A vector containing the names of any columns from the layout which, if not already of type character, should be coerced to character. Character-type columns are automatically detected using readr::parse_guess() in read_plate_layout, so this argument is necessary only if a user wants to override the decision made by readr::parse_guess().
#'
#' @return A tibble, containing all columns present in the input data and layout tibbles, joined into a single, labeled tibble.
#'
#' @importFrom dplyr left_join filter mutate across all_of
#' @importFrom glue glue
#' @importFrom utils "globalVariables"
#'
#' @export
label_data <- function(data,
                       layout,
                       join_by = "well",
                       drop_empties = TRUE,
                       .drop_from = protein,
                       .drop_matches = c("Empty", "empty"),
                       .make_numeric  = c(),
                       .make_character = c()) {
  # join data and layout
  out <-
    data %>%
    left_join( . , layout, by = join_by)

  # filter out .drop_matches
  if (drop_empties == TRUE) { try( out <-
                                     out %>%
                                     filter(! {{ .drop_from }} %in% .drop_matches) %>% # when empties NOT dropped from layout
                                     filter(! is.na( {{ .drop_from }})) # when empties WERE dropped from layout
  ) }

  # alert users of invalid .make_character and .make_numeric selections
  if ("value" %in% .make_character)       { cant_change_that("value", ".make_character", "numeric")
    .make_character <- .make_character[!.make_character %in% c("Temperature", "value", "column")]}

  if ("Temperature" %in% .make_character) { cant_change_that("Temperature", ".make_character", "numeric")
    .make_character <- .make_character[!.make_character %in% c("Temperature", "value", "column")]}

  if ("column" %in% .make_character)      { cant_change_that("column", ".make_character", "numeric")
    .make_character <- .make_character[!.make_character %in% c("Temperature", "value", "column")]}

  if ("well" %in% .make_numeric)          { cant_change_that("well", ".make_numeric", "character")
    .make_numeric <- .make_numeric[!.make_numeric %in% c("well", "row")]}

  if ("row" %in% .make_numeric)           { cant_change_that("row", ".make_numeric", "character")
    .make_numeric <- .make_numeric[!.make_numeric %in% c("well", "row")]}

  # ensure no protected columns are present in the .make_character and .make_numeric selections


  # perform desired type-changes
  try(out <- out %>% mutate(across(all_of(.make_numeric), base::as.numeric)))
  try(out <- out %>% mutate(across(all_of(.make_character), base::as.character)))

  out
}

cant_change_that <- function( change_what, which_argument, keep_as ) {
  glue("You have supplied '{change_what}' to the {which_argument} argument. However, the {change_what} column must remain {keep_as}, so no change has been made to the type of the {change_what} column.") %>% print()
}

utils::globalVariables(c("protein"))
