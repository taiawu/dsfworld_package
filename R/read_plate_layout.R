#' Read a plate layout file into a tibble
#'
#'read_layout() reads a plate layout file (.csv, .txt, .xls, or .xlsx), and returns it as a formatted tibble. Variable types are guessed with readr::parse_guess(). The originally required "Type" column heading is now optional.
#'
#'
#'
#' @param filepath A complete file path, pointing to the plate layout file
#' @param .empty_vals A vector containing all values which signify an empty well.
#'
#' @return Returns a tibble, mapping experimental variables to well positions. All outputs contain the columns: row, column, and well.
#'
#' A single column is added containing the values for all user-defined variables, separated by '__'. If all experimental variables are defined in the layout, wells with identical entries in the "condition" column are technical replicates.
#'
#' Column types for each variable (e.g. character, numeric) are guessed using \code{readr::parse_guess()}.
#'
#' Wells for which all variables contain one of the "empty well" values supplied to the argument `.empty_vals` are dropped from the layout.
#'
#' @importFrom tidyselect everything
#' @importFrom tools file_ext
#' @importFrom readr read_csv read_tsv parse_guess
#' @importFrom readxl read_excel
#' @importFrom purrr set_names discard
#' @importFrom dplyr filter if_all mutate across
#' @importFrom tidyr pivot_longer pivot_wider unite
#' @importFrom utils "globalVariables"
#' @importFrom rlang .data
#'
#' @export
read_plate_layout <- function(filepath, .empty_vals = c("Empty", "empty", NA)) {

  # read file based on it's type
  ext <- file_ext(filepath)

  raw <- switch(ext,
                csv = read_csv(filepath, col_names = FALSE),
                txt = read_tsv(filepath, col_names = FALSE),
                xlsx = read_excel(filepath, col_names = FALSE),
                xls =  read_excel(filepath, col_names = FALSE)
  ) |>  base:: suppressMessages()

  # handle files with or without the "Type" header
  first_cell <- raw[1,1][[1]] # first cell of the file
  out <- switch(first_cell,
                Type = raw[-1,],
                raw)

  # extract plate column numbers
  plate_cols <- unlist(out[1,][-c(1,2)]) # extract column numbers to vector

  # convert into layout form
  layout <- out  |>
    set_names( c("variable", "row", plate_cols)) |>
    filter(row %in% c(base::letters[1:16],base::LETTERS[1:16])) |>
    discard(~all(is.na(.x)))  |> # drop columns if everything is NA
    filter(if_all(everything(), ~ !is.na(.x)))  |>
    mutate(across(everything(), as.character)) |> # make all character, to prevent issues in pivot
    pivot_longer(-c("variable", "row"), names_to = "column", values_to = "value") |>
    pivot_wider(names_from = "variable", values_from = "value") |>
    mutate(well = paste0("row", "column")) |> # make well column
    unite(condition, -c("row", "column", "well"), sep = "__", remove = FALSE)  |> # make condition column
    filter(if_all(-c("well", "row", "column", "condition"), \(x) !x %in% .empty_vals)) |>
    mutate(across(everything(), parse_guess)) # convert likely numeric variables to numeric

}
