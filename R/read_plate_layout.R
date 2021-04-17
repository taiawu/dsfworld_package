#' Read a plate layout file into a tibble
#'
#'read_layout() reads a plate layout file (.csv, .txt, .xls, or .xlsx), and returns it as a formatted tibble. Variable types are guessed with readr::parse_guess(). The originally required "Type" column heading is now optional.
#'
#'
#'
#' @param filepath A complete file path, pointing to the plate layout file
#'
#' @return Returns a tibble, mapping experimental variables to well positions. All outputs contain the columns: row, column, and well. Additionally, a single column is added for each user-defined variable in the plate layout file, from which one additional "condtion" column is created, which contains all experimental variables. If all experimental variables are defined in the layout, wells with identical entries in the "condition" column are technical replicates.
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import purrr
#' @import readr
#' @import readxl
#' @import tidyr
#' @import tools
#' @importFrom utils "globalVariables"
#'
#'
#' @export
read_plate_layout <- function(filepath){

  # read file based on it's type
  ext <- tools::file_ext(filepath)

  raw <- switch(ext,
                csv =  readr::read_csv(filepath, col_names = FALSE),
                txt =  readr::read_tsv(filepath, col_names = FALSE),
                xlsx = readxl::read_excel(filepath, col_names = FALSE),
                xls =  readxl::read_excel(filepath, col_names = FALSE)
  ) %>% base:: suppressMessages()

  # handle files with or without the "Type" header
  first_cell <- raw[1,1][[1]]
  out <- switch(first_cell,
                Type = raw[-1,],
                raw)

  # convert into layout form
  out %>%
    purrr::set_names( c("variable", "row", .[1,][-c(1,2)])) %>%
    dplyr::filter(row %in% base::LETTERS[1:16]) %>%
    purrr::discard(~all(is.na(.x)))  %>% # drop columns if everything is NA
    dplyr::filter(if_all(everything(), ~ !is.na(.x))) %>%
    dplyr::mutate(across(everything(), as.character)) %>% # make all character, to prevent issues in pivot
    tidyr::pivot_longer(-c(.data$variable, .data$row), names_to = "column", values_to = "value") %>%
    tidyr::pivot_wider(names_from = .data$variable, values_from = .data$value) %>%
    dplyr::mutate(well = paste0(.data$row, .data$column)) %>% # make well column
    tidyr::unite(condition, -c(.data$row, .data$column, .data$well), sep = "__", remove = FALSE) %>% # make condition column
    dplyr::filter(!across(-c(.data$well, .data$row, .data$column, .data$condition)) == "Empty") %>% # if all variables are "Empty",   wells
    dplyr::filter(!is.na(across(-c(.data$well, .data$row, .data$column, .data$condition))) == TRUE) %>% # if all variables are NA,   wells
    dplyr::mutate(across(everything(), readr::parse_guess)) # convert likely numeric variables to numeric
}

utils::globalVariables(c(".", "condition"))
