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
#'
#' @importFrom magrittr "%>%"
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
read_plate_layout <- function(filepath) {

  # read file based on it's type
  ext <- file_ext(filepath)

  raw <- switch(ext,
                csv = read_csv(filepath, col_names = FALSE),
                txt = read_tsv(filepath, col_names = FALSE),
                xlsx = read_excel(filepath, col_names = FALSE),
                xls =  read_excel(filepath, col_names = FALSE)
  ) %>% base:: suppressMessages()

  # handle files with or without the "Type" header
  first_cell <- raw[1,1][[1]]
  out <- switch(first_cell,
                Type = raw[-1,],
                raw)

  # convert into layout form
  out %>%
    set_names( c("variable", "row", .[1,][-c(1,2)])) %>%
    filter(row %in% base::LETTERS[1:16]) %>%
    discard(~all(is.na(.x)))  %>% # drop columns if everything is NA
    filter(if_all(everything(), ~ !is.na(.x))) %>%
    mutate(across(everything(), as.character)) %>% # make all character, to prevent issues in pivot
    pivot_longer(-c(.data$variable, .data$row), names_to = "column", values_to = "value") %>%
    pivot_wider(names_from = .data$variable, values_from = .data$value) %>%
    mutate(well = paste0(.data$row, .data$column)) %>% # make well column
    unite(condition, -c(.data$row, .data$column, .data$well), sep = "__", remove = FALSE) %>% # make condition column
    filter(!across(-c(.data$well, .data$row, .data$column, .data$condition)) == "Empty") %>% # if all variables are "Empty",   wells
    filter(!is.na(across(-c(.data$well, .data$row, .data$column, .data$condition))) == TRUE) %>% # if all variables are NA,   wells
    mutate(across(everything(), parse_guess)) # convert likely numeric variables to numeric
}

utils::globalVariables(c(".", "condition"))
