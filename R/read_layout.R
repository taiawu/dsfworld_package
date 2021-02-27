read_layout <- function(filepath){

  # read file based on it's type
  ext <- tools::file_ext(filepath)

  raw <- switch(ext,
                csv =  readr::read_csv(filepath, col_names = FALSE),
                txt =  readr::read_tsv(filepath, col_names = FALSE),
                xlsx = readxl::read_excel(filepath, col_names = FALSE),
                xls =  readxl::read_excel(filepath, col_names = FALSE)
  )

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
    tidyr::pivot_longer(-c(variable, row), names_to = "column", values_to = "value") %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    dplyr::mutate(well = paste0(row, column)) %>% # make well column
    tidyr::unite(condition, -c(row, column, well), sep = "__", remove = FALSE) %>% # make condition column
    dplyr::filter(!across(-c(well, row, column, condition)) == "Empty") %>% # if all variables are "Empty",   wells
    dplyr::filter(!is.na(across(-c(well, row, column, condition))) == TRUE) %>% # if all variables are NA,   wells
    dplyr::mutate(across(everything(), readr::parse_guess)) # convert likely numeric variables to numeric
}
