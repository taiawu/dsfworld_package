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
