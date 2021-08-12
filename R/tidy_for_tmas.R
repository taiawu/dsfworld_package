tidy_for_tmas <-
  function(df,
           unique_col = "well",
           value_col = "value",
           temp_col = "Temperature",
           keep_cols = c("well", "Temperature", "value", "value_norm")) {
    # rlang as_string
    # glue glue glue_collapse
    # signal sgolayfilt
    # deply select rename group_by mutate
    # tidyselect all_of
    # scales rescale
    # tidyr nest

    # handle either quoted or symbol inputs
    unique_col <- as.name(substitute(unique_col))
    value_col <- as.name(substitute(value_col))
    temp_col <- as.name(substitute(temp_col))

    col_nm <- c(rlang::as_string(unique_col),
                rlang::as_string(value_col),
                rlang::as_string(temp_col))

    #_____Check input column names____
    if (!all( col_nm %in% names(df))) { # ensure user columns present in df
      abort_bad_argument("supplied column names not present in dataframe. All columns",
                         must = glue::glue("be in dataframe names: {glue::glue_collapse(names(df), sep = ', ')}"),
                         not = NULL ) }


    # all columns are of correct type
    df %>%
      dplyr::select(tidyselect::all_of(col_nm)) %>%
      dplyr::rename(.var = {{ unique_col }},
                    value = {{ value_col}},
                    Temperature = {{ temp_col}}) %>%
      dplyr::group_by(.data$.var) %>%
      dplyr::mutate(value_norm = scales::rescale(.data$value, to = c(0, 1)),
                    Temperature_norm = scales::rescale(.data$Temperature, to = c(0, 1)),
                    drfu_norm = signal::sgolayfilt(.data$value_norm, p = 5, n = 13, m = 1)) %>% # could (should?) use dyanmic formals eventually?
      tidyr::nest()
  }
