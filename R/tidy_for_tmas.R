#' Tidy raw dsf dataframe for tma calculations
#'
#' Also adds 0 - 1 range normalizations of temperature and raw data, and the first derivative of the normalizsed raw value.
#'
#' @param df tibble containing long-form raw dsf data
#' @param unique_col name of the column containing a unique identifier for each dataset. Defaults to "well".
#' @param value_col name of the column containing raw rfu values. Defaults to "value".
#' @param temp_col name of the column containing temperatures. Defaults to "Temperature"
#' @param keep_cols name of all column to keep. Defaults to minimum: "well", "Temperature", "value", and "value_norm"
#'
#' @return a nested tibble containig tidied data for downstream tma analyses. Column names are .var (for unique identifier), and data (for the nested data corresponding to each value of .var). Each nested tibble contains numeric columns: Temperature, Temperature_norm, value, value_norm, and drfu_norm.
#'
#' @importFrom rlang as_string
#' @importFrom glue glue glue_collapse
#' @importFrom signal sgolayfilt
#' @importFrom dplyr select rename group_by mutate relocate
#' @importFrom tidyselect all_of
#' @importFrom scales rescale
#' @importFrom tidyr nest
#'
#' @export
tidy_for_tmas <-
  function(df,
           unique_col = "well",
           value_col = "value",
           temp_col = "Temperature",
           keep_cols = c("well", "Temperature", "value", "value_norm")) {

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
      dplyr::relocate(.data$.var, .data$Temperature, .data$Temperature_norm, .data$value, .data$value_norm, .data$drfu_norm) %>%
      tidyr::nest()
  }
