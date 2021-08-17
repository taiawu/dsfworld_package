#' Calculate apparent melting temperatures using first derivative
#'
#' I AM NOT SURE THAT THE DEFAULT LOESS SPAN IS APPROPRIATE!! It's just the default in R. Follow up on this.
#'
#' @param data a tibble for a single dataset containing temperature and dRFU (first derivative) as columns
#' @param .x_vec name of the column containing temperature data
#' @param .y_vec name of the column containing dRFU data
#' @param .precision a number, giving the precision with which to determine tma
#' @param .loess_span a number, giving the span of the loess filter.
#' @param ... Allows any irrelevant arguments that might have been passed from upstream functions using ... to be ignored. This is relevant when det_drfu_tmas might be included in a larger analysis workflow which makes use of ...
#'
#' @return a number; the interpolated maximum of the dRFU data
#'
#' @importFrom tibble tibble
#' @importFrom rlang as_string
#' @importFrom glue glue
#' @importFrom modelr add_predictions
#' @importFrom stats loess
#'
#' @export
get_drfu_tmas <-
  function(data,
           .x_vec = "Temperature", # is Temperature
           .y_vec = "drfu_norm", # is drfu
           .precision = 0.1,
           .loess_span = 0.75,
           ...) {

    # handle either quoted or symbol inputs
    .x_vec <- as.name(substitute(.x_vec))
    .y_vec <- as.name(substitute(.y_vec))

    col_nm <- c(rlang::as_string(.x_vec),
                rlang::as_string(.y_vec))

    #_____Check input column names____
    if (!all( col_nm %in% names(data))) { # ensure user columns present in df
      abort_bad_argument("supplied column names not present in dataframe. All columns",
                         must = glue::glue("be in dataframe names: {glue::glue_collapse(names(data), sep = ', ')}"),
                         not = NULL ) }


    df <-
      tibble::tibble(x =  data[[.x_vec]],
                     y = data[[.y_vec]])

    grid <-
      tibble::tibble( x = seq(min(df$x), max(df$x), by = .precision)) %>%
      modelr::add_predictions(loess(y ~ x, data = df, span = .loess_span))

    tma <- grid$x[which(grid$pred == max(grid$pred))][[1]] # 1 is incase there are ~equal maxes
  }


#' Add drfu tmas as a column to nested dsf data
#'
#' @param by_var a nested tibble. one unique dataset per row, with data nested as a tibble. each tibble contains temperature, value, and drfu data
#' @param .data_col name of the column containing the nested data tibbles
#' @param ... arguments passed to get_drfu_tmas, such as .x_vec (name of temperature column, defaults to "Temperature), .y_vec (name of dRFU column, defaults to "drfu_norm"), .precision (precision to determine tmas to, defalts to 0.1)
#'
#' @return the by_var tibble, with a new column "drfu_tma"
#'
#' @importFrom rlang as_string
#' @importFrom dplyr mutate
#' @export
add_drfu_tmas <-
  function(by_var,
           .data_col = "data",
           ...) {
    # accept data col
    .data_col <-
      as.name(substitute(.data_col)) %>%
      rlang::as_string()
    out <-
      by_var %>%
      dplyr::mutate("drfu_tma" = purrr::map(.data[[.data_col]], get_drfu_tmas, ...)[[1]]) # ensure only one tma
  }
