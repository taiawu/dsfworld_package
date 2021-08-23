#' Calculate apparent melting temperatures using first derivative
#'
#' I AM NOT SURE THAT THE DEFAULT LOESS SPAN IS APPROPRIATE!! It's just the default in R. Follow up on this.
#'
#' @param data a tibble for a single dataset containing temperature and dRFU (first derivative) as columns
#' @param .x_vec name of the column containing temperature data
#' @param .y_vec name of the column containing dRFU data
#' @param .n_interp a number, giving the number of points to appear in the interpolation. Passed to stats::approx. Defaults to 50.
#' @param .n_points_either_side a number, giving the number of points on either side of the dRFU maximum to carry forward into interpolation. Default is 1, which gives a three-point range for linear interpolation..
#' @param ... Allows any irrelevant arguments that might have been passed from upstream functions using ... to be ignored. This is relevant when det_drfu_tmas might be included in a larger analysis workflow which makes use of ...
#'
#' @return a number; the interpolated maximum of the dRFU data
#'
#' @importFrom tibble tibble
#' @importFrom rlang as_string
#' @importFrom glue glue
#' @importFrom modelr add_predictions
#' @importFrom stats approx
#'
#' @export
get_drfu_tmas <-
function(data,
         .x_vec = "Temperature", # is Temperature
         .y_vec = "drfu_norm", # is drfu
         .n_interp = 50,
         .n_points_either_side = 1,
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
                   y = data[[.y_vec]]) %>%
    dplyr::mutate(ddrfu_norm = sgolay(.data$y, m = 1)) # use double deriv for linear fit


  # extract the area of the local maximum
  which_max_y <- which.max(df[["y"]]) # which measurement contains
  first_meas <- which_max_y - .n_points_either_side
  last_mmeas <- which_max_y + .n_points_either_side

  df_local_max <- # df containining only the local region
    df[c(first_meas:last_mmeas),]

  # double deriv should be linear through zero at drfu max
  appx <- # should be linear with negative slope
    stats::approx(x = df_local_max$x,
                  y = df_local_max$ddrfu_norm,
                  method = "linear",
                  n = .n_interp,
                  rule = 1,
                  f = 0,
                  ties = mean)

  tma <- appx$x[which(appx$y == min(abs(appx$y)))][[1]] # 1 is incase there are ~equal maxes
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
