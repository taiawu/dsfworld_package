#' Add tmas for components to a nested tibble
#'
#' @param df nested tibble
#'
#' @return nested tibble with column contaiing tmas from components
#'
#' @importFrom dplyr mutate select
#' @importFrom purrr map
#' @importFrom tidyr unnest
#'
#' @export
add_component_tmas <-
  function(df) {
    out <-
      df %>%
      dplyr::mutate(model_tma =
                      tryCatch( purrr::map(.data$data, extract_component_tma),
                                error = function(e) {NA}, warning = function(w) {NA})) %>%
      dplyr::select(.data$.var,.data$ model_name, .data$model_tma) %>%
      tidyr::unnest(cols = c(.data$model_tma))

  }

#' Extract tmas from each component using max drfu method
#'
#' @param data dataframe containing tma components
#' @param component_names names of the components to extract tmas from
#'
#' @return tibble containing tmas and their numbering (e.g. tma1, tma2...)
#'
#' @importFrom dplyr filter group_by mutate ungroup select
#' @importFrom tidyr nest
#' @importFrom purrr map
#' @importFrom scales rescale
#'
#' @export
extract_component_tma <-
  function(data,
           component_names = c("sigmoid_1", "sigmoid_2", "sigmoid_3")) {
    # dplyr filter group_by mutate ungroup select
    # tidyr nest
    # purrr map
    # scales rescale

    df <- data %>%
      dplyr::filter(.data$which_value %in% component_names) %>%
      dplyr::group_by(.data$which_value) %>%
      tidyr::nest() %>%
      dplyr::mutate(data = purrr::map(.data$data, ~ dplyr::mutate(.x,
                                                                  value_norm = scales::rescale(.data$value, to = c(0,1)),
                                                                  drfu_norm = sgolay(.data$value_norm, m = 1)
      )),
      tma = purrr::map(data, get_drfu_tmas, .y_vec = "drfu_norm")[[1]])

    out <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$which_value, .data$tma)
  }

#' Sort tmas returned from models into tma1, tma2, etc
#'
#' @param df df of tmas
#' @param sort_type sorting method. defaults to "low_high". Also can be "closest_to"
#' @param close_val if closest to, the value to rank tmas by proximity to.
#' @param .tma_col name of column containing tma data. defaults to tma.
#'
#' @return the df, with a "rank" column
#'
#' @importFrom dplyr group_by mutate min_rank arrange
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom utils globalVariables
#' @export
sort_tmas <-
  function(df,
           sort_type = "low_high",
           close_val = 0,
           .tma_col = "tma") {

    if(is.null(df)) { stop("Input dataframe is NULL")}
    if(!is.numeric(close_val)) {
      warning("`close_val` is not numeric. Coercing to numeric.")
      close_val <- as.numeric(close_val)
      if(is.na(close_val)) {warning("`close_val couldn't be converted to numeric. Coerced to 0")}
    }

    .tma_col <- base::as.name(base::substitute(.tma_col))

    close_val <-
      switch(sort_type,
             "low_high" = 0,
             "closest_to" = close_val)

    df <- df %>%
      dplyr::group_by(.data$.var, .data$model_name) %>%
      tidyr::nest() %>%
      dplyr::mutate(data = purrr::map(.data$data, ~
                                        dplyr::mutate(.x,
                                                      rank =  try(dplyr::min_rank(abs({{ .tma_col }} - close_val)) )))) %>%
      tidyr::unnest(cols = c(.data$data)) %>%
      dplyr::arrange(.data$.var, .data$model_name, .data$rank)
  }

# consider refactoring .R files so these functions don't show up like this
utils::globalVariables(c("get_drfu_tma"))
