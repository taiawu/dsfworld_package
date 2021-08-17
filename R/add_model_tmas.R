#' Add dRFU tmas to nested dataframe
#'
#' @param by_var  nested tibble, as output by add_model_preds()
#' @param ... additional named arguments, passed to downstream functions. Also serves the purpose of allowing irrelevant arguments passed from upstream functions using ... to be ignored.
#'
#' @return The input nested df, with an additioan column, model_tma,as calculated using the maximum of the dRFU for each reconstructed model component.
#'
#' @export
add_model_tmas <-
  function(by_var, ...) {
    out <-
      by_var %>%
      dplyr::mutate(model_tma = tryCatch( purrr::map(.data$data, extract_component_tma, ...),
                                          error = function(e) {NA}, warning = function(w) {NA}))
  }

#' Extract component tmas
#'
#' A helper function for add_model_tmas. Extracts apparent melting temperatures from individually-predicted sigmoids from model fits. Uses maximum of dRFU to do this.
#'
#' @param data a tibble, as returned by add_model_preds().
#' @param component_names the names of the components from which to extract Tmas. Defaults to c("sigmoid_1", "sigmoid_2", "sigmoid_3"). Components listed but not found in data are silently ignored.
#' @param ... additional arguments, passed to get_drfu_tmas. Also serves the purpose of allowing irrelevant arguments passed from upstream functions using ... to be ignored.
#'
#' @return a tibble containing two columsn, "which_value", a character defining which of the input component_names the Tma corresponds to, and "tma", the tma value determined using get_drfu_tmas. Note that the tmas will be for the x vector to which the models were fit, which in the standard workflow, is "Temperature_norm". Conversion to real temperatures currently is handled outside of the fitting workflow.
#'
#' @export
extract_component_tma <-
  function(data,
           component_names = c("sigmoid_1", "sigmoid_2", "sigmoid_3"),
           ...) {

    df <- data %>%
      dplyr::filter(.data$which_value %in% component_names) %>%
      dplyr::group_by(.data$which_value) %>%
      tidyr::nest() %>%
      dplyr::mutate(data = purrr::map(.data$data, ~ dplyr::mutate(.x,
                                                                  value_norm = scales::rescale(.data$value, to = c(0,1)),
                                                                  drfu_norm = sgolay(.data$value_norm, m = 1)
      )),
      tma = purrr::map(data, get_drfu_tmas, ...)[[1]])

    out <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$which_value, .data$tma)
  }
