#' Wrapper for Savistky-Golay filtering
#'
#' A closure which sets defaults frequently appropriate for DSF for p (filter order) and n (filter length). Allows the filter to be called with the simplier syntax later. e.g. sgolay(value, 1) returns the first derivative of the vector 'value'. Formals for sgolay and be reset after calculations performed on user data to determine ideal p and n. This behavior is not implemented yet.
#'
#'Savistky-Golay filtering is used for all smoothing and derivative calculations in dsfworld.
#'
#' @param x a numeric vector to be savitsky-golay filtered
#' @param p the filter order to use. Defaults to 5. Must be odd and >2.
#' @param n the number of measurements in the filter window. Defaults to 13.
#' @param m the filter order. 0: smoothing only; 1: first derivative, 2: second derivative, etc.
#'
#' @return a numeric vector, with a savistky-golay filter applied.
#'
#' @importFrom signal sgolayfilt
#'
#' @export
sgolay <- function(x, p, n, m) {signal::sgolayfilt(x, p = 5, n = 13, m)}


#' Generate helpful error messages for invalid function inputs
#'
#' This function appears in Advanced R Chapter 8.5.2 Signalling
#' \url{https://adv-r.hadley.nz/conditions.html#signalling}
#' It was presumably written by Hadley Wickham.
#' Generates a helpful error message and passes it to rlang::abort. Does not determine argument validity. Use this function inside a statement which determines that an input is invalid. Is adapted with extremely minor modifications from
#'
#'
#' @param arg the invalid function argument
#' @param must what a valid argument must be (e.g "numeric", or "all_models")
#' @param not what a valid argument cannot be (e.g. "character" or "adlkfm_modelz")
#'
#' @return an error. prints the custom message to the console.
#'
#' @importFrom glue glue
#' @importFrom rlang abort
#'
#' @export
abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    msg <- glue::glue("{msg}; not `{not}`.")
  }

  rlang::abort("error_bad_argument",
               message = msg,
               arg = arg,
               must = must,
               not = not
  )
}
