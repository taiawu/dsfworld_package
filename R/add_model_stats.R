#' Add statistics
#'
#' @param by_var A nested tibble, as output by add_nls()
#' @param ... additional named arguments, which is supplied here to serve the purpose of allowing irrelevant arguments passed from upstream functions using ... to be ignored.
#'
#' @return The input nested df, with an additional columns named "tidied", and "glanced" containing the outputs of broom::tidy and broom::glance, respectively.
#'
#' @export
add_model_stats <-
  function(by_var, ...) {
    by_var %>%
      mutate(tidied  = tryCatch(purrr::map(.data$model, broom::tidy), warning = function(w) return(NA), error = function(e) return(NA)), # extract model parameters
             glanced = tryCatch(purrr::map(.data$model, broom::glance), warning = function(w) return(NA), error = function(e) return(NA)))
  }
