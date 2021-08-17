#' Add model fits to nested data
#'
#' @param by_var A nested tibble, as output by add_start_pars().
#' @param formula formula for the model to be fit
#' @param lower_bound named numeric vector containing lower bounds, in order, for the input parameters.
#' @param control_list a named list, passed directly to nlslM, to set controls for the fitting.
#' @param ... additional named arguments, which will be passed to the helper function get_nls. Also serves the purpose of allowing irrelevant
#'
#' @return The input nested df, with an additional column named "model", containing the input formula fitted to the data. Is either an s3 model class, or s3 faild model class.
#'
#' @export
add_nls <-
  function(by_var,
           formula,
           lower_bound,
           control_list = list(maxiter = 500),
           ...) {
    by_var %>%
      dplyr::mutate(model = purrr::map2(.data$data,
                                        .data$pars,
                                        get_nls,
                                        formula = formula, # global variable
                                        lower_bound = lower_bound,
                                        ...)) # where is this coming from again? formula, right?
  }

# ---------- get_nls


#' Fit a given formula to single trace of DSF data
#'
#' @param df a tibble containing x and y data for a single trace of DSF data. In default use, this is Temperature_norm and value_norm.
#' @param start_pars named list of starting parameters for the supplied formula. Passed to nlsLM.
#' @param formula formula to fit. Passed to nlsLM.
#' @param lower_bound a named numeric vector, in identical parameter order to start_pars, containing lower bounds for the input functions.
#' @param control_list an optional named list of controls to pass to nlslM, including max interations.
#' @param ... additional arguments, which, if inherited from upstream functions, can be ignored by this function.
#'
#' @return a fitted model, exactly as output by nlslM. Or, if the nlsLM operation failed, returns NA.
#'
#'
#' @export
get_nls <- # an updated version of compose_dsf_model, with a better name
  function(df,
           start_pars,
           formula,
           lower_bound,
           control_list = list(maxiter = 500),
           ...) {
    tryCatch(
      minpack.lm::nlsLM(formula,
                        data = df,
                        start = start_pars,
                        lower = lower_bound,
                        control = control_list) ,
      warning = function(w) return(NA),
      error = function(e) return(NA)
    )
  }
