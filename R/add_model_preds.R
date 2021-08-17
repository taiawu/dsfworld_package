#' Add model predcition to nested data.
#'
#' @param by_var  A nested tibble, as output by add_nls()
#' @param ... additional named arguments, passed to downstream functions. Also serves the purpose of allowing irrelevant arguments passed from upstream functions using ... to be ignored.
#'
#'
#' @return The input nested df, with the data column updated to include predictions for the individual sigmoids from the model fits.
#'
#' @export
add_model_preds <-
  function(by_var,
           ...) {

    pred_long  <-
      tryCatch({add_standard_preds(by_var, ...) %>%
          drop_and_pivot(., ...)},
          warning = function(w) return(by_var),
          error = function(e) return(by_var))


    out <-
      tryCatch(add_component_predictions(pred_long, ...),
               warning = function(w) return(NULL),
               error = function(e) return(NULL))
  }

#' Drop unneeded columns from data, and pivot longer
#'
#' A helper function to add_predictions
#'
#' @param by_var A nested tibble, as output by add_nls()
#' @param drop_raw A boolean, specifying whether or not to drop unnecessary columns from data. Defaults to TRUE, which minimizes output size, but requires re-joining with layouts after fitting for exploratory data analysis and plotting.
#' @param temperature_col a character vector containing, as a string, the name of the column containing normalized Temperature. Defaults to c("Temperature_norm"). Could also include non-normalized Temperature (e.g. c("Temperature_norm", "Temperature")), but this is currently not particularly well supported by the downstream functions.
#' @param keep_data_cols a character vector containing, as strings, the names of the columns to retain in the data tibble. Defaults to resid, and pred, which contain the residuals and predictions output by modelr::add_predictions().
#' @param ... additional arguments to be passed to downstream functions.
#'
#' @return the data tibble, with unnneded columns removed, and converted to long-form, with columns Temperature_norm, which_value (contents include "resid" and "pred"), and value.
#'
#' @export
drop_and_pivot <-
  function(by_var,
           drop_raw = TRUE,
           temperature_col = c("Temperature_norm"),
           keep_data_cols = c("resid", "pred"),
           ...) {

    tryCatch({
      if( drop_raw ) {
        by_var <- by_var %>%
          dplyr::mutate(data =
                          tryCatch(purrr::map(.data$data,
                                              ~ dplyr::select(.x,
                                                              tidyselect::any_of(c(temperature_col, keep_data_cols)))),
                                   warning = function(w) return(.data$data),
                                   error = function(e) return(.data$data)))
      }


      pred_long <- by_var %>%
        dplyr::mutate(data  =
                        tryCatch(purrr::map(.data$data,
                                            tidyr::pivot_longer,
                                            cols = tidyselect::any_of(keep_data_cols),
                                            names_to = "which_value",
                                            values_to = "value"),
                                 warning = function(w) return(.data$data),
                                 error = function(e) return(.data$data)))
    },
    warning = function(w) return(by_var),
    error = function(e) return(by_var))

  }

#' Add standard predictions from modelr to data.
#'
#' @param by_var A nested tibble, as output by add_nls()
#' @param ... additional arguments to be passed to downstream functions
#'
#' @return The data tibble, with residuals and predictions added, as calculated by modelr::add_residuals and modelr::add_predictions.
#'
#' @export
add_standard_preds <-
  function(by_var, ...) {
    df <-
      by_var %>%
      mutate(data = tryCatch(purrr::map2(.data$data, .data$model, modelr::add_residuals),
                             warning = function(w) return(.data$data), error = function(e) return(.data$data)),
             data = tryCatch(purrr::map2(.data$data, .data$model, modelr::add_predictions),
                             warning = function(w) return(.data$data), error = function(e) return(.data$data)))
  }

#' Extract indiviidual component expressions from  model formulas
#'
#' @param component_type the type of component to extract expressions from; "sigmoid", or "initial"
#'
#' @return a quoted fomrula for an individual component, which is evaluated in downstream functions to reconstruct individual sigmoid components from model fits.
#'
get_component_exprs <-
  function(component_type) {
    switch(component_type,
           "sigmoid" = quote(Asym/(1 + exp((xmid - x_val)/scal)) * exp(d * (x_val - xmid))),
           "initial" = quote(id_d * exp(x_val * id_b)))
  }


#' Add predictions for individual model components
#'
#' I honestly don't exactly re
#' @param by_var A nested tibble, as output by add_nls()
#' @param ... additional arguments to be passed to downstream functions.
#'
#' @return The input nested df, with the columns tidied and data.
#'
#' @export
add_component_predictions <-
  function(by_var, ...) {
    # this should be updated so that the tidied column doesn't need
    out <-
      by_var %>%
      dplyr::mutate(tidied = purrr::map(.data$tidied, annotate_tidied_components),
                    data = purrr::map2(.data$tidied, .data$data, get_component_predictions))
  }

#' Annotate tidied components
#'
#' A helper function for add_component_predictions
#'
#' @param tidied a tibble resulting from broom::tidy of a dsfworld model fit.
#'
#' @return the same tibble, with columns added: "term_type", and "component", to facilitate reconstruction and formulas to predict components.
#'
#' @export
#'
annotate_tidied_components <-
  function(tidied) {
    tryCatch({
      tidied %>% # the result of broom::tidy
        dplyr::mutate(component_number = readr::parse_number(.data$term),
                      term_type = gsub(pattern = "([0-9]+).*$", replacement = "", .data$term),
                      component_type = if_else(.data$term_type %in% c("id_d", "id_b"),
                                               true = "initial",
                                               false = "sigmoid")) %>%
        unite("component", c(.data$component_type, .data$component_number), sep = "_", remove = FALSE)
    }, warning = function(w) return(tidied) ,
    error = function(e) return(tidied) )
  }

#' Get component predictions
#'
#' @param tidied  a tibble resulting from annotate_tidied_components
#' @param data a tibble of dsf data, resulting from drop_and_pivot, to which component predictions will be added.
#' @param temp_col a string, containing the name of the column in the dataframe supplied to the data argument, whch contains the x data used to fit the model. Defaults to Temperature_norm. If the original model was fit to Temperature_norm, this must also be Temperature_norm--it cannot be "Temperature"
#' @param rescale_temp_col boolean, that, if true, triggers the rescaling of the input Temperautre column to a 0 to 1 range.
#' @param ... additional parameters passed to downstream functions or ignored.
#'
#' @return the data tibble, with additional rows corresponding to the reconstructed component.
#'
#' @export
get_component_predictions <-
  function(tidied,
           data,
           temp_col = "Temperature_norm",
           rescale_temp_col = TRUE, ...) {
    tryCatch({
      temp_vec <- data %>% pull({{ temp_col }})

      #### needs to be for temp norm
      if(rescale_temp_col) { temp_vec <- scales::rescale(temp_vec)}

      component_pred <-
        tidied %>%
        dplyr::select(.data$term, .data$term_type, .data$component_type, .data$component, .data$estimate) %>%
        dplyr::group_by(.data$component) %>%
        tidyr::nest() %>%
        dplyr::mutate(comp_pred = purrr::map(.data$data, predict_component, temp_vec = temp_vec)) %>%
        dplyr::select(.data$component, .data$comp_pred) %>%
        tidyr::unnest(cols = c(.data$comp_pred)) %>%
        set_names(c("which_value", temp_col, "value"))  %>%
        mutate(!!temp_col := data %>% pull({{ temp_col }})) # replacing again with non-norm temperature

      out <-
        data %>%
        dplyr::bind_rows(component_pred)
    },
    error = function(e) return(data), # return the input unchanged
    warning = function(w) return(data)) # return the input unchanged
  }

#' Predict y values for an individual component
#'
#' A helper function for get_component_predictions. Given a qutoed formula and x data, predicts the values of he individual components.
#'
#' @param df data, after drop_and_pivot, to which the predicted component values will be added.
#' @param temp_vec numeric vector containing the temperature
#' @param .component_type_col the name of the column in the df argument, containing the componet type to be reconstructed.
#' @param .est_col a string, giving the name of the column containing estimates values.
#' @param .term_type_col a string, giving the name of the column containing the term type values.
#'
#' @return a tibble containing two columns, x and y, containing the input temperature vector and the predicted y values for the predicted component.
#'
#' @export
predict_component <-
  function(df,
           temp_vec,
           .component_type_col = "component_type",
           .est_col = "estimate",
           .term_type_col = "term_type") {

    # fetch the parameters and formulas for the component
    component_type <- # e.g. initial or sigmoid
      unique(df[[.component_type_col]])

    to_eval <- # pull the expressions used in that component
      get_component_exprs(component_type)

    # bundle parameter values into a named list
    par_list <- # get the estimated parameter
      as.list(df[[.est_col]])

    names(par_list) <- # name each parameter
      df[[.term_type_col]]

    par_list <-
      c(par_list, # coefficients of the fit
        "x_val" = list(temp_vec)) # Temperatures (usually normalized)

    # evaluate the formula using the given parameters
    tibble::tibble(x = temp_vec, # need to be 0,1, or match what was given in the fitting!
                   y = eval(to_eval, par_list))
  }
