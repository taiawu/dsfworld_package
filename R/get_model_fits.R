#' Extract apparent tms from model component fits
#'
#' @param by_var by var df
#' @param which_models models to fit
#' @param dsfworld_models use dsfworld models?
#' @param model_pars defaults to NULL, user supplies if using their own model
#' @param model_formula defaults to NULL, user supplies if using their own model
#' @param model_lower_bound defaults to NULL, user supplies if using their own model
#' @param ... passed to the model fitting
#'
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr mutate relocate
#' @importFrom utils globalVariables
#'
#' @return a df containing model tmas
#'
#' @export
add_model_fits <-
  function(by_var,
           which_models, # options c("model_1", "model_2", "model_3", "model_4", "model_5", "model_6")
           #dsfworld_formulas, # should we make this an argument? unsure how to pass formulas to this function
           dsfworld_models = TRUE,
           model_pars = NULL,
           model_formula = NULL,
           model_lower_bound = NULL,
           ...) {

    dsfworld_model_names <- c("model_1", "model_2", "model_3", "model_4", "model_5", "model_6")

    if( which_models == "all") { which_models <- dsfworld_model_names }
    # model names are unique
    if(! length(which_models) == length(unique(which_models))) {
      warning("model names supplied to `which_models` are not all unique. \n Fitting each model name only once.")
      which_models <- unique(which_models) }

    # if dsfworld models, names can be found
    if(dsfworld_models == TRUE) {
      if(is.null(formulas)) {
        stop("dsfworld models selected, but `formulas` object cannot be found. Please defines `formulas` object in the global workspace.")
      }

      if(! all(which_models %in% dsfworld_model_names)) {
        stop(glue::glue("Supplied model names not in dsfworld models. \n dsfworld model names are: {glue::glue_collapse(dsfworld_model_names, sep = ', ')}"))
      }
    }

    # if user-supplied models, all input are legit
    if(dsfworld_models == FALSE) {
      if (any(is.null(model_pars),
              is.null(model_formula),
              is.null(model_lower_bound))) {
        stop("`dsfworld_models` set to FALSE, but user-supplied model aruguments `model_pars`, `model_formula`, `model_lower_bound` are missing.")
      }
    }

    out <-
      which_models %>%
      lapply( . ,function(x) {
        get_model_fits(by_var,
                       formulas[[x]]$pars %>% names(),
                       formulas[[x]]$formula,
                       formulas[[x]]$lower_bound) %>%
          dplyr::mutate(model_name = x) %>%
          dplyr::relocate(.data$.var, .data$model_name)}
      ) %>%
      dplyr::bind_rows()
  }

#' Fit all supplied datasets to a single requested model
#'
#' @param by_variable nested tibble
#' @param which_pars names of the parameters in the model
#' @param formula formula to fit
#' @param lower_bound lower bound for model parameters
#' @param drop_raw drop the raw data from the results? defaults to TRUE
#' @param temperature_col name of column containing temperature data. defaults to c("Temperature")
#' @param keep_data_cols names of columns to retain in the fit results. defaults to c("resid", "pred")
#' @param ... more arguments, passed to the fit
#'
#' @return a dataframe containing nested rows with the fitted model, and tidied, glanced, predicted, and residual values
#'
#' @importFrom dplyr filter select mutate
#' @importFrom tidyselect any_of
#' @importFrom purrr map map2
#' @importFrom broom tidy glance augment
#' @importFrom modelr add_residuals add_predictions
#' @importFrom tidyr pivot_longer
#'
#' @export
get_model_fits <-
  function(by_variable,
           which_pars, # formulas$model_2$pars %>% names())[1]
           formula, # formulas$model_2$formula
           lower_bound, # manual_pars$m2_low
           drop_raw = TRUE,
           temperature_col = c("Temperature"),
           keep_data_cols = c("resid", "pred"),
           ...
  ) {
    # dplyr filter
    # purrr map map2
    # broom tidy glance augment
    # modelr add_residuals add_predictions
    out <-
      by_variable %>%
      ### ultimately, I think we could / should add starting parameter estimates to this function
      dplyr::mutate(est = map(.data$data, add_estimates),
                    pars = tryCatch( # translate estimates to start pars
                      purrr::map(.data$est,
                                 pars_from_estimates,
                                 which_pars = which_pars)[1],
                      warning = function(w) return(NA), error = function(e) return(NA)),
                    model = tryCatch( # fit to the given formula
                      purrr::map2(.data$data,
                                  .data$pars,
                                  fit_nls,
                                  formula = formula,
                                  lower_bound = lower_bound),
                      warning = function(w) return(NA), error = function(e) return(NA)),
                    # model qualities and predictions
                    tidied  = tryCatch(purrr::map(.data$model, broom::tidy), warning = function(w) return(NA), error = function(e) return(NA)), # extract model parameters
                    glanced = tryCatch(purrr::map(.data$model, broom::glance), warning = function(w) return(NA), error = function(e) return(NA)),
                    data    = tryCatch(purrr::map2(.data$data, .data$model, modelr::add_residuals), warning = function(w) return(.data$data), error = function(e) return(.data$data)), # add the residuals
                    data    = tryCatch(purrr::map2(.data$data, .data$model, modelr::add_predictions), warning = function(w) return(.data$data), error = function(e) return(.data$data)) # model is the column we created earlier, not the actual model itself
      )

    if( drop_raw ) {
      out <- out %>%
        dplyr::mutate(data = tryCatch(map(.data$data, ~ dplyr::select(.x, tidyselect::any_of(c(temperature_col, keep_data_cols)))), warning = function(w) return(.data$data), error = function(e) return(.data$data)))
    }
    ###### give this a more informative error message or when the model didn't converge
    #        Error : Problem with `mutate()` column `data`.
    # ℹ `data = map(data, ~select(.x, any_of(keep_data_cols)))`.
    # x no applicable method for 'select' applied to an object of class "NULL"
    # ℹ The error occurred in group 100: well = "E12".

    # pivot longer -- is cleaner in the downstream handling steps
    out <- out %>%
      dplyr::mutate(data  = tryCatch(purrr::map(.data$data,
                                                tidyr::pivot_longer,
                                                cols = tidyselect::any_of(keep_data_cols),
                                                names_to = "which_value",
                                                values_to = "value"),
                                     warning = function(w) return(.data$data),
                                     error = function(e) return(.data$data)
      )
      )

  }


#' Fit dsf data to a model
#'
#' @param df tibble containing data to fit
#' @param start_pars list of starting parameters, passed to nlsLM
#' @param formula model formula to fit, passed to nlsLM
#' @param lower_bound a vector of ower bounds of fit, passed to nlsLM
#' @param control_list list of controls for nlsLM. Defaults to list(maxiter = 500)
#'
#' @importFrom minpack.lm nlsLM
#'
#' @return fitted model, or, if it fails to converge, NA
#' @export
fit_nls <- # an updated version of compose_dsf_model, with a better name
  function(df,
           start_pars,
           formula,
           lower_bound,
           control_list = list(maxiter = 500)) {

    # minpack.lm nlsLM
    tryCatch(

      minpack.lm::nlsLM(formula,
                        data = df,
                        start = start_pars,
                        lower = lower_bound,
                        control = control_list),

      warning = function(w) return(NA), error = function(e) return(NA)

    )
  }





#' Add predictions for individual model components
#'
#' @param df nested tibble
#'
#' @return nested tibble with individual components added to the nested dataframes
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map map2
#'
#' @return the df, with component predictions added
#'
#' @export
add_component_predictions <-
  function(df) {
    # this should be updated so that the tidied column doesn't need
    out <-
      df %>%
      dplyr::mutate(tidied = purrr::map(.data$tidied, annotate_tidied_components),
                    data = purrr::map2(.data$tidied, .data$data, get_component_predictions))
  }

#' Annotate the components from the model parameter outputs
#'
#' @param tidied a tibble, the tidied model output
#'
#' @return the tidied model output, now with additional annotation columns
#'
#' @importFrom dplyr mutate if_else
#' @importFrom tidyr unite
#' @importFrom readr parse_number
#'
#' @return components, annotated extra columns
#'
#' @export
annotate_tidied_components <-
  function(tidied) {
    tryCatch({
      tidied %>% # the result of broom::tidy
        dplyr::mutate(component_number = parse_number(.data$term),
                      term_type = gsub(pattern = "([0-9]+).*$", replacement = "", .data$term),
                      component_type = if_else(.data$term_type %in% c("id_d", "id_b"),
                                               true = "initial",
                                               false = "sigmoid")) %>%
        unite("component", c(.data$component_type, .data$component_number), sep = "_", remove = FALSE)
    }, warning = function(w) {print("error in annotate_tidied_components")
      tidied},
    error = function(e) {print("error in annotate_tidied_components")
      tidied})
  }

#' Add predictions for each component to the dataframe
#'
#' @param tidied a tibble; the tidied model outputs
#' @param data the column containing nested data
#' @param temp_col name of the column containing temperature in data
#' @param rescale_temp_col normalize the temperature column?
#'
#' @importFrom scales rescale
#' @importFrom dplyr select mutate pull bind_rows
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom rlang ":=" "!!"
#'
#' @return data df, with predicts added
#'
#' @export
get_component_predictions <- # was add_component_predictions
  function(tidied,
           data,
           temp_col = "Temperature",
           rescale_temp_col = TRUE) {
    tryCatch({
      temp_vec <- data %>% pull({{ temp_col }})

      #### needs to be for temp norm
      if(rescale_temp_col) { temp_vec <- scales::rescale(temp_vec)}

      component_pred <-
        tidied %>%
        dplyr::select(.data$term, .data$term_type, .data$component_type, .data$component, .data$estimate) %>%
        dplyr::group_by(.data$component) %>%
        tidyr::nest() %>%
        dplyr::mutate(comp_pred = purrr::map(.data$data, predict_component, x_vec = temp_vec)) %>%
        dplyr::select(.data$component, .data$comp_pred) %>%
        tidyr::unnest(cols = c(.data$comp_pred)) %>%
        set_names(c("which_value", temp_col, "value"))  %>%
        mutate(!!temp_col := data %>% pull({{ temp_col }})) # replacing again with non-norm temperature

      out <- data %>%
        dplyr::bind_rows(component_pred)
    },
    error = function(e){print("error on pred")
      data},
    warning = function(w){print("warning on pred")
      data})
  }

#' Predict the values of a single component of a model
#'
#' @param df tibble with data
#' @param x_vec x vector to use for prediction.
#' @param ... does nothing yet; to pass extra external pars eventually
#'
#' @importFrom tibble tibble
#'
#' @return predicted components
#'
#' @export
predict_component <-
  function(df,
           x_vec = seq(from = 0, to = 1, length = 70),
           ... ) {

    component_type <- unique(df$component_type)
    to_eval <- get_component_exprs(component_type)

    par_list <- as.list(df$estimate)
    names(par_list) <- df$term_type
    par_list <- c(par_list,
                  "x_val" = list(x_vec))

    tibble::tibble(x = x_vec,
                   y = eval(to_eval, par_list))
  }


#' Get expressions for each omponent
#'
#' @param component_type the type of component
#'
#' @return quoted expressioned for each model component
#'
#' @export
get_component_exprs <-
  function(component_type) {
    switch(component_type,
           "sigmoid" = quote(Asym/(1 + exp((xmid - x_val)/scal)) * exp(d * (x_val - xmid))),
           "initial" = quote(id_d * exp(x_val * id_b)))
  }


utils::globalVariables(c("formulas"))
