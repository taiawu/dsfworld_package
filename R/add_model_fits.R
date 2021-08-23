
#' Add fits and associated apparent melting temperatures for a list of models to nested tibbel
#'
#' A wrapper function for get_model_fits, which performs the entire fitting workflow for a single model to a nested tibble of dsf data, from starting parameter estimation to extraction of tmas for individaul model components.
#'
#' @param by_var A nested tibble, as output by tidy_for_tmas. Contains at least two columns; .var, a character column containing unique identifiers for each dataset, by which by_var is nested, and data, a nested tibble containing numeric columns corresponding to normalized temperature, value, and the first derivative of the normalized value. Default names are "Temperature_norm", "value_norm", and "drfu_norm", corresponding to the names output by tidy_for_tmas. However, alternative column names may used by supplying them in the ... argument. These names are passed to get_estimates.
#' @param which_models the names of the models from the set of dsfworld models, to be fit. Model options are: "model_1", "model_2" . . . to "model_6"
#' @param dsfworld_models a boolean. If TRUE, indicates that fits will be performed to the provided dsfworld models. If false, all model information must be provided by the user. I haven't worked through an example of what this would actually look like in the full workflow, so I suspect that this wrapper function wouldn't adapt seamlessly to an external model without at least some minor adaptations.
#' @param model_pars an argument required only if the user wants to provide their own model, rather than using the dsfworld models.
#' @param model_formula an argument required only if the user wants to provide their own model, rather than using the dsfworld models.
#' @param model_lower_bound an argument required only if the user wants to provide their own model, rather than using the dsfworld models.
#' @param ... additional arguments, passed to the downstream functions: add_estimates, add_start_pars, add_nls, add_model_stats, add_model_preds, add_model_tmas.
#'
#'
#' @return A nested tibble, as described by get_model_fits. Each model gets it's own row.
#'
#' @export
add_model_fits <-
  function(by_var,
           which_models, # options c("model_1", "model_2", "model_3", "model_4", "model_5", "model_6")
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

    out <-which_models %>%
      lapply( . , function(x) {
        get_model_fits(by_var,
                       which_pars =  formulas[[x]]$pars %>% names(),
                       formula = formulas[[x]]$formula,
                       lower_bound = formulas[[x]]$lower_bound,
                       ...) %>%
          dplyr::mutate(model_name = x) %>%
          dplyr::relocate(.data$.var, .data$model_name)
      }) %>%
      dplyr::bind_rows()

  }



#' Fit a single model to a nested tibble contaiing multiple traces of DSF data.
#'
#' @param by_var A nested tibble, as output by tidy_for_tmas. Contains at least two columns; .var, a character column containing unique identifiers for each dataset, by which by_var is nested, and data, a nested tibble containing numeric columns corresponding to normalized temperature, value, and the first derivative of the normalized value. Default names are "Temperature_norm", "value_norm", and "drfu_norm", corresponding to the names output by tidy_for_tmas. However, alternative column names may used by supplying them in the ... argument. These names are passed to get_estimates.
#' @param ... additional arguments, passed to the downstream functions: add_estimates, add_start_pars, add_nls, add_model_stats, add_model_preds, add_model_tmas.
#'
#' @return A nested tibble, with additional columns corresponding to the results of the model fitting.
#' #' @format A tibble with columns
#' \describe{
#'   \item{.var}{A character containing unique identifier for each dataset. Typically this is well names.}
#'   \item{model_name}{A character, containing the name of the fitted model as a string. e.g. "model_1"}
#'   \item{data}{A tibble containing normalized Temperature data, and resulting y value datasets: model predictions, model residuals, and individual model components.}
#'   \item{est}{A tibble of estimated key values in the input data, as produced by add_estimates()}
#'   \item{pars}{A named list of starting parameters to be passed to the model fitting, as produced by add_start_pars()}
#'   \item{model}{A fitted model, as produced by add_nls()}
#'   \item{tidied}{A tibble containing the final parameters for the fitted model. Contains additional columns with annotations used to reconstruct individual model components}
#'   \item{glanced}{A tibble containing one-line summarys statistics for the fitted model, as output by broom::glance}
#'   \item{model_tma}{Atibble containing the apparent melting temperatures of each reconstructed sigmoid component from the fitted model, as determined by get_drfu_tmas()}
#'    }
#'
#' @export
get_model_fits <-
  function(by_var,
           ...) {
    out <-
      by_var %>%
      add_estimates(., ...) %>%
      add_start_pars(.,  ...) %>%
      add_nls(., ...) %>%
      add_model_stats(., ...) %>%
      add_model_preds(., ...)  %>%
      add_model_tmas(.,  .x_vec = "Temperature_norm")

  }

utils::globalVariables(c("formulas"))

