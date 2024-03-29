% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_model_fits.R
\name{get_model_fits}
\alias{get_model_fits}
\title{Fit a single model to a nested tibble contaiing multiple traces of DSF data.}
\usage{
get_model_fits(by_var, ...)
}
\arguments{
\item{by_var}{A nested tibble, as output by tidy_for_tmas. Contains at least two columns; .var, a character column containing unique identifiers for each dataset, by which by_var is nested, and data, a nested tibble containing numeric columns corresponding to normalized temperature, value, and the first derivative of the normalized value. Default names are "Temperature_norm", "value_norm", and "drfu_norm", corresponding to the names output by tidy_for_tmas. However, alternative column names may used by supplying them in the ... argument. These names are passed to get_estimates.}

\item{...}{additional arguments, passed to the downstream functions: add_estimates, add_start_pars, add_nls, add_model_stats, add_model_preds, add_model_tmas.}
}
\value{
A nested tibble, with additional columns corresponding to the results of the model fitting.
#' @format A tibble with columns
\describe{
\item{.var}{A character containing unique identifier for each dataset. Typically this is well names.}
\item{model_name}{A character, containing the name of the fitted model as a string. e.g. "model_1"}
\item{data}{A tibble containing normalized Temperature data, and resulting y value datasets: model predictions, model residuals, and individual model components.}
\item{est}{A tibble of estimated key values in the input data, as produced by add_estimates()}
\item{pars}{A named list of starting parameters to be passed to the model fitting, as produced by add_start_pars()}
\item{model}{A fitted model, as produced by add_nls()}
\item{tidied}{A tibble containing the final parameters for the fitted model. Contains additional columns with annotations used to reconstruct individual model components}
\item{glanced}{A tibble containing one-line summarys statistics for the fitted model, as output by broom::glance}
\item{model_tma}{Atibble containing the apparent melting temperatures of each reconstructed sigmoid component from the fitted model, as determined by get_drfu_tmas()}
}
}
\description{
Fit a single model to a nested tibble contaiing multiple traces of DSF data.
}
