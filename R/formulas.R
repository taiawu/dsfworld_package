#' Formulas and defaults for dsfworld models
#'
#' @description A list of formulas and default values. This is used internally by the fitting functions. It is expsoed externally for users interested in using these formulas in their own fitting workflows.
#'
#' @format  A named list containing one element for each of the six standard dsfworld models, "model_1", "model_2", "model_3", "model_4", "model_5", "model_6". Each element (e.g. "model_1"), contains the following
#' \describe{
#'   \item{formula}{The formula for the model, which can be passed directly to fitting functions such as nls.LM. Class: 'formula'}
#'   \item{pars}{ A named list containing default starting values for all parameters in the formula. Some of these are over-written with per-dataset calculated starting parameters in the fitting workflow.}
#'   \item{lower_bound}{A named number vector containing numeric lower bounds for each parameter in the formula, with the names corresponding to the parameter name. The parameters appear in the same order in both this object and the pars object..}
#' }

"formulas"
