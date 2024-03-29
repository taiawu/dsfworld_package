% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_dye_screen_model_plots.R
\name{extract_model_tmas}
\alias{extract_model_tmas}
\title{Extract model tmas}
\usage{
extract_model_tmas(
  by_var_fit,
  .var_col = ".var",
  .model_name_col = "model_name",
  .model_tma_col = "model_tma"
)
}
\arguments{
\item{by_var_fit}{a tibble, as output by \code{add_model_fits}.}

\item{.var_col}{a string, giving the name of the column in \code{by_var_fit} containing the unique-identifier for each dataset. Defaults to ".var"}

\item{.model_name_col}{a string, giving the name of the column in \code{by_var_fit} containing the name of the fitted model used to find a given tma. Defaults to "model_name"}

\item{.model_tma_col}{a string, giving the name of the column in \code{by_var_fit} containing the tma resulting from the model fitting and tma extraction. Defaults to "model_tma".}
}
\value{
a tibble of apparent melting temperatures, comprising four columns
\itemize{
\item \strong{.var} character, giving the unique dataset identifier for each DSF trace.
\item \strong{model_name} character, giving the name of the model from which the given values and tmas were found, e.g. "model_1", "model_2"
\item \strong{which_value} character, giving the specific component for a given value, e.g. "sigmoid_1", "sigmoid_2"
\item \strong{tma} numeric, the value of the calculated tma in reference to normalized Temperatures.
}
}
\description{
Starting from a tibble containing dsfworld fit information, use \code{extract_model_tmas} to pull just resulting tmas into a tidy tibble. Probably belongs somewhere in the fitting function scripts, ultimately.
}
