% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_dye_screen_model_plots.R
\name{tidy_for_plot}
\alias{tidy_for_plot}
\title{Tidy model data for plotting}
\usage{
tidy_for_plot(
  tidied_for_tmas,
  by_var_fit,
  layout,
  .var_col = ".var",
  .model_name_col = "model_name",
  .hit_filt_col = "dye_channel_f",
  .hit_filt_components = c("dye", "channel_f"),
  ...
)
}
\arguments{
\item{tidied_for_tmas}{a tibble of raw data, as returned by \code{tidy_for_tmas}}

\item{by_var_fit}{a tibble of fit results, as returned by \code{add_model_fits}}

\item{layout}{a tibble of plate layout information, as returned by \code{read_plate_layout} or \code{extract_plate_layout}}

\item{.var_col}{a string, giving the name of the column in both \code{tidy_for_tmas} and \code{by_var_fit} giving identical unqiue identifiers fo a given dataset. Defaults to ".var"}

\item{.model_name_col}{a string, giving the name of column in \code{by_var_fit} which gives the name of the fitted model, e.g. "model_1". Defaults to "model_name".}

\item{.hit_filt_col}{a string, giving the name of the column which is created here through a \code{unite()} operation, and later matched to hit calls. Defaults to "dye_channel_f"}

\item{.hit_filt_components}{a character vector of length 2, giving the columns in \code{tidied_for_tmas} to join (in the given order) to create the column which will bear the name given in \code{.hit_filt_col}, with an underscore separator. Defaults to c("dye", "channel_f").}

\item{...}{additional arguments, not passed to anything internal here, but allows for the function to recieve named arguments from upstream functions while ignoring arguments passed with ... which match nothing in this function.}
}
\value{
a tibble, rady for passing to downstream plotting functions.
}
\description{
Ultimately, may not export this function.
}
\details{
Use \code{tidy_for_plot} to prepare raw and predicted data for plotting.
}
