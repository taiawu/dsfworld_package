% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_estimates.R
\name{tidy_estimates}
\alias{tidy_estimates}
\title{Clean and format estimates for get_estimates.}
\usage{
tidy_estimates(
  points,
  values,
  norm_raw_x,
  norm_raw_y,
  transition,
  min_val_at_est,
  max_val_at_est,
  default_point_norm = c(0.4, 0.6, 0.2),
  ...
)
}
\arguments{
\item{points}{a numeric vector, containing positions in the input data vector which correspond to estimated values .}

\item{values}{a numeric vector, containing the data from which the estimate was calculated (first derivative of normalized raw data for "major" transition estimates, third derivative of normalized raw data for "minor" transition estimates, and normalized raw data for "initial" estimates.
)}

\item{norm_raw_x}{numeric vector containing raw normalizedxy values for the estimated data. For dsfworld applications, is normalized temperature (typically named "Temperature_norm").}

\item{norm_raw_y}{numeric vector containing raw normalized y values for the estimated data. For dsfworld applications, is normalized RFU data (typically named "value_norm").}

\item{transition}{a string, describing the type of value estimated: "major", "minor" or "initial"}

\item{min_val_at_est}{lower threshhold for the magnitude of the y value at an estimate, as contained in the provided "value" arugment. Estiamtes which do not meet this criteria are removed.}

\item{max_val_at_est}{upper threshhold for the magnitude of the y value at an estimate, as contained in the provided "value" arugment. Estiamtes which do not meet this criteria are removed.}

\item{default_point_norm}{numeric vector with numbers ranging 0 to 1.If fewer estimated transitions are returned than will be needed to determine starting parameters in the downstream model fitting, default positons in the x vector from which transitions shoudl be estimated. Defaults to c(0.4, 0.6, 0.2), which returns the placed 40\%, 60\%, and 20\% of the way through the Temperature_norm vector (e.g. for a standard 25- 94 C run, corresponds to starting parameter estimates of 53, 67, and 39 C).}

\item{...}{additional arguments, inherited from upstream functions such as add_estimates, get_estimates, or estimate_one, which can be ignored by this function.}
}
\value{
Estimates found for a single transition type ("major", "minor", or "initial").

#' @format
\describe{ A tibble containing 7 columns.
\item{est_row}{the measurement number of the estimated value. type: numeric.}
\item{val_at_est}{ the value of the vector use to identify the estimate, at the estimated value. type: numeric. Helpful for plotting estimates, and for any user wanting to perform teir own rank-ordering of esimates for passing to starting parameters.}
\item{est_val}{ the value of the estimate itself. Is a normalized temperature for "major" and "minor" estimates, and a normalized raw RFU value for "initial". This value is used as the starting parameter.}
\item{est_raw_x}{the x value corresponding to the estimated point. Is a normalized temperature for dsfworld applications. Helpful for plotting estimates, and for any user wanting to perform teir own rank-ordering of esimates for passing to starting parameters.}
\item{est_raw_y}{the normalized raw y value corresponding to the estimated point. Is normalized RFU for dsfworld applications. Helpful for plotting estimates, and for any user wanting to perform teir own rank-ordering of esimates for passing to starting parameters.}
\item{est_type}{the type of transition estimates: "major", "minor" or "initial". Used to match estimates with their apporpriate starting parameters in add_start_pars.}
\item{est_rank}{an integer, containing the rank of predicted "importance" for major and minor transitions. Used in the extremely common case where the number of estimated transitions exceed the number of transitions in the fitted model. Ensures that the transitions likely to be most important are passed to starting parameters.}
}
}
\description{
This may or may not be exported in the final package.
}
\details{
A helper function for get_estimates. Tidies the output of estimates into a 7-column tibble containing all information necessary to pass estimates unambiguously to starting parameters, and map the estimated value back to the data from which it was calculated.
}
