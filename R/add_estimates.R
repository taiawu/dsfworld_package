#' Add estimates for key feautres in data
#'
#' A wrapper function to apply get_estimates across a tibble of nested dsf data, and append a column of estimates to that input tibble, named "est". "est" contains the output of get_estimates for each variable.
#'
#' @seealso \code{\link{tidy_for_tmas}}
#' @seealso \code{\link{get_estimates}}
#'
#' @param by_var A nested tibble, as output by tidy_for_tmas. Contains at least two columns; .var, a character column containing unique identifiers for each dataset, by which by_var is nested, and data, a nested tibble containing numeric columns corresponding to normalized temperature, value, and the first derivative of the normalized value. Default names are "Temperature_norm", "value_norm", and "drfu_norm", corresponding to the names output by tidy_for_tmas. However, alternative column names may used by supplying them in the ... argument. These names are passed to get_estimates.
#' @param ... additional named argument, which will be passed to the helper functions get_estimates, estimate, and tidy_estimates
#'
#' @return The input nested df, with an additional column named "est", containing a nested tibble of starting estimates, as created by tidy_estimates, for each row.
#'
#' @export
add_estimates <-
  function(by_var, ...) {
    by_var %>%
      dplyr::mutate(est = purrr::map(.data$data, get_estimates, ...))
  }


#' Estimate key values from in a single trace of DSF data.
#'
#' Estimate the values of key features in a single DSF dataset. Is also a helper function for add_estimates. Returns a tibble containing estimations of key values for each input dataset. Key values include the normalized temperatures at which transitions may occur in the data, and the normalized value of the initial readings. These values are used to generate starting parameter estimates for each dataset in add_start_pars. Estimates are included here (rather than just start pars) to enable users to extract their own starting parameters for their own models. Estimates for locations of transitions are rank-ordered by rank_estimates, to simplify passing to start parameters for downstream fitting.
#'
#' @seealso \code{\link{add_estimates}}
#'
#' @param df A tibble of DSF data containing numeric columns corresponding to temperature, normalized RFU, and first derivative of normalized RFU.
#' @param .est_peak_col name of the column, as a string, containing data in which to look for major peaks. Defaults to "drfu_norm".
#' @param .est_init_col name of the column, as a string, containing data in which to look for inital RFU values.  Defaults to "value_norm".
#' @param .norm_raw_x name of the column, as a string, containing normalizd temperature data. Defaults to "Temperature_norm".
#' @param .norm_raw_y name of the column, as a string, containing normalizd raw RFU data. Defaults to "value_norm".
#' @param ... addional named arguments, passed to estimate, rank_estimates, and tidy_estimates
#'
#' @return a tibble, containing columns val_at_est (the value of the data used to define the estimate at the estiamted point), est_val (value of the actual estimate itself, which is passed to get_start_pars), est_raw_x (raw x value at the estimated point), est_raw_y (raw y value at the estimated point), est_type (type of estimate, "transition" or "initial")

#' @export
get_estimates <-
  function(df,
           .est_peak_col = "drfu_norm",
           .est_init_col = "value_norm",
           .norm_raw_x = "Temperature_norm",
           .norm_raw_y = "value_norm",
           ...) {
    ##### how to we handle places where this fails??
    # probably in "estimate_transitions" function--correct?

    raw <-
      lapply(X = c("major", "minor", "initial"), # all three transtion types
             FUN = function(.x) {
               estimate_one(values = df %>% pull({{ .est_peak_col }}),
                        norm_raw_x = df %>% pull({{ .norm_raw_x }}),
                        norm_raw_y = df %>% pull({{ .norm_raw_y }}),
                        transition = .x,
                        ...)
             }) %>%
      dplyr::bind_rows()

    out <- tryCatch(rank_estimates(raw ,...), # clusering may fail
                    error = function(e) return(raw %>% arrange(.data$est_type, .data$est_rank) %>% mutate(est_rank = row_number())), # make unique est_ranks
                    warning = function(w) return(raw %>% arrange(.data$est_type, .data$est_rank) %>% mutate(est_rank = row_number()))) # make unique est_ranks
  }

#' Estimate a key value (either transition or starting RFU) for a single DSF dataset
#'
#' A helper function used inside of get_estimates to identify estimates for a single type of value. Types of values include: "major" and "minor" transitions, and "initial" normalized RFU values.
#'
#' @param values numeric vector containing the values from which the estimates for transitions should be drawn. For DSF applications, should be the first derivative of normalized raw RFU data for "major" and "minor" transition estimates, and normalized raw RFU data for "initial" RFU estimates.
#' @param norm_raw_y numeric vector containing normalized raw data, from which estimates for initial RFU values will be drawn. Is provided in the tidy_estimates output regardless of estimate type, to assist with downstream plotting and assessment of estimates.
#' @param norm_raw_x numeric vector containing the x values corresponding to each measurement. For DSF aplications, should be normalized temperature. Is provided in the tidy_estimates output regardless of estimate type, to assist with downstream plotting and assessment of estimates.
#' @param transition the type of transition to estimate--major transition, minor transtion, or initial value. Defaults to "major".
#' @param low_temp_margin a number, setting the ealiest measurement for which a transition estimate can be made. Defaults to 5, which drops all estimated transitions within the first five measurements. Ensures that estimates are not provided for transitions too close to the edge of the measured region to be reasonably extracted using the downstream model fitting.
#' @param high_temp_margin a number, setting the latest measurement for which a transition estimate can be made. Defaults to 5, which drops all estimated transitions within the last five measurements. Ensures that estimates are not provided for transitions too close to the edge of the measured region to be reasonably extracted using the downstream model fitting.
#' @param major_peak_min a number, defining the minimum dRFU value for which any "major" transition will be returned. Reduces the number of transitions returned for insignificantly small local maxima. The appropriateness of the default value is dependent on the input data being normalied 0 to 1. DSF data containing unusually large or small temperature windows, such that the slope of the normalized value vs normalized temperature data is unusual, should also use the default value with caution.
#' @param major_peak_max a number, defining the maximum dRFU value for any "major" transition will be returned. Provides additional protection for sharp peaks resulting from noise, beyond what is provided by the quantmod package. The appropriateness of the default value is dependent on the input data being normalied 0 to 1. DSF data containing unusually large or small temperature windows, such that the slope of the normalized value vs normalized temperature data is unusual, should also use the default value with caution. Extremely small measured Temperature ranges (e.g. << 70 degrees) may benefit from a decreased value. Extremely large measured Temperatures ranges (e.g. >> 70 degrees) may benefit from an increased value. Because estimates and fitting in dsfworld are performed using normalized temperatures by default, the appropriateness of this estimate depends only on the measured temperature range, not the the number of measurements performed per degree. Robustness to varied numbers of measurements per degree is the primary rationale for the default use of normalized temperatures, rather than raw temperatures, for dsfworld fitting.
#' @param minor_peak_max a number, defining the maximum value in the triple derivative from which a minor transition can be returned. The appropriateness of the default value is dependent on the input data being normalied 0 to 1. DSF data containing unusually large or small temperature windows, such that the slope of the normalized value vs normalized temperature data is unusual, should also use the default value with caution.
#' @param loess_span a number, passed to stats::loess(), as in stats::loess(span = loess_span). Loess is used to smooth the input data prior to estimation.
#' @param ... additional arguments, passed to tidy_estimates.
#'
#' @return Estimates found for a single transition type ("major", "minor", or "initial"), as output by tidy_estimates.
#' @seealso \code{\link{tidy_estimates}}
#'
#' @export
estimate_one <-
  function( values,
            norm_raw_y,
            norm_raw_x,
            transition = "major",
            low_temp_margin = 5,
            high_temp_margin = 5,
            major_peak_min = 0.01,
            major_peak_max = 10,
            minor_peak_max = 0.001,
            loess_span = 0.1,
            ...) {

    norm_length <- scales::rescale(c(0:(length(values)-1)))

    use_val <- switch(transition,
                      "major" = values,
                      "minor" = values %>% sgolay(m = 2),
                      "initial" = norm_raw_y)


    find_val_f <- switch(transition,
                         "major" = quantmod::findPeaks,
                         "minor" = function(x) {quantmod::findValleys(abs(x)) },
                         "initial" = function(x) {mean(x[c(1:3)])})


    val_range <- switch(transition,
                        "major" = c(major_peak_min, major_peak_max),
                        "minor" = c(-abs(minor_peak_max), abs(minor_peak_max)),
                        "initial" = c(-1, 2))

    points <-
      stats::loess(use_val ~ norm_length,
                   span = loess_span) %>%
      stats::predict() %>%
      find_val_f()

    out <-
      tidy_estimates(points = points,
                 values = use_val,
                 norm_raw_x = norm_raw_x,
                 norm_raw_y = norm_raw_y,
                 transition = transition,
                 min_val_at_est = min(val_range)[[1]],
                 max_val_at_est = max(val_range)[[1]],
                 ...) %>%
      dplyr::filter(.data$est_row > low_temp_margin,
                    .data$est_row < (length(values) - high_temp_margin)) %>%
      dplyr::mutate(est_rank = dplyr::dense_rank(.data$val_at_est))
  }

#' Clean and format estimates for get_estimates.
#'
#' This may or may not be exported in the final package.
#'
#'
#' A helper function for get_estimates. Tidies the output of estimates into a 7-column tibble containing all information necessary to pass estimates unambiguously to starting parameters, and map the estimated value back to the data from which it was calculated.
#'
#' @param points a numeric vector, containing positions in the input data vector which correspond to estimated values .
#' @param values a numeric vector, containing the data from which the estimate was calculated (first derivative of normalized raw data for "major" transition estimates, third derivative of normalized raw data for "minor" transition estimates, and normalized raw data for "initial" estimates.
#' )
#' @param norm_raw_x numeric vector containing raw normalizedxy values for the estimated data. For dsfworld applications, is normalized temperature (typically named "Temperature_norm").
#' @param norm_raw_y numeric vector containing raw normalized y values for the estimated data. For dsfworld applications, is normalized RFU data (typically named "value_norm").
#' @param transition a string, describing the type of value estimated: "major", "minor" or "initial"
#' @param min_val_at_est lower threshhold for the magnitude of the y value at an estimate, as contained in the provided "value" arugment. Estiamtes which do not meet this criteria are removed.
#' @param max_val_at_est upper threshhold for the magnitude of the y value at an estimate, as contained in the provided "value" arugment. Estiamtes which do not meet this criteria are removed.
#' @param default_point_norm numeric vector with numbers ranging 0 to 1.If fewer estimated transitions are returned than will be needed to determine starting parameters in the downstream model fitting, default positons in the x vector from which transitions shoudl be estimated. Defaults to c(0.4, 0.6, 0.2), which returns the placed 40%, 60%, and 20% of the way through the Temperature_norm vector (e.g. for a standard 25- 94 C run, corresponds to starting parameter estimates of 53, 67, and 39 C).
#'
#' @param ... additional arguments, inherited from upstream functions such as add_estimates, get_estimates, or estimate_one, which can be ignored by this function.
#'
#' @return Estimates found for a single transition type ("major", "minor", or "initial").
#'
#' #' @format
#' \describe{ A tibble containing 7 columns.
#'   \item{est_row}{the measurement number of the estimated value. type: numeric.}
#'   \item{val_at_est}{ the value of the vector use to identify the estimate, at the estimated value. type: numeric. Helpful for plotting estimates, and for any user wanting to perform teir own rank-ordering of esimates for passing to starting parameters.}
#'   \item{est_val}{ the value of the estimate itself. Is a normalized temperature for "major" and "minor" estimates, and a normalized raw RFU value for "initial". This value is used as the starting parameter.}
#'   \item{est_raw_x}{the x value corresponding to the estimated point. Is a normalized temperature for dsfworld applications. Helpful for plotting estimates, and for any user wanting to perform teir own rank-ordering of esimates for passing to starting parameters.}
#'   \item{est_raw_y}{the normalized raw y value corresponding to the estimated point. Is normalized RFU for dsfworld applications. Helpful for plotting estimates, and for any user wanting to perform teir own rank-ordering of esimates for passing to starting parameters.}
#'   \item{est_type}{the type of transition estimates: "major", "minor" or "initial". Used to match estimates with their apporpriate starting parameters in add_start_pars.}
#'   \item{est_rank}{an integer, containing the rank of predicted "importance" for major and minor transitions. Used in the extremely common case where the number of estimated transitions exceed the number of transitions in the fitted model. Ensures that the transitions likely to be most important are passed to starting parameters.}
#' }
#'
#' @export
tidy_estimates <-
  function(points,
           values,
           norm_raw_x,
           norm_raw_y,
           transition,
           min_val_at_est,
           max_val_at_est,
           default_point_norm = c(0.4, 0.6, 0.2),
           ...
  ) {

    if(is.null(points)) {
      print("points are null")
      pos <-
        default_point_norm %>%
        sapply( X = .,
                FUN = function(x, values) {
                  if(x > 1) { x <- 0.5 }
                  as.integer(length(values)*x)
                }, values = values)


      points <- c(1:length(values))[pos]
    }

    if(transition == "initial") {
      points <- c(1) }

    out <-
      tibble::tibble(est_row = points,
                     val_at_est =  values[points], # the value from drfu
                     est_val = norm_raw_x[points], # the value passed to par
                     est_raw_x = norm_raw_x[points],
                     est_raw_y = norm_raw_y[points],
                     est_type = transition) %>%
      filter(.data$val_at_est > min_val_at_est,
             .data$val_at_est < max_val_at_est)
  }
