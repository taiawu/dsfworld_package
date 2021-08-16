###### ---- given drfu data, estimates major and minor transitions, and returns them ordered by their magnitude
#' Estimate locations of transitions in data
#'
#' @param values numeric vector containing a single raw dataset
#' @param norm_raw_y numeric vector containing normalized raw rfu data
#' @param norm_raw_x numeric vector containing normalized temperature data
#' @param transition which transition to detect. options: "major" (peaks in drfu), "minor" (valleys in ddrfu), "initial" (the starting rfu value)
#' @param low_temp_margin do not take any values in the first x measurements  (e.g. don't take a peak if it's the first value)
#' @param high_temp_margin do not take any values after the first x measurement (e.g. don't take a peak if it's the last value)
#'
#' @return tibble containing potential transition locations
#'
#' @importFrom stats loess predict
#' @importFrom quantmod findPeaks findValleys
#' @importFrom tibble tibble
#' @importFrom dplyr arrange desc mutate
#'
#' @export
estimate_transitions <-
  function( values,
            norm_raw_y,
            norm_raw_x,
            transition = "major",
            low_temp_margin = 5,
            high_temp_margin = 95) {

    # secondary dependency: signal sgolayfilt (from sgolay function)

    # smooth the input vector ### is this really necessary?
    norm_length <- c(1:length(values))/length(values)
    interp <- stats::loess( values ~ norm_length, span = 0.1) %>%  stats::predict()

    if (transition == "major") {
      points <- interp %>% quantmod::findPeaks()

    } else if (transition == "minor") {
      dval <- values %>% sgolay(m = 1)
      interp <- loess( dval ~ norm_length, span = 0.1) %>%  predict()

      points <- interp %>% abs() %>% quantmod::findValleys()

    } else if (transition == "initial") {

      init_mean <- norm_raw_y[c(1:3)] %>% mean()

      out <-
        tibble::tibble(est_row = 1,
                       val_at_est =  init_mean,
                       est_val = init_mean,
                       est_raw_x = norm_raw_x[1],
                       est_raw_y = norm_raw_y[1],
                       est_type = transition) %>%
        dplyr::arrange(dplyr::desc(.data$val_at_est)) %>%
        dplyr::mutate(est_rank = c(1:nrow(.)))

      return(out) ###### RETURNS HERE

    } else {
      print("Invalid transition type requested; returning major transition")
      points <- interp %>% quantmod::findPeaks()
    }

    # correct for indexing
    points <- points - 1

    # return peaks, removing ones which fall outside the legitimate tma range
    points[points > low_temp_margin | points > high_temp_margin]

    out <-
      tibble::tibble(est_row = points,
                     val_at_est =  values[points], # the value from drfu
                     est_val = norm_raw_x[points], # the value passed to par
                     est_raw_x = norm_raw_x[points],
                     est_raw_y = norm_raw_y[points],
                     est_type = transition) %>%
      dplyr::arrange(dplyr::desc(.data$val_at_est)) %>%
      dplyr::mutate(est_rank = c(1:nrow(.)))
  }


#' Add locations of estimated transitions to dataframe
#'
#' @param df nested tibble of dsf data
#' @param .est_peak_col name of column from which to estimate peaks defaults to "drfu_norm"
#' @param .est_init_col name of column from which to estimate inital rfu. defaults to "value_norm"
#' @param .norm_raw_x name of column containing normalized temperature. defaults to "Temperature_norm"
#' @param .norm_raw_y name of column containing normalized raw RFU. defaults to "value_norm"
#'
#' @return df, with a new column with estimations
#'
#' @importFrom dplyr bind_rows
#'
#' @export
add_estimates <-
  function(df,
           .est_peak_col = "drfu_norm",
           .est_init_col = "value_norm",
           .norm_raw_x = "Temperature_norm",
           .norm_raw_y = "value_norm") {
    ##### how to we handle places where this fails??
    # probably in "estimate_transitions" function--correct?

    # dplyr bind_rows
    out <-
      lapply(X = c("major", "minor", "initial"), # all three transtion types
             FUN = function(.x) {
               estimate_transitions(values = df %>% pull({{ .est_peak_col }}),
                                    norm_raw_x = df %>% pull({{ .norm_raw_x }}),
                                    norm_raw_y = df %>% pull({{ .norm_raw_y }}),
                                    transition = .x)
             }) %>%
      dplyr::bind_rows()
  }

#' Select and format estimates for model starting parameters
#'
#' @param estimates a tibble containing all estimates
#' @param which_pars names of the parameters to get starting estimates for
#' @param pars_defaults default values for all parameters
#' @param par_order order in which nlsLM will expect the parameters in the downstream funtions
#'
#' @return parameter list for passing to fitting function (uses nlsLM)
#'
#' @importFrom dplyr mutate if_else filter bind_rows arrange
#' @importFrom tidyr unite
#' @importFrom tibble tibble
#' @importFrom purrr as_vector
#'
#' @export
pars_from_estimates <- #__translate estimates df to pars list for nlsLM
  function(estimates, # df with all estimate information
           which_pars, # all parameters desired
           pars_defaults = list(Asym1 = 1, xmid1 = 0.4, scal1 = 0.03, d1 = -0.5,
                                Asym2 = 0.5,  xmid2 = 0.6, scal2 = 0.03,  d2 = -1,
                                Asym3 = 0.3, xmid3 = 0.2, scal3 = 0.03, d3 = -1.5,
                                id_d1 = 0.2, id_b1 = -5), # not everything gets an estimate
           par_order = c("Asym1", "xmid1", "scal1", "d1", # par order must match final formula
                         "Asym2", "xmid2", "scal2", "d2",
                         "Asym3", "xmid3", "scal3", "d3",
                         "id_d1", "id_b1")) {

    ##### work to do on this function ########
    # intelligent estimate of Asym from data?
    # improved ordering of xmid values -- when to use minor or major transtions?

    # dplyr mutate if_else filter bind_rows arrange
    # tidyr unite
    # tibble tibble
    # purrr as_vector

    ## match estimates to par names
    par_df <- # keep as intermediate - useful for additional intelligent ordering
      estimates %>%
      # match parameter names to forumula
      dplyr::mutate(par_type =
                      dplyr::if_else(.data$est_type %in% c("minor", "major"),
                                     true = "xmid",
                                     false = "id_d")) %>%
      tidyr::unite("par_name", c(.data$par_type, .data$est_rank),
                   sep = "", remove = FALSE) %>%

      # drop pars irrelevant or redundant to given formula
      dplyr::filter(.data$est_type != "minor",  ### CURRENTLY IGNORES MINOR TRANSITIONS!!!!!!
                    .data$par_name %in% which_pars)

    ## create the final parameter list
    defaults <-
      tibble::tibble("par_name" = names(pars_defaults),
                     "est_val" =  pars_defaults %>% purrr::as_vector()) %>%
      dplyr::filter(.data$par_name %in% which_pars, # drop pars not in the model
                    ! .data$par_name %in% par_df$par_name) # drop pars supplied by estimates

    #-----PARAMETER LIST ORDER HAS TO MATCH FORMULA -----#
    df <- # combine estimated and default pars
      dplyr::bind_rows(par_df, defaults) %>%
      dplyr::mutate("par_f" = factor(.data$par_name, levels = par_order)) %>%
      dplyr::arrange(.data$par_f)

    #### HERE -- ADD A CHECK TO REPLACE THE ESTIMATES WITH THE DEFAULTS IF THEY ARE WHACKY ###

    ## nlsLM expects a list
    out <- as.list(df$est_val)
    names(out) <- df$par_name
    out # can be fed directly to nlsLM as start pars

  }
