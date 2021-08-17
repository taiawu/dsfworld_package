#' add starting parameters
#'
#' @param by_var do this
#' @param ... do this
#'
#' @return do this
#' @export
add_start_pars <-
  function(by_var, ...) {
    by_var %>%
      mutate(pars =
               purrr::map(.data$est,
                          get_start_pars,
                          ...)[1])
  }

# --------- get start pars
#' Select and format estimates for model starting parameters
#'
#' @param estimates a tibble containing all estimates
#' @param which_pars names of the parameters to get starting estimates for
#' @param pars_defaults default values for all parameters
#' @param par_order order in which nlsLM will expect the parameters in the downstream funtions
#' @param ... do this
#'
#' @return parameter list for passing to fitting function (uses nlsLM)
#'
#' @importFrom dplyr mutate if_else filter bind_rows arrange
#' @importFrom tidyr unite
#' @importFrom tibble tibble
#' @importFrom purrr as_vector
#'
#' @export
get_start_pars <- #__translate estimates df to pars list for nlsLM
  function(estimates, # df with all estimate information
           which_pars, # all parameters desired
           pars_defaults = list(Asym1 = 1, xmid1 = 0.4, scal1 = 0.03, d1 = -0.5,
                                Asym2 = 0.5,  xmid2 = 0.6, scal2 = 0.03,  d2 = -1,
                                Asym3 = 0.3, xmid3 = 0.2, scal3 = 0.03, d3 = -1.5,
                                id_d1 = 0.2, id_b1 = -5), # not everything gets an estimate
           par_order = c("Asym1", "xmid1", "scal1", "d1", # par order must match final formula
                         "Asym2", "xmid2", "scal2", "d2",
                         "Asym3", "xmid3", "scal3", "d3",
                         "id_d1", "id_b1"),
           ...) {

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
      dplyr::filter(.data$par_name %in% which_pars)

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
