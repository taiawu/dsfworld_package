#' Wrapper for Savistky-Golay filtering
#'
#' A closure which sets defaults frequently appropriate for DSF for p (filter order) and n (filter length). Allows the filter to be called with the simplier syntax later. e.g. sgolay(value, 1) returns the first derivative of the vector 'value'. Formals for sgolay and be reset after calculations performed on user data to determine ideal p and n. This behavior is not implemented yet.
#'
#'Savistky-Golay filtering is used for all smoothing and derivative calculations in dsfworld.
#'
#' @param x a numeric vector to be savitsky-golay filtered
#' @param p the filter order to use. Defaults to 5. Must be odd and >2.
#' @param n the number of measurements in the filter window. Defaults to 13.
#' @param m the filter order. 0: smoothing only; 1: first derivative, 2: second derivative, etc.
#'
#' @return a numeric vector, with a savistky-golay filter applied.
#'
#' @importFrom signal sgolayfilt
#'
#' @export
sgolay <- function(x, p, n, m) {signal::sgolayfilt(x, p = 5, n = 13, m)}


#' Generate helpful error messages for invalid function inputs
#'
#' This function appears in Advanced R Chapter 8.5.2 Signalling
#' \url{https://adv-r.hadley.nz/conditions.html#signalling}
#' It was presumably written by Hadley Wickham.
#' Generates a helpful error message and passes it to rlang::abort. Does not determine argument validity. Use this function inside a statement which determines that an input is invalid. Is adapted with extremely minor modifications from
#'
#'
#' @param arg the invalid function argument
#' @param must what a valid argument must be (e.g "numeric", or "all_models")
#' @param not what a valid argument cannot be (e.g. "character" or "adlkfm_modelz")
#'
#' @return an error. prints the custom message to the console.
#'
#' @importFrom glue glue
#' @importFrom rlang abort
#'
#' @export
abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    msg <- glue::glue("{msg}; not `{not}`.")
  }

  rlang::abort("error_bad_argument",
               message = msg,
               arg = arg,
               must = must,
               not = not
  )
}

#' Extract layout from labeled plate data
#'
#' Ultimately, probably belongs in utils.R or the like. Use \code{extract_plate_layout} to generate a plate layout object from labeled data.
#'
#' @param data a tibble containing labeled dsf data
#' @param .auto_detect a boolean. If TRUE, attempts to automatically identify which columns contain layout information based on the number of unique values provided per well (layout columns have only one value per well; data columns have one per measurement). Defaults to TRUE.
#' @param .extract_well a boolean. If TRUE, extracts the "well" component from a column which contains well, e.g. "A1_FAM", will extract "A1"
#' @param .extract_well_into a character vector, passed directly to the "into" argument of tidyr::separate, if .extract_well is TRUE. Gives the names of the pieces into which the column named in the .var_col argument will be separated. Defaults to c("well", "channel_from_var", "type_from_var"). The elements other than 'well' in .extract_well_into will be dropped from the final layout. A warning is passed if columns of this name existed in input and will therefore also be missing from the resulting layout.
#' @param .extract_sep a string, passed directl yto the "sep" argument to tidyr::separate, if .extract_well is TRUE. Gives the separator between components; defaults to "_"
#' @param .var_col a string, giving the name of the column in the provided \code{data} containing unqiue trace identifiers. Defaults to "variable". This column is used to get well names, if .extract_well_into is TRUE.
#' @param .well_col a string, giving the name of the column containing well names, if any.
#' @param .keep_manual a characer vector which, if if .auto_detect = FALSE, gives the names of all column (other than well), which are to be kept and put in the final layout.
#' @param .respect_grouping a boolean, defaults to FALSE, on whether or not to respect grouping of input \code{data}. Defaults to FALSE.
#' @param ... additional named arguments, not passed to anything, but used to allow the passage of ... arguments to this function from upstream, while ignoring and ... arguments from upstream which do not match arguments in this function.
#'
#' @return a plate layout tibble, as described in \code{read_plate_layout}
#'
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr ungroup select group_by summarise across n_distinct distinct
#' @importFrom tidyr separate
#' @importFrom tidyselect any_of everything where
#'
#' @export
extract_plate_layout <- # given a tibble of labeld data, extract just the layout
  function(data, # tibble of cleaned data
           .auto_detect = TRUE, # bool--drop all columns with the most unique values?
           .extract_well = TRUE,
           .extract_well_into = c("well", "channel_from_var", "type_from_var"),
           .extract_sep = "_",
           .var_col = "variable",
           .well_col = "well", # name of the column which will be called as well
           .keep_manual = c("dye_conc_uM",
                            "channel_f",
                            "dye",
                            "type",
                            "exp_num",
                            "identity"),
           .respect_grouping = FALSE,
           ... # ignore non-matching arguments passed via .. from upstream
  ) {


    if (.respect_grouping == FALSE) {
      data <- data %>% dplyr::ungroup()
    }

    if(.extract_well) {
      msg <- glue::glue("Dropping columns extracted from `{.var_col}`: {glue::glue_collapse(.extract_well_into[.extract_well_into != 'well'], sep = ', ')}")
      print(msg)


      ## check if existing columns will be dropped in this process, warn coder
      .existing_col_to_be_dropped <- names(data)[names(data) %in% .extract_well_into]
      if (length(.existing_col_to_be_dropped > 0)) {
        msg_warning <- glue::glue("Column(s) `{glue::glue_collapse(.existing_col_to_be_dropped, sep = ', ')}` present in both `.extract_well_into`, and input data.\nAll column(s) named `{glue::glue_collapse(.existing_col_to_be_dropped, sep = ', ')}` will be dropped! \nTo retain column(s) `{glue::glue_collapse(.existing_col_to_be_dropped, sep = ', ')}` in input data, change entries in `.extract_well_into` to be unique!")
        print(msg_warning)
      }

      # extract well and drop other elements
      data <-
        data %>%
        tidyr::separate({{ .var_col }}, into = .extract_well_into,
                        sep = .extract_sep,
                        remove = FALSE) %>%
        dplyr::select(-tidyselect::any_of(.extract_well_into[.extract_well_into != "well"])) # remove additional pieces
    }

    if(.auto_detect) {
      layout_cols <-
        data %>%
        dplyr::group_by(.data[[.var_col]]) %>% # each trace
        dplyr::summarise(dplyr::across(.cols = tidyselect::everything(),
                                       dplyr::n_distinct))  %>% # has only one value per layout variable
        dplyr::select(where(~!any(. > 1))) %>% # see utils::globalVariables() below--where() is not exported from tidyselect
        names()

    } else {
      layout_cols <- .keep_manual
    }

    layout <-
      data %>%
      dplyr::select(tidyselect::any_of(c(.well_col, layout_cols))) %>%
      dplyr::distinct()

  }
# example useage
# layout <- extract_plate_layout(tidied_screen)

utils::globalVariables("where")
# known issue witth importing where() from tidyselect as of
#  https://community.rstudio.com/t/where-is-not-an-exported-object-from-namespace-tidyselect/74911
# points to  https://github.com/r-lib/tidyselect/issues/201#issuecomment-650547846
# > checking dependencies in R code ... NOTE
# Missing or unexported object: ‘tidyselect::where’
# where() is not yet exported from tidyselect
# per lionel's comment Aug 18 202 in the github thread
# lionel "globalVariables() is often the right and correct answer to static analysis problems with NSE functions."
# this issue was closed, but note it's getting mentioned in thread active as of "this month" August 2021
# https://github.com/r-lib/tidyselect/issues/244
# but note that they're just pointing folks back to the original github issue, where the recommendations are still to use the globalVariables() approach
