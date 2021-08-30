# small adjustments to get these functions package-ready

########## YOU NEED TO UPDATE CONVERT HEIGHTS AND PULL ASSIGNMENTS IN DSFWORLD
# YOU ADDED .... ARGUMENTS TO THESE AND THEY NEED TO HAVE THEM TO WORK

##---------------- for utils.R in dsfworld package ------
#' Extract layout from labeled plate data
#'
#' Ultimately, probably belongs in utils.R or the like. Use \code{extract_plate_layout} to generate a plate layout object from labeled data.
#'
#' @param data a tibble containing labeled dsf data
#' @param .auto_detect a boolean. If TRUE, attempts to automatically identify which columns contain layout information based on the number of unique values provided per well (layout columns have only one value per well; data columns have one per measurement). Defaults to TRUE.
#' @param .extract_well a boolean. If TRUE, extracts the "well" component from a column which contains well, e.g. "A1_FAM", will extract "A1"
#' @param .extract_well_into a character vector, passed directly to the "into" argument of tidyr::separate, if .extract_well is TRUE. Gives the names of the pieces into which the column named in the .var_col argument will be separated. Defaults to c("well", "channel_f", "type")
#' @param .extract_sep a string, passed directl yto the "sep" argument to tidyr::separate, if .extract_well is TRUE. Gives the separator between components; defaults to "_"
#' @param .var_col a string, giving the name of the column in the provided /code{data} containing unqiue trace identifiers. Defaults to "variable". This column is used to get well names, if .extract_well_into is TRUE.
#' @param .well_col a string, giving the name of the column containing well names, if any.
#' @param .keep_manual a characer vector which, if if .auto_detect = FALSE, gives the names of all column (other than well), which are to be kept and put in the final layout.
#' @param .respect_grouping a boolean, defaults to FALSE, on whether or not to respect grouping of input \code{data}. Defaults to FALSE.
#' @param ... additional named arguments, not passed to anything, but used to allow the passage of ... arguments to this function from upstream, while ignoring and ... arguments from upstream which do not match arguments in this function.
#'
#' @return a plate layout tibble, as described in \code{read_plate_layout}
#'
#' @export
extract_plate_layout <- # given a tibble of labeld data, extract just the layout
  function(data, # tibble of cleaned data
           .auto_detect = TRUE, # bool--drop all columns with the most unique values?
           .extract_well = TRUE,
           .extract_well_into = c("well", "channel_f", "type"),
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

# known issue as of
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


utils::globalVariables("where")

# example useage
# layout <- extract_plate_layout(tidied_screen)

##---------------- for one of the modeling .R scrips in dsfworld -------
#' Extract model tmas
#'
#' Starting from a tibble containing dsfworld fit information, use \code{extract_model_tmas} to pull just resulting tmas into a tidy tibble. Probably belongs somewhere in the fitting function scripts, ultimately.
#'
#' @param by_var_fit a tibble, as output by \code{add_model_fits}.
#' @param .var_col a string, giving the name of the column in \code{by_var_fit} containing the unique-identifier for each dataset. Defaults to ".var"
#' @param .model_name_col a string, giving the name of the column in \code{by_var_fit} containing the name of the fitted model used to find a given tma. Defaults to "model_name"
#' @param .model_tma_col a string, giving the name of the column in \code{by_var_fit} containing the tma resulting from the model fitting and tma extraction. Defaults to "model_tma".
#'
#' @return a tibble of apparent melting temperatures, comprising four columns
#' \itemize{
#' \item \strong{.var} character, giving the unique dataset identifier for each DSF trace.
#' \item \strong{model_name} character, giving the name of the model from which the given values and tmas were found, e.g. "model_1", "model_2"
#' \item \strong{which_value} character, giving the specific component for a given value, e.g. "sigmoid_1", "sigmoid_2"
#' \item \strong{tma} numeric, the value of the calculated tma in reference to normalized Temperatures.
#' }
#' @export
extract_model_tmas <-
  function(by_var_fit,
           .var_col = ".var",
           .model_name_col = "model_name",
           .model_tma_col = "model_tma") {
    # by_var_fit %>% str()
    #    grouped_df [1,152 × 9] (S3: grouped_df/tbl_df/tbl/data.frame)
    # $ .var      : chr [1:1152] "A1_FAM_protein" "A2_FAM_protein" "A3_FAM_protein" "A12_FAM_protein" ...
    # $ model_name: chr [1:1152] "model_1" "model_1" "model_1" "model_1" ...
    # $ data      :List of 1152

    out <-
      by_var_fit %>%
      dplyr::select(tidyselect::any_of(c(.var_col,
                                         .model_name_col,
                                         .model_tma_col)))  %>%
      tidyr::unnest(cols = c({{ .model_tma_col }})) %>%
      dplyr::ungroup()

    # out %>% str()
    #    tibble [2,259 × 4] (S3: tbl_df/tbl/data.frame)
    # $ .var       : chr [1:2259] "A1_FAM_protein" "A2_FAM_protein" "A3_FAM_protein" "A12_FAM_protein" ...
    # $ model_name : chr [1:2259] "model_1" "model_1" "model_1" "model_1" ...
    # $ which_value: chr [1:2259] "sigmoid_1" "sigmoid_1" "sigmoid_1" "sigmoid_1" ...
    # $ tma        : num [1:2259] 0.432 0.268 0.283 0.462 0 ...

  }
# example useage
# model_tmas <- extract_model_tmas(by_var_fit)


#' Combine raw and modeled data
#'
#' Use \code{join_with_preds} to combine raw data with the values predicted by a given model fit.
#'
#' @param tidied_for_tmas a grouped, nested tibble, as returned by \code{tidy_for_tmas}
#' @param by_var_fit a grouped, nested tibble, as returned by \code{add_model_fits}
#' @param .join_by a string, giving the name of the column shared between \code{tidied_for_tmas} and \code{by_var_fit}, by which data can be mapped between the two. Should be renamed, since the final combination of the tibbles is done with bind_rows(), not join_by(). Defaults to ".var".
#' @param .var_col a string, giving the name of the column shared between \code{tidied_for_tmas} and \code{by_var_fit}, which uniquely identifies each individual dataset. Defaults to ".var".
#' @param .model_name_col a string, giving the name of the column in which the fitted model is specified by name (e.g. "model_1", "model_2" . . . ). Defaults to "model_name".
#' @param .fit_data_col a string, giving the name of the column in \code{by_var_fit} containing nested tibbles of fitted data. Defaults to "data",.
#' @param .tidied_data_col a string, giving the name of the column in \code{tidied_for_tmas} containing nested tibbles of raw data. Defaults to "data".
#' @param .temp_col_raw a string, giving the name of the columns shared between \code{tidied_for_tmas} and \code{by_var_fit}, containing raw Temperatures. Defaults to "Temperature
#' @param .value_norm_col_raw a string, giving the name of the column in \code{tidied_for_tmas} containing normalized RFU data.
#' @param .value_col_pred a string, giving the name of the column in \code{by_var_fit} containing predicted and reconstructed values. Defaults to "value"
#' @param .keep_cols_from_raw a character vector, containing names of the columns to be carried through into the final output tibble from \code{tidied_for_tmas}. Defaults to c("Temperature_norm", "value_norm").
#' @param .rescale_raw_temps a boolean, defaults to FALSE, giving whether or not the supplied column name to \code{.temp_col_raw} should be normalized in a 0 to 1 range to match the always-normalized Temperature_norm used in fitting.
#' @param ... additional arguments, not passed to anything internal here, but allows for the function to recieve named arguments from upstream functions while ignoring arguments passed with ... which match nothing in this function.
#'
#' @return a tibble of five columns
#' \itemize{
#' \item \strong{.var} character, giving the unique dataset identifier for each DSF trace.
#' \item \strong{Temperature_norm} numeric, giving the normalized Temperature for a given value
#' \item \strong{value} numeric, giving either the normalized raw RFU, a prediction from a fitted model to that normalized raw data, or individual component extract from the model prediction.
#' \item \strong{which_value} character, describing which of the possible values types corresponding to a given value in the \code{value} column. e.g. "raw" (normalized raw RFU), "pred" (full model prediction), "resid" (residuals from the model prediction)
#' \item \strong{model_name} character, giving the name of the model which was fit, and from which the predictions have been extracted. e.g. "model_1", "model_2" ... For the rows corresponding to raw data, modle_name is NA.
#' }
#'
#' @export
join_with_preds <- # rejoin predictions and raw data after model fitting
  function(tidied_for_tmas, # tidied raw data
           by_var_fit, # model fitting results
           .join_by = ".var", # str chr
           .var_col = ".var", # str chr
           .model_name_col = "model_name", # str chr
           .fit_data_col = "data", # str chr
           .tidied_data_col = "data", # name of the data col to unnest in raw

           .temp_col_raw = "Temperature", # name of the Temperature col in raw data
           .value_norm_col_raw = "value_norm", # name of the value_norm col in raw data
           .value_col_pred = "value",
           .keep_cols_from_raw = c("Temperature_norm", "value_norm"),
           .rescale_raw_temps = FALSE, # if TRUE, adds Temperature_norm col to raw data
           ...) {

    #------------ inputs
    # tidied_for_tmas %>% str()
    # grouped_df [3,696 × 2] (S3: grouped_df/tbl_df/tbl/data.frame) ## NESTED
    #  $ .var: chr [1:3696] "A1_FAM_protein" "A2_FAM_protein" "A3_FAM_protein" "A4_FAM_protein" ...
    #  $ data:List of 3696

    # by_var_fit %>% str()
    # grouped_df [1,152 × 9] (S3: grouped_df/tbl_df/tbl/data.frame)
    #  $ .var      : chr [1:1152] "A1_FAM_protein" "A2_FAM_protein" "A3_FAM_protein" "A12_FAM_protein" ...
    #  $ model_name: chr [1:1152] "model_1" "model_1" "model_1" "model_1" ...
    #  $ data      :List of 1152

    if(.rescale_raw_temps) {
      tidied_for_tmas <-
        tidied_for_tmas %>%
        dplyr::mutate(!!.tidied_data_col := purrr::map({{ .tidied_data_col }}),
                      function(df)  df %>% dplyr::mutate( "Temperature_norm" = scales::rescale({{ .temp_col_raw }})))
    }

    # extract predictions
    preds <-
      by_var_fit %>%
      dplyr::ungroup() %>%
      dplyr::select(tidyselect::any_of(c(.var_col,
                                         .model_name_col,
                                         .fit_data_col) )) %>%
      tidyr::unnest(cols = c(.data[[.fit_data_col]])) %>%
      dplyr::rename(!!.join_by := .data[[.var_col]]) # match to raw for joining


    #------------ structures for documentation
    # preds %>% str()
    # grouped_df [357,980 × 5] (S3: grouped_df/tbl_df/tbl/data.frame)
    #  $ variable        : chr [1:357980] "A1_FAM_protein" "A1_FAM_protein" "A1_FAM_protein" "A1_FAM_protein" ...
    #  $ model_name      : chr [1:357980] "model_1" "model_1" "model_1" "model_1" ...
    #  $ Temperature_norm: num [1:357980] 0 0 0.0145 0.0145 0.029 ...
    #  $ which_value     : chr [1:357980] "resid" "pred" "resid" "pred" ...
    #  $ value           : num [1:357980] 3.44e-01 1.41e-14 3.81e-01 4.09e-14 3.89e-01 ...
    #  - attr(*, "groups")= tibble [192 × 2] (S3: tbl_df/tbl/data.frame)

    raw <-
      tidied_for_tmas %>%
      dplyr::ungroup() %>%
      tidyr::unnest(cols = c(.data[[.tidied_data_col]])) %>%
      dplyr::select(tidyselect::any_of(c(.join_by, .keep_cols_from_raw))) %>%
      dplyr::rename(!!.value_col_pred := .data[[.value_norm_col_raw]]) %>%
      dplyr::mutate("which_value" = "raw")

    out <- dplyr::bind_rows(raw, preds)
    # out %>% str()
    # tibble [616,700 × 5] (S3: tbl_df/tbl/data.frame)
    #  $ .var            : chr [1:616700] "A1_FAM_protein" "A1_FAM_protein" "A1_FAM_protein" "A1_FAM_protein" ...
    #  $ Temperature_norm: num [1:616700] 0 0.0145 0.029 0.0435 0.058 ...
    #  $ value           : num [1:616700] 0.935 0.947 1 0.945 0.883 ...
    #  $ which_value     : chr [1:616700] "raw" "raw" "raw" "raw" ...
    #  $ model_name      : chr [1:616700] NA NA NA NA ...
  }

# example use
# raw_and_pred <- join_with_preds(tidied_for_tmas, by_var_fit)


#' Tidy model data for plotting
#'
#' Ultimately, may not export this function.
#'
#' Use \code{tidy_for_plot} to prepare raw and predicted data for plotting.
#'
#' @param tidied_for_tmas a tibble of raw data, as returned by \code{tidy_for_tmas}
#' @param by_var_fit a tibble of fit results, as returned by \code{add_model_fits}
#' @param layout a tibble of plate layout information, as returned by \code{read_plate_layout} or \code{extract_plate_layout}
#' @param .var_col a string, giving the name of the column in both \code{tidy_for_tmas} and \code{by_var_fit} giving identical unqiue identifiers fo a given dataset. Defaults to ".var"
#' @param .model_name_col a string, giving the name of column in \code{by_var_fit} which gives the name of the fitted model, e.g. "model_1". Defaults to "model_name".
#' @param .hit_filt_col a string, giving the name of the column which is created here through a \code{unite()} operation, and later matched to hit calls. Defaults to "dye_channel_f"
#' @param .hit_filt_components a character vector of length 2, giving the columns in \code{tidied_for_tmas} to join (in the given order) to create the column which will bear the name given in \code{.hit_filt_col}, with an underscore separator. Defaults to c("dye", "channel_f").
#' @param ... additional arguments, not passed to anything internal here, but allows for the function to recieve named arguments from upstream functions while ignoring arguments passed with ... which match nothing in this function.
#'
#' @return a tibble, rady for passing to downstream plotting functions.
#'
#' @export
tidy_for_plot <- # re-joins raw and fitting prediction data and laouts for plotting
  function(tidied_for_tmas, # as output by
           by_var_fit, # as output by add_model_fits, must contain  column.var, model_name, model_tma
           layout, # layout, often extracted from raw_df #  layout <- extract_plate_layout(raw_df, ...)
           .var_col = ".var",
           .model_name_col = "model_name",
           .hit_filt_col = "dye_channel_f",
           .hit_filt_components = c("dye", "channel_f"),
           ...) {

    fitted_vars <-
      by_var_fit[[.var_col]] %>%
      unique()

    df <- # keep only the traces that were fit
      tidied_for_tmas %>%
      dplyr::filter(.data[[.var_col]] %in% fitted_vars) %>%
      join_with_preds(., by_var_fit, ...) %>% # add "which_value" col
      dplyr::left_join(., layout,
                       by = .var_col) %>% # add layout info used for plotting downstream
      tidyr::unite(".var_which_val", c(.data[[.var_col]], # from user
                                       .data$which_value), # always "which_value", via join_with_preds
                   remove = FALSE) %>%
      tidyr::unite(!!.hit_filt_col,  c(.data[[.hit_filt_components[[1]]]], # typically dye
                                       .data[[.hit_filt_components[[2]]]]), # typically channel_f
                   remove = FALSE)
  }


#' Plot raw and predicted data together, with Tmas
#'
#' use \code{plot_with_preds} to generate a plot displaying the results of model fitting. In a better version of this function, would likely be refactored alongside its wrappers, \code{plot_analyzed_hits()} and \code{save_model_figure()}.
#'
#' @param raw_and_pred a tibble, as returned by \code{join_with_preds}
#' @param tma_df a tibble, as returend by \code{extract_model_tmas}
#' @param .var_col a string, giving the name of the column in both \code{tidy_for_tmas} and \code{by_var_fit} giving identical unqiue identifiers fo a given dataset. Defaults to ".var"
#' @param .var_which_val_col a string, giving the name of the column in \code{raw_and_pred} which further specifies the unique datasets into individaul value types, e.g. "A1_FAM_raw", "A1_FAM_sigmoid_1", etc. Passed to ggplot(aes(group = )), to define unique datasets for all geoms used in this plot.
#' @param .which_value_col a string, giving the name of the column in \code{raw_and_pred} giving the value type--e.g. "sigmoid_1", "sigmoid_2", etc. Passed to aes(fill = ) argument of geom_ribbon; used to shade the reconstructed sigmoids and reconstructed initial fluroescence omponents differently.
#' @param .model_name_col a string, giving the name of column in \code{raw_and_pred} which gives the name of the fitted model, e.g. "model_1". Defaults to "model_name".
#' @param .Temperature_norm_col a string, giving the name of the column in \code{raw_and_pred} containing normalized Temperatures. Used as the x values / x axis  in the returned plot.
#' @param .value_col a string, giving the name of the column in \code{raw_and_pred} containing y values, regardless of value type. Used as the y axis / y values in the returned plot.
#' @param .tma_col a string, giving the name of the column in \code{tma_df} ctaining the apparent melting temperatures extracted from the fits. Note that these must correspond to the normalized Temperature, unless the predictions and raw data have been mapped onto raw Temperature before the creation of this plot.
#' @param .fill_scale a named character vector, passed directly to the \code{values} argument of \code{scale_color_manual}. Determines the shading colors used in the reconstructed fit components. Defaults to c("sigmoid_1" = "#4AA267", "sigmoid_2" = "#4AA267", "sigmoid_3" = "#4AA267", "initial_1" = "darkgrey")
#' @param .point_size a number, passed to geom_point(size = ), the geom used to display the raw data. Defaults to 0.1
#' @param .point_alpha a number, passed to geom_point(alpha = ), the geom used to display the raw data. Defaults to 0.2.
#' @param .point_color a string of a quoted color name, passed to geom_point(color = ),  the geom used to display the raw data. Defaults to "black".
#' @param .pred_line_color a string of a quoted color name, passed to geom_line(color = ), the geom used to display the full model prediction. Defaults to "black".
#' @param .vline_color a string of a quoted color name, passed to geom_vline(color = ), the geom used to display tmas. Defaults to "black"
#' @param .vline_size a number, passed to geom_vline(size = ), the geom used to display tmas. Defaults to 0.2
#' @param .line_size a number, passed to geom_line(size = ), the geom used to display the full model prediction. Defaults to 0.3
#' @param .line_alpha a number, passed to geom_line(alpha = ), the geom used to display the full model prediction. Defaults to 0.8
#' @param .ribbon_alpha a number, passed to geom_ribbon(alpha = ), the geom used to display the individual reconstructed model components. Defaults to 0.3
#' @param .ribbon_color a string of a quoted color name, passed to geom_ribbon(color = ), the geom used to display the full model prediction. Defaults to NA, but if assigned, will appear as a line on the top of the shaded regions.
#' @param ... additional arguments, not passed to anything internal here, but allows for the function to recieve named arguments from upstream functions while ignoring arguments passed with ... which match nothing in this function. IN a better version of this function, these arguments be passed to \code{theme_screen_austere()} within this function.
#'
#' @return a minimal ggplot2 plot object for the given data, without faceting, fixed dimensions, or cutom labels.
#'
#' @export
plot_with_preds <-
  function(raw_and_pred,
           tma_df,
           .var_col = ".var",
           .var_which_val_col = ".var_which_val",
           .which_value_col = "which_value",
           .model_name_col = "model_name",
           .Temperature_norm_col = "Temperature_norm",
           .value_col = "value",
           .tma_col = "tma",

           .fill_scale = c("sigmoid_1" = "#4AA267",
                           "sigmoid_2" = "#4AA267",
                           "sigmoid_3" = "#4AA267",
                           "initial_1" = "darkgrey"),
           .point_size = 0.1,
           .point_alpha = 0.2,
           .point_color = "black",
           .pred_line_color = "black",
           .vline_color = "black",
           .vline_size = 0.2,

           .line_size  = 0.3,
           .line_alpha = 0.8,

           .ribbon_alpha = 0.3,
           .ribbon_color = NA,
           ...) {

    raw <- raw_and_pred %>%
      dplyr::filter(.data[[.which_value_col]] == "raw") %>%
      dplyr::select(-.data[[.model_name_col]])

    pred <- raw_and_pred %>%
      dplyr::filter(.data[[.which_value_col]] == "pred")

    components <- raw_and_pred %>%
      dplyr::filter(! .data[[.which_value_col]] %in% c("pred", "raw", "resid"))

    p <- pred %>%
      ggplot2::ggplot(ggplot2::aes(x = .data[[.Temperature_norm_col]],
                                   y = .data[[.value_col]],
                                   group = .data[[.var_which_val_col]]) # .var, + which component
      ) +

      ggplot2::geom_vline(data = tma_df,
                          ggplot2::aes(xintercept = .data[[.tma_col]]),
                          color = .vline_color,
                          size = .vline_size) +

      ggplot2::geom_point(data = raw,
                          size = .point_size,
                          alpha = .point_alpha,
                          color = .point_color) +

      ggplot2::geom_line(data = pred,
                         color = .pred_line_color,
                         size = .line_size,
                         alpha = .line_alpha,
                         linetype = "solid") +

      ggplot2::geom_ribbon( data = components,
                            ggplot2::aes(ymin = 0,
                                         ymax = .data[[.value_col]],
                                         fill = .data[[.which_value_col]]
                            ),
                            alpha =  .ribbon_alpha,
                            color = .ribbon_color) +

      ggplot2::scale_fill_manual( values = .fill_scale) +

      dsfworld::theme_screen_austere()
    #
    # list("plot" = p,
    #      "raw" = raw,
    #      "pred" = pred,
    #      "components" = components,
    #      "raw_and_pred" = raw_and_pred)
  }

#' Plot
#'
#' A wrapper function for \code{plot_with_preds}; \code{plot_analyzed_hits} uses an additional input tibble to filter the plotted data to include only hits, and adds custom plot titles. In a better version of this function, would likely be refactored alongside \code{plot_with_preds()} and \code{save_model_figure()}.
#'
#'
#' @param tidied_for_plot a tibble of raw data, as returned by \code{tidy_for_tmas}
#' @param hits_by_channel a tibble, as expected by \code{pull_assigned}, containing assignments matching to values found in \code{tidied_fot_plot}, in the column specifided through the \code{.dye_channel_col} argument.
#' @param model_tmas a tibble, as returned by \code{extract_model_tmas}
#' @param .dye_channel_col a string, giving the name of the column in \code{tidied_for_plot} containing information on which hits were called, e.g. "dye_channel_f", which typically contains values in the format <dye_name>_<channel_name>, e.g. "T004_FAM".
#' @param .plot_assignment a string, passed to \code{pull_assigned}; all values with this assignment in \code{hits_by_channel} will appear in the output plot.
#' @param .var_col a string, giving the name of the column in both \code{tidy_for_tmas} and \code{by_var_fit} giving identical unqiue identifiers fo a given dataset. Defaults to ".var"
#' @param ... additional arguments, passed to \code{plot_with_preds()}.
#'
#' @return a ggplot2 object, as returned by \code{plot_with_preds}, for a defined subset of the data, and with a plot title.
#'
#' @export
plot_analyzed_hits <-
  function(tidied_for_plot,
           hits_by_channel,
           model_tmas,
           .dye_channel_col = "dye_channel_f",
           .plot_assignment = "hit",
           .var_col = ".var",
           ...) {

    titles <- make_figure_title(tidied_for_plot)

    hits <- pull_assigned(hits_by_channel,
                          assignment = .plot_assignment, # usually "hit"
                          .dye_col = .dye_channel_col)

    for_plot_hits <-
      tidied_for_plot %>%
      dplyr::filter(.data[[.dye_channel_col]] %in% hits)

    for_plot_tmas <-
      model_tmas %>%
      dplyr::filter(.data[[.var_col]] %in% for_plot_hits[[.var_col]]) %>%
      dplyr::left_join(for_plot_hits, by = .data[[.var_col]]) # add other cols, e.g. those used for faceting
    print("The message Joining, by = <> (usually .var, model_name, which_value), is generated within plot_analyzed_hits(), when labeling the tma data so that it will facet correctly. Address this in a later version of the function; the printing is unsettling and this introduces a potentil source of silent failure." )

    p <- plot_with_preds(for_plot_hits, for_plot_tmas, ...)

  }


#' Save a figure displaying raw data, fits, and tmas. Use reasonable dimensions.
#'
#' A wrapper function for adding facets, calculating dimenstions, and saving with a consistent name. In a better version of this function, would likely be refactored alongside \code{plot_with_preds()} and \code{plot_analyzed_hits()}. Calls on \code{save_stacked_plot()} to do so.
#'
#' @param tidied_for_tmas a grouped, nested tibble, as returned by \code{tidy_for_tmas}
#' @param by_var_fit a tibble, as output by \code{add_model_fits}.
#' @param tidied_screen a tibble, as returned by \code{tidy_dye_screen()}
#' @param hits_by_channel a tibble containing hit assignments, as expected by \code{pull_assigned()}
#' @param extract_figure_title a boolean; if TRUE, both saved name and plot title are extracted from the given data via \code{make_figure_title()}
#' @param extract_layout_from_tidied a boolean; if TRUE, the experimental layout is extracted from \code{tidied_screen} via \code{extract_plate_layout}, and the unique-identifier column of the output layout is named ".var"
#' @param extract_tmas_from_by_var_fit a boolean; if TRUE, the tmas applied to the plots are extracted directly from \code{by_var_fit}. This is usually done in a single operation, and it's not clear exactly why one would set this to FALSE unless avoid that additional operation was important, and the model tma values had already been calculated and stored elsewhere.
#' @param layout if extract_layout_from_tidied is FALSE, a tibble containig a layout, as returned by \code{read_plate_layout} or \code{extract_plate_layout}. Gets passed to \code{tidy_for_plot()} in this function.
#' @param model_tmas if extract_tmas_from_by_var_fit is FALSE, a tibble containing the extracted model tmas, formatted to match the output of \code{extract_model_tmas()}.
#' @param .well_col_layout a string, giving the name of the column containing well names in \code{tidied_for_tmas}, which is renamed ".var" if extract_layout_from_tidied = TRUE.
#' @param ... additional named parameters, passed to the functions: \code{extract_plate_layout()}, \code{join_with_preds()}, \code{tidied_for_plot()}, \code{plot_analyzed_hits()}, \code{force_panel_sizing()}, and \code{save_stacked_plots()}
#'
#' @return if assigned, the final figure object. regardless of assignment, a saved figure, with names, directories, and file types as dictated by \code{save_stacked_plots()}. Uses \code{save_stacked_plots()} defaults, unless these arguments are overwritten by passing new named values for them to this function.
#'
#' @export
save_model_figure <-
  function(tidied_for_tmas, # as generated by tidy_for_tmas
           by_var_fit, # as greated by add_model_fits
           tidied_screen, # a layout, or a tibble with layout info
           hits_by_channel, # a tibble containing the hits, called by channel
           extract_figure_title = TRUE,
           extract_layout_from_tidied = TRUE, # boolean, if FALSE, user can supply
           extract_tmas_from_by_var_fit = TRUE, # boolean, if FALSE, user can supply
           layout = NULL, # linked to extract_layout_from_tidied; user supplies here
           model_tmas = NULL, # linked to extract_tmas_from_by_var_fit; user supplies here
           .well_col_layout = "variable",
           ... # passed to join_with_preds, tidy_for_plot, and/or plot_analyzed_hits
  ) {

    if (extract_layout_from_tidied) {
      layout <-
        extract_plate_layout(tidied_screen,
                             .well_col = .well_col_layout,
                             .extract_well = FALSE,
                             ...) %>%
        dplyr::rename(".var" = .data[[.well_col_layout]])
    }

    if (extract_tmas_from_by_var_fit) {
      model_tmas <-
        extract_model_tmas(by_var_fit)
    }

    if (extract_figure_title) {
      fig_titles <- make_figure_title(tidied_screen)
    }

    raw_and_pred <-
      join_with_preds(tidied_for_tmas,
                      by_var_fit,
                      ...)

    tidied_for_plot <- # homoegenize column names and join
      tidy_for_plot(tidied_for_tmas,
                    by_var_fit,
                    layout,
                    ...)

    p_hits <- # facet by model, only hits
      plot_analyzed_hits(tidied_for_plot,
                         hits_by_channel,
                         model_tmas,
                         ...) +
      ggplot2::facet_wrap(ggplot2::vars(.data$dye_channel_f, .data$model_name),
                          scales = "free", ncol = 6) +
      ggplot2::labs(fill = "Component type",
                    x = "Normalized Temperature",
                    y = "Normalized RFU",
                    subtitle = "Fits and Tmas for hits") +
      ggplot2::theme(aspect.ratio = 1/1.618,
                     plot.title = element_text(hjust = 0),
                     plot.subtitle = element_text(hjust = 0)) +
      force_panel_sizing(...)

    hits <-
      hits_by_channel %>%
      dplyr::filter(.data$assignment == "hit") %>%
      dplyr::pull(.data$dye_channel_f)

    save_height <- convert_heights(.paneled_by = hits,
                                   .facet_type = "grid")

    save_stacked_plots(.plot_list = list(p_hits),
                       .figure_titles = fig_titles,
                       .plot_heights = c(save_height),
                       .default_width = 7,
                       .save_suffix = "with_model_preds",
                       ...
    )

    out <- p_hits

  }

