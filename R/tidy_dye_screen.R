#' Label and clean dye screen data
#'
#' A helper function for tidy_dye_screen(). Ensures that individual runs contain the correct columns.
#'
#' @param .data a long-form tibble containing a single plate of raw DSF data from a dye screen. Must include columns containing information on each of the following things:
#' \itemize{
#'   \item \strong{well}, or some other individual reaction-identifying information. This column is coerced to character-type in the output of this function.
#'   \item \strong{Temperature}, giving the Temperature of each RFU measurement. This column is coerced to numeric-type in the output of this function.
#'   \item \strong{Wavelength of measurement} A column giving information about the wavelengths at which measurements were made, e.g. "channel_f". This column must be a factor; it is recommended that the factor levels reflect the wavelengths measured by each channel. For example, "channel_f" may be a factor column, with levels of "FAM", "JOE", "TAMRA", "ROX", "Cy5", "Cy5.5". This column is added automatically by read_qtower, but may need to be generated manually for other types of inputs.
#'
#'   \item \strong{Raw RFU value}, giving the raw values measured. This column is coerced to numeric-type in the output of this function. A by-variable normalized version of this column is also present in the returned tibble.
#' }
#' @param .layout a long-form tibble containing plate layout information for the supplied raw data. Must include columns containing information on each of the following things:
#' #' \itemize{
#'   \item \strong{well}, or some other individual reaction-identifying information which matches the identifying column in the supplied data.nThis column is coerced to character-type in the output of this function.
#'   \item \strong{dye}, giving a unique identifying name for the dye that was present in a well. This column is coerced to character-type in the output of this function.
#'   \item \strong{dye concentration}, a column giving the concentration (in uM) at which the dye was tested. This column is coerced to numeric-type in the output of this function. uM units are not precisely necessary, but the default name of this column for processing is "dye_conc_uM", so non-uM units may lead to misleading column labeling if default column names are used.
#' }
#' @param .type a string, giving either "protein" or "buffer". This value will be present in the output as a column entitled "type", with only this value below.
#' @param .variable_col a string, giving the name of the column containing unique identifying information for each trace of data. For DSF runs in which each well was measured in multiple channels, note that "variable" must comprise both the well and measurement channel to be unique, e.g. "A1_FAM", "A1_JOE", etc. Defaults to "variable".
#' @param .temp_col a string, giving the name of the column containing the temperature data. Defaults to "Temperature".
#' @param .value_col a string, giving the name of the column containing the raw RFU values. Defaults to "value".
#' @param .dye_col a string, giving the name of the column containing the unique dye names. Defaults to "final_compound".
#' @param .dye_conc_col a string, giving the name of the column containing the dye concentrations Defaults to "final_concentration".
#' @param .channel_col a string, giving the name of the column containing the channel information for the measurements. Defaults to "channel_f".
#' @param .drop_layout_empties_from a string, giving the name of the column  used to drop any NA-containing rows from the output data. Is passed to .drop_from in label_data, and defaults here to "dye"
#' @param .keep_additional_cols a character vector, giving names of any additional columns that should be retained in the final output beyond the default "variable", "Temperature", "value", "dye_conc_uM", "channel_f", "dye". This argument is useful in cases where additional information from the layout should be carried through the dye screen analysis process.
#' @param ... additional arguments, passsed to label_data.
#'
#' @return a tibble, containing data for a labeled dye screen, of either protein or buffer. Columns are:
#' #' #' \itemize{
#'   \item \strong{variable}, a character column holding unique identifiers for each trace.
#'   \item \strong{Temperature},
#'   \item \strong{value},
#'   \item \strong{value_norm},
#'   \item \strong{dye}, a character column specifying the dye tested in each well.
#'   \item \strong{dye_conc_uM}, a character column specifying the concentration at which a dye was tested.
#'   \item \strong{channel_f}, a character column specifying the channel in which the raw values were measured.
#'   \item \strong{type}, a character column specifying the type of moleceule screened-- "protein" or "buffer". Each output may contain only one value for "type". For plates in which buffer and protein were both screened, layouts which mask the wells of the opposite type should be used, to create separate outputs for each type of data.
#'
#' }
#' @export
label_dye_screen <-
  function(.data,
           .layout,
           .type,
           .variable_col = "variable",
           .temp_col = "Temperature",
           .value_col = "value",
           .dye_col = "final_compound",
           .dye_conc_col = "final_concentration",
           .channel_col = "channel_f",
           .drop_layout_empties_from = "dye",
           .keep_additional_cols = "",
           ...
  ) {

    out <- label_data(data = .data,
                      layout = .layout,
                      .drop_from = .drop_layout_empties_from,
                      ...) %>%
      dplyr::ungroup() %>% # incase data is put in grouped
      dplyr::mutate("type" = .type) %>%
      tidyr::unite("variable", c(.data[[.variable_col]], .data$type), remove = FALSE) %>%
      dplyr::rename("variable" = .data[[.variable_col]],
                    "Temperature" = .data[[.temp_col]],
                    "value" = .data[[.value_col]],
                    "dye_conc_uM" = .data[[.dye_conc_col]],
                    "channel_f" = .data[[.channel_col]],
                    "dye" = .data[[.dye_col]])  %>%
      dplyr::filter(!is.na(.data[[.drop_layout_empties_from]])) %>%
      dplyr::select(tidyselect::any_of(c("variable",
                                         "Temperature",
                                         "value",
                                         "dye_conc_uM",
                                         "channel_f",
                                         "dye",
                                         "type",
                                         .keep_additional_cols))) %>%
      dplyr:: mutate(dplyr::across(.cols = c(.data$variable,
                                             .data$dye,
                                             .data$type),
                                   as.character),
                     dplyr::across(.cols = c(.data$Temperature,
                                             .data$value,
                                             .data$dye_conc_uM),
                                   as.numeric)) %>%
      dplyr::group_by(.data$variable) %>%
      dplyr::mutate(value_norm = scales::rescale(.data$value)) %>%
      dplyr::ungroup()
  }

# # example use
# raw_labeled <-
#       label_dye_screen(.data = df_raw,
#                        .layout = layout_1187,
#                        .type = "protein")
#
# buffer_labeled <-
#       label_dye_screen(.data = buffer_raw,
#                        .layout = layout_1187,
#                        .type = "buffer")


#' Tidy dye screens, combine buffer and protein data
#'
#' Combine protein and buffer dye screening data, and output a dye screening dataframe, tidied for downstream analysis.
#'
#' @param .raw_data a long-form tibble containing a single plate of raw DSF data from a dye screen. Must include columns containing information on each of the following things:
#' \itemize{
#'   \item \strong{well}, or some other individual reaction-identifying information. This column is coerced to character-type in the output of this function.
#'   \item \strong{Temperature}, giving the Temperature of each RFU measurement. This column is coerced to numeric-type in the output of this function.
#'   \item \strong{Wavelength of measurement} A column giving information about the wavelengths at which measurements were made, e.g. "channel_f". This column must be a factor; it is recommended that the factor levels reflect the wavelengths measured by each channel. For example, "channel_f" may be a factor column, with levels of "FAM", "JOE", "TAMRA", "ROX", "Cy5", "Cy5.5". This column is added automatically by read_qtower, but may need to be generated manually for other types of inputs.
#'
#'   \item \strong{Raw RFU value}, giving the raw values measured. This column is coerced to numeric-type in the output of this function. A by-variable normalized version of this column is also present in the returned tibble.
#' }
#'
#' @param .raw_layout a long-form tibble containing plate layout information for the supplied raw data. Must include columns containing information on each of the following things:
#' #' \itemize{
#'   \item \strong{well}, or some other individual reaction-identifying information which matches the identifying column in the supplied data.nThis column is coerced to character-type in the output of this function.
#'   \item \strong{dye}, giving a unique identifying name for the dye that was present in a well. This column is coerced to character-type in the output of this function.
#'   \item \strong{dye concentration}, a column giving the concentration (in uM) at which the dye was tested. This column is coerced to numeric-type in the output of this function. uM units are not precisely necessary, but the default name of this column for processing is "dye_conc_uM", so non-uM units may lead to misleading column labeling if default column names are used.
#' }
#'
#' @param .buffer_data the same as .raw_data, but data is from a no-protein control.
#' @param .buffer_layout the same as .raw_data, but data is from a no-protein control.
#' @param .protein_name the name of the protein screened.
#' @param .exp_num a unique identifier which can assist in tracing this output data back to a specific experiment or notebook entry.
#' @param .buffer_used a single string containing brief information on the buffer used in this screen.
#' @param ... additional arguments passed to label_dye_screen(), and label_data(), which is called within label_dye_screen().
#'
#' @return a tibble, containing data for a labeled dye screen, containing both protein and buffer data.
#'   \itemize{
#'   \item \strong{variable}, a character column holding unique identifiers for each trace.
#'   \item \strong{Temperature}, a numeric column holding Temperatures at which RFU measurements were made.
#'   \item \strong{value}, a numeric column holding raw RFU values measured.
#'   \item \strong{value_group_norm}, a numeric column holding raw RFU values, in which buffer and protein data for a particular dye, at a particular concentration, in a particular channel, collectively have been normalized to a 0 to 1 scale.
#'   \item \strong{value_norm}, a numeric column holding raw RFU values, scales 0 to 1 for each individual trace.
#'   \item \strong{dye}, a character column specifying the dye tested in each well.
#'   \item \strong{dye_conc_uM}, a character column specifying the concentration at which a dye was tested.
#'   \item \strong{channel_f}, a character column specifying the channel in which the raw values were measured.
#'   \item \strong{type}, a character column specifying the type of moleceule screened-- "protein" or "buffer". Each output may contain only one value for "type". For plates in which buffer and protein were both screened, layouts which mask the wells of the opposite type should be used, to create separate outputs for each type of data.
#'   \item \strong{identity}, a character column holding specific identities for the protein and buffer screened, e.g. "lysozyme" and "20mM_HEPES_pH7p2_200mM_NaCl_1mM_TCEP".
#'   \item \strong{exp_num}, a character column holding information which can be used to trace the experiment back to a specific notebook entry or other form of documentation.
#'}
#'
#' @export
tidy_dye_screen <-
  function(.raw_data,
           .raw_layout,
           .buffer_data,
           .buffer_layout,
           .protein_name,
           .exp_num,
           .buffer_used,
           ...) {

    ## write checks to ensure that these next two steps always work as expected
    # might make more sense to break these each out into their own tidying functions
    raw <-
      label_dye_screen(.raw_data,
                       .raw_layout,
                       .type = "protein")

    buffer <-
      label_dye_screen(.buffer_data,
                       .buffer_layout,
                       .type = "buffer")

    ## combine buffer and raw data
    out <-
      dplyr:: bind_rows(raw, buffer) %>%
      dplyr::mutate("exp_num" = .exp_num,
                    "identity" = dplyr::if_else(.data$type == "protein",
                                              true = .protein_name,
                                              false = .buffer_used)) %>%
      dplyr::group_by(.data$dye, .data$dye_conc_uM, .data$channel_f) %>%
      dplyr::mutate("value_group_norm" = scales::rescale(.data$value)) %>%
      dplyr::ungroup()

  }
