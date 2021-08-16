#' Read raw data file as exported from a qTower
#'
#' Read raw data file (.csv) directly as exported from a qTower 384G qPCR instrument. Creates a "channel_f" column, which contains the channels as a factor, with default levels of FAM, JOE, TAMRA, ROX, Cy5 ,Cy5.5, SyproOrange. Converts from cycle number to Temperature, with a default starting temperature of 25, and 1 C increase between measurements.
#'
#' @param file_path A complete file path, pointing to the raw data (.csv) exported from a qTower
#' @param start_temp The temperature of the first read in the data file. Used to convert from cycle number to Temperature
#' @param inc_temp The increment by which the temperature increases with each read. Used to convert from cycle number to Temperature.
#' @param channel_levels A vector used to set the order of the channels measured in the loaded experiment. Defaults to FAM, JOE, TAMRA, ROX, Cy5 ,Cy5.5, SyproOrange. Channels not present in this list will be NA in the output tibble.
#'
#' @return A tibble containg all data, but no meta-data, present in the loaded csv file. Includes columns: variable (combines well and channel, to uniquely identify each trace), well, channel, channel_f (channels as a factor, ordered by increasing excitation wavelength), Temperature, value, and value_norm (each raw data trace, rescaled to a 0 to 1 range).
#'
#' @importFrom tidyr unite fill pivot_longer separate
#' @importFrom dplyr pull mutate row_number filter if_else across select na_if
#' @importFrom vroom vroom
#' @importFrom purrr set_names
#' @importFrom scales rescale
#'
#' @export
read_qtower <- function( file_path,
                         start_temp = 25,
                         inc_temp = 1,
                         channel_levels  = c("FAM", "JOE", "TAMRA", "ROX", "Cy5", "Cy5.5", "SyproOrange")
) {
  #____Determine the length of the metadata header
  # define the possible wells (on the fly bc its cheap)
  wells <-
    expand.grid(LETTERS[1:16], c(1:24)) %>%
    tidyr::unite("well", sep = "") %>%
    dplyr::pull(.data$well)

  # identify first row that contains a well
  first_well_row <-
    vroom::vroom(file_path, delim = ",", col_select = c(1)) %>%
    purrr::set_names(c("col")) %>%
    dplyr::mutate(is_well = .data$col %in% wells,
                  first_well = .data$is_well == TRUE & !duplicated(.data$is_well == TRUE),
                  row_num = dplyr::row_number()) %>%
    dplyr::filter(.data$first_well == TRUE) %>%
    dplyr::pull(.data$row_num) %>%
    base::suppressMessages() %>% # suppressing column specification message
    base::suppressWarnings()

  #_____Read the file in_____
  df <-
    vroom::vroom(file_path,
                 delim = ",",
                 skip = first_well_row-1,
                 col_names =  FALSE,
                 show_col_types = FALSE) %>%
    purrr::set_names(c("well_channel", .[1,][-1])) %>%
    dplyr::mutate(channel = dplyr::if_else(.data$well_channel %in% channel_levels,
                                           true = .data$well_channel,
                                           false = "well"),
                  channel = na_if(.data$channel, "well")) %>%
    tidyr::fill(.data$channel, .direction = "down") %>%
    dplyr::filter(.data$well_channel != .data$channel) %>%
    tidyr::unite("variable", c(.data$well_channel, .data$channel), sep = "_") %>%
    tidyr::pivot_longer(-.data$variable, names_to = "Temperature", values_to = "value") %>%
    dplyr::mutate(dplyr::across(.cols = c(.data$Temperature, .data$value), as.numeric),
                  Temperature = (start_temp - 1) + (.data$Temperature * inc_temp)) %>% # convert cycle to temp
    tidyr::separate(.data$variable, into = c("well", "channel"), sep = "_", remove = FALSE) %>%
    dplyr::mutate(channel_f = factor(.data$channel, levels = channel_levels),
                  value_norm = scales::rescale(.data$value, to = c(0,1))) %>%
    dplyr::select(.data$variable, .data$well, .data$channel_f, .data$Temperature, .data$value, .data$value_norm)

}
