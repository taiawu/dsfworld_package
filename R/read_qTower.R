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
#' @importFrom utils "globalVariables"
#'
#' @export
read_qtower <- function( file_path,
                         start_temp = 25,
                         inc_temp = 1,
                         channel_levels  = c("FAM", "JOE", "TAMRA", "ROX", "Cy5", "Cy5.5", "SyproOrange")
) {

  df_raw <- readr::read_csv(file_path,
                            col_names = c(0:500) %>% base::as.character()) %>% # read in 500 columns; exceeding any run, filling in cols to right as NAs
    dplyr::select_if(~sum(!is.na(.)) > 0) %>% # remove all columns which are all NAs
    base::suppressMessages() %>% # suppressing column specification message
    base::suppressWarnings() # suppress parsing failure warning for columns given in the range 0:500 but not found

  df <- df_raw %>%
    tidyr::drop_na(ncol(.)) %>% # drop the header, which is NA in the tailing columns
    dplyr::mutate( channel = make_channel_vec(.)) %>% # add channel as a column
    dplyr::rename(well = "0") %>%
    dplyr::filter(!.data$well %in% .$channel) %>% # drop the channel-containing rows
    dplyr::mutate(dplyr::across(c(-.data$well, -.data$channel), base::as.numeric)) %>% # make data numeric
    tidyr::pivot_longer(-c("well", "channel"), names_to = "Temperature", values_to = "value") %>% # to long form
    dplyr::mutate(dplyr::across(c(.data$well, .data$channel), base::as.character)) %>% # make well and channel character
    dplyr::mutate(dplyr::across(c(.data$Temperature, .data$value), base::as.numeric)) %>% # make data numeric
    dplyr::mutate(channel_f = base::factor(.data$channel, levels = channel_levels)) %>% # make channel a factor
    dplyr::mutate(Temperature = (start_temp - 1) + (.data$Temperature * inc_temp)) %>% # convert cycle to temp
    tidyr::unite(variable, c(well, channel_f), remove = FALSE) %>% # unique trace identifier
    dplyr::group_by(variable) %>%
    dplyr::mutate(value_norm = scales::rescale(value, to = c(0,1))) %>%
    dplyr::ungroup() %>%
    dplyr::select(variable, well, channel, channel_f, Temperature, value, value_norm)
}

# helper function for read_qtower, not exported
make_channel_vec <- function( df ) { # make the vector which specifies channel for each reading
  channels <- df %>%
    dplyr::group_by(.data$`0`) %>%
    dplyr::filter(n() == 1) %>%
    dplyr::select(.data$`0`) %>%
    purrr::as_vector()

  n_meas <- df %>%  # the number of wells measured per channel (always the same for all channels )
    dplyr::group_by(.data$`0`) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::tally() %>%
    base::nrow()

  base::rep(channels , each = n_meas + 1) # add one, for the row which will still contain the channel itself
}

utils::globalVariables(c("Temperature", "channel", "channel_f", "value", "value_norm", "variable", "well"))
