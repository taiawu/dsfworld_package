#' Read raw data file as exported from a qTower
#'
#' Read raw data file (.csv) directly as exported from a qTower 384G qPCR instrument. Creates a "channel_f" column, which contains the channels as a factor, with default levels of FAM, JOE, TAMRA, ROX, Cy5 ,Cy5.5, SyproOrange. Converts from cycle number to Temperature, with a default starting temperature of 25, and 1 C increase between measurements.
#'
#' @param file_path A complete file path, pointing to the raw data (.csv) exported from a qTower
#' @param start_temp The temperature of the first read in the data file. Used to convert from cycle number to Temperature
#' @param inc_temp The increment by which the temperature increases with each read. Used to convert from cycle number to Temperature.
#' @param channel_levels A vector used to set the order of the channels measured in the loaded experiment. Defaults to FAM, JOE, TAMRA, ROX, Cy5 ,Cy5.5, SyproOrange. Channels not present in this list will be NA in the output tibble.
#'
#' @return A tibble containg all data, but no meta-data, present in the loaded csv file. Includes columns: well, channel, Temperature, value, and channel_f.
#'
#' @export
read_qtower <- function( file_path,
                         start_temp = 25,
                         inc_temp = 1,
                         channel_levels  = c("FAM", "JOE", "TAMRA", "ROX", "Cy5", "Cy5.5", "SyproOrange")
) {

  df_raw <- readr::read_csv(file_path,
                            col_names = c(0:500) %>% base::as.character()) %>% # read in 500 columns; this should exceed any actual run, and fill in columsn to right as NAs
    select_if(~sum(!is.na(.)) > 0) #%>% # remove all columns which are all NAs

  df <- df_raw %>%
        tidyr::drop_na(ncol(.)) %>% # drop the header, which is NA in the tailing columns
        mutate( channel = make_channel_vec(.)) %>% # add channel as a column
        rename(well = "0") %>%
        dplyr::filter(!.data$well %in% .$channel) %>%
        mutate(across(c(-.data$well, -.data$channel), base::as.numeric)) %>%
        pivot_longer(-c("well", "channel"), names_to = "Temperature", values_to = "value") %>%
        mutate(across(c(.data$well, .data$channel), base::as.character)) %>%
        mutate(across(c(.data$Temperature, .data$value), base::as.numeric)) %>%
        mutate(channel_f = factor(.data$channel, levels = channel_levels)) %>%
        mutate(Temperature = start_temp + (.data$Temperature * inc_temp))
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
