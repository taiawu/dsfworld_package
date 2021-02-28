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

read_qTower <- function( file_path,
                         channel_levels  = c("FAM", "JOE", "TAMRA", "ROX", "Cy5", "Cy5.5", "SyproOrange")
) {

  df_raw <- readr::read_csv(file_path,
                            col_names = c(0:500) %>% base::as.character()) %>% # read in 500 columns; this should exceed any actual run, and fill in columsn to right as NAs
    select_if(~sum(!is.na(.)) > 0) #%>% # remove all columns which are all NAs

  df <- df_raw %>%
    tidyr::drop_na(ncol(.)) %>% # drop the header, which is NA in the tailing columns
    mutate( channel = make_channel_vec(.)) %>% # add channel as a column
    rename(well = "0") %>%
    dplyr::filter(!well %in% .$channel) %>%
    mutate(across(c(-.data$well, -.data$channel), base::as.numeric)) %>%
    pivot_longer(-c("well", "channel"), names_to = "Temperature", values_to = "value") %>%
    mutate(across(c(.data$well, .data$channel), base::as.character)) %>%
    mutate(across(c(.data$Temperature, .data$value), base::as.numeric)) %>%
    mutate(channel_f = factor(.data$channel, levels = channel_levels))
}
