#------- Correct output format ---------
# should return a tibble--but don't read objects in here, use rdas
# this avoid redundant testing of read_qtower and read_plate_layout

# test_that("read_qtower returns a tibble", {
#   test_data <- read_qtower(test_path("raw_qtower.csv"))
#   expect_s3_class(test_data, "tbl_df")
# })

#------- correct dropping of masked / empty wells
