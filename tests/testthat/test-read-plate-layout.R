#------- Correct output format ---------
test_that("read_plate_layout returns a tibble", {
  test_data <- read_plate_layout(test_path("raw_plate_layout.csv"))
  expect_s3_class(test_data, "tbl_df")
})

# has correct column names
# has correct column types
# has correct wells
# has correct handling of missing values


### CAN HANDLE ALL OF THE EXPECTED TYPES OF INPUTS, e.g. csvs and excel files

#------- Correct argument behavior ---------
