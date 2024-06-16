#------- Correct output format ---------
test_data <- read_plate_layout(test_path("raw_plate_layout.csv"))

test_that("read_plate_layout returns a tibble", {
  expect_s3_class(test_data, "tbl_df")
})

test_that("read_layout returns correct column types", {
  expect_equal(
    unname(unlist(lapply(test_data, class))),
    c("character", #"well",
      "character", #"row"
      "numeric",   #"column"
      "character", #"condition"
      "character", #"compound"
      "numeric",   #"concentration"
      "numeric",   #"volume"
      "character", #"protein"
      "character", #"ligand"
      "numeric"   #"ligand_conc"
    )
  )
})

test_that("empty wells are dropped (test data has 352 wells)", {
  expect_equal(nrow(test_data), 352)
})


### CAN HANDLE ALL OF THE EXPECTED TYPES OF INPUTS, e.g. csvs and excel files

#------- Correct argument behavior ---------
