#------- Correct output format ---------
test_that("read_qtower returns a tibble", {
  test_data <- read_qtower(test_path("raw_qtower.csv"))
  expect_s3_class(test_data, "tbl_df")
})

test_that("read_qtower's output has correct column names", {
  test_data <- read_qtower(test_path("raw_qtower.csv"))
  expect_equal(names(test_data), c("variable", "well", "channel_f", "Temperature", "value", "value_norm"))
})

test_that("read_qtower's output has correct column types", {
  test_data <- read_qtower(test_path("raw_qtower.csv"))
    expect_equal(
      unname(unlist(lapply(test_data, class))),
      c("character",  # variable
        "character", # well
        "factor", # channel_f
        "numeric", # Temperature
        "numeric", # value
        "numeric" # value_norm
        )
    )
})

#------- Correct argument behavior ---------
test_that("read_qtower's start_temp input correctly determines starting temperature", {
  start_temp_val <- 40
  test_data <- read_qtower(test_path("raw_qtower.csv"),
                           start_temp = start_temp_val)
  expect_equal(min(test_data$Temperature), start_temp_val)

})

test_that("read_qtower's inc_temp input correctly determines inter-measurement temperature changes", {
  inc_temp_val <- 0.5

  test_data <- read_qtower(test_path("raw_qtower.csv"),
                           inc_temp = inc_temp_val) %>%
    filter(variable == "A1_FAM") # for a single variable

  expect_equal(diff(test_data$Temperature),
               rep(inc_temp_val, times = length(diff(test_data$Temperature))))

})

test_that("read_qtower's channel_levels input ordering genuinely determines channel_f levels", {
  channel_levels_value <-  c("SyproOrange",  "Cy5.5", "FAM", "JOE", "TAMRA", "ROX", "Cy5")

  test_data <- read_qtower(test_path("raw_qtower.csv"), channel_levels = channel_levels_value)

  expect_equal(levels(test_data$channel_f), channel_levels_value)

})

test_that("read_qtower's normalized columns scale from 0 to 1", {
  test_data <- read_qtower(test_path("raw_qtower.csv"))

  expect_equal(c(min(test_data$value_norm), max(test_data$value_norm)), c(0,1))

})

#------- Correct exception handling --- NOT YET WRITTEN INTO FUNCTION ---------
