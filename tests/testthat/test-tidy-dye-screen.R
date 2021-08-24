load("raw_protein_dye_screen.RData")
load("raw_buffer_dye_screen.RData")
load("dye_screen_layout.RData")

tidied_screen <-
  tidy_dye_screen(.raw_data  = raw_protein_dye_screen,
                  .raw_layout = dye_screen_layout,
                  .buffer_data = raw_buffer_dye_screen,
                  .buffer_layout  = dye_screen_layout,
                  .protein_name = "SP149_CHIP",
                  .exp_num = "Exp1194",
                  .buffer_used = "P1_1mM_TCEP")

test_that("output of tidy_dye_screen is a tibble", {
  expect_s3_class(tidied_screen, "tbl_df")
})

test_that("output of tidy_dye_screen has the correct columns", {
  correct_names <- c("variable", "Temperature", "value", "dye_conc_uM", "channel_f", "dye",  "type", "value_norm", "exp_num", "identity",  "value_group_norm")
  expect_named(tidied_screen, correct_names)

})

test_that("output of tidy_dye_screen has columns of the correct types", {
  expect_equal(
    unname(unlist(lapply(tidied_screen, class))),
    c("character", #"variable",
      "numeric",#"Temperature",
      "numeric",#"value",
      "numeric",#"dye_conc_uM",
      "factor", #"channel_f",
      "character",  #"dye",
      "character",  #"type",
      "numeric",#"value_norm",
      "character",  #"exp_num",
      "character",  #"identity"
      "numeric" #value_group_norm
    )
  )
})

test_that("output of tidy_dye_screen has 'Temperature' and 'value' columns which are not normalized", {
  temps <- tidied_screen$Temperature %>% unique()
  expect_true(max(temps) > 1)

  values <- tidied_screen$value %>% unique()
  expect_true(max(values) > 1)
})

test_that("output of tidy_dye_screen contains no NAs", {
  expect_equal(tidied_screen, tidyr::drop_na(tidied_screen) )

})

test_that("output of tidy_dye_screen has 'variable' column which is genuinely unique to a single trace", {
  temp_dup <- tidied_screen %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(unique_temps  = !duplicated(Temperature))

  expect_true(all(temp_dup$unique_temps))
})
