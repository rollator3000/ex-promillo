# test enviroment for promillo2
options(digits = 5)

test_that("correct result for females!", {
  expect_equal(tell_me_how_drunk(
    age = 14,
    sex = "Female",
    height = 160,
    weight = 54,
    drinking_time = as.POSIXct(c("2016-10-03 14:00:00", "2016-10-03 21:00:00")),
    drinks = list("hoibe" = 1, "schnaps" = 2)
  ), 0.3687942141)
})
