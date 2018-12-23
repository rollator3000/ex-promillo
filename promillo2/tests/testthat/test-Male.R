# test enviroment for promillo2


# Check wheater we get right results for males!
test_that("correct result for males!", {
  expect_equal(tell_me_how_drunk(
    age = 34,
    sex = "M",
    height = 190,
    weight = 87,
    drinking_time = as.POSIXct(c("2016-10-03 18:15:00", "2016-10-03 22:55:00")),
    drinks = c("massn" = 2, "schnaps" = 4)
  ), 1.734178371)
  
  expect_equal(tell_me_how_drunk(
    age = 38,
    sex = "M",
    height = 190,
    weight = 134,
    drinking_time = as.POSIXct(c("2016-10-03 18:00:00", "2016-10-03 22:00:00")),
    drinks = c("hoibe" = 1)
  ), 0)
  
  expect_equal(tell_me_how_drunk(
    age = 68,
    sex = "Male",
    height = 169,
    weight = 84,
    drinking_time = as.POSIXct(c("2016-10-03 08:10:00", "2016-10-03 08:15:00")),
    drinks = c("schnaps" = 3)
  ), 0.6872831691)
})
