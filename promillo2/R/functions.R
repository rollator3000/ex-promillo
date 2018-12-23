# functions for the "promillo2" package!
# Write your functions, that should be in the package! 
#' Get your approximate blood alcohol concentration!!!
#'
#' tell_me_how_drunk is used to calculate the alcohlic concentration in the 
#' blood of a person, given its attributes (weight, sex,...) according to 
#' [this page] ("https://thatsthefinger.com/")
#'
#' @param age is a numeric integer for the age in years of a given person!
#' @param sex is a character variable, indicating the gender of the given person!
#' @param height is the size of the given person in cm!
#' @param weight is a the weight of a given person in kg 
#' @param drinking_time is a vector of dates, with the start and the end time!
#' @param drinks is a list of drinks, the person has drunk
#' @return a numeric value of the approximate blood alcohol concentration of the
#' person at the end of the given drinking time
#' @export
#' @import checkmate
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  alcohol_drunk <- .get_alcohol(drinks)
  bodywater <- .get_bodywater(sex, age, height, weight)
  .get_permille(alcohol_drunk, bodywater, drinking_time)
}


# no comments, as we don't want to export this function! --> no doc!
.get_alcohol <- function(drinks) {
  # homogenize inputs:
  drinks <- unlist(drinks)
  assert_subset(names(drinks), choices = c("massn", "hoibe", "wein", "schnaps"),
                empty.ok = FALSE)
  assert_numeric(drinks, lower = 0)
  
  volume <- c(
    "massn"    = 1000,
    "hoibe"   = 500,
    "wein"    = 200,
    "schnaps" = 40)
  alcohol_concentration <- c(
    "massn"    = 0.06,
    "hoibe"   = 0.06,
    "wein"    = 0.11,
    "schnaps" = 0.4)
  alcohol_density <- 0.8
  
  sum(drinks * volume[names(drinks)] *
        alcohol_concentration[names(drinks)] * alcohol_density)
}


# no comments, as we don't want to export this function! --> no doc!
.get_bodywater <- function(sex = c("male", "female"), age, height, weight) {
  sex <- tolower(sex)
  sex <- match.arg(sex)
  
  assert_number(age, lower = 10, upper = 110)
  if (age < 16 | age > 90) {
    warning("...ts ts ts, this at your age!")
  }
  assert_number(height, lower = 100, upper = 230)
  assert_number(weight, lower = 40, upper = 300)
  
  coef <- if (sex == "male") {
    c(2.447, -0.09516, 0.1074, 0.3362)
  } else {
    c(0.203, -0.07, 0.1069, 0.2466)
  }
  t(coef) %*% c(1, age, height, weight)
}


# no comments, as we don't want to export this function! --> no doc!
.get_permille <- function(alcohol_drunk, bodywater, drinking_time){
  assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)
  
  alcohol_density <- 0.8
  blood_density <- 1.055
  permille <- alcohol_density * alcohol_drunk / (blood_density * bodywater)
  
  partylength <- difftime(drinking_time[2], drinking_time[1], units = "hours")
  sober_per_hour <- 0.15
  # sobering up starts only after one hour & you can't be more sober than 0:
  max(0, permille  - (max(0, partylength - 1) * sober_per_hour))
}