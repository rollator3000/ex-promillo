# function for the "promillo2" package, to plot the alcohol over time!
# Write your functions, that should be in the package! 

#' Plot the progress of the alcohol concentration!
#'
#' used to  plot the permille of a person over the time, with 5 min intervalls!
#'
#' @param age is a numeric integer for the age in years of a given person!
#' @param sex is a character variable, indicating the gender of the given person!
#' @param height is the size of the given person in cm!
#' @param weigth is a the weight of a given person in kg 
#' @param drinking_time is a vector of dates, with the start and the end time!
#' @param drinks is a list of drinks, the person has drunk
#' @return plot of the progress of the blood alcohol concentraion of a person!
#' @export
#' @import ggplot2

show_me_how_drunk <- function(age, sex, height, weigth, drinking_time, drinks) {
  
  # create a sequence from the start to the endpoint, with 5 min intervalls!
  time_seq <-  seq(from = drinking_time[1], to = drinking_time[2], by = 300)

  permille_list <- lapply(1:length(time_seq), FUN = function(x) tell_me_how_drunk(
    age = age, sex = sex, height = height, weight = weigth,
    drinking_time = as.POSIXct(c(time_seq[1], time_seq[x])),
    drinks = drinks))
  
  permilles_plot <- data.frame("Permille" = unlist(permille_list),
                               "drinking_time" = time_seq,
                               stringsAsFactors = FALSE)
  
  qplot(x = permilles_plot$drinking_time, y = permilles_plot$Permille,
        main = "Porgress of the alcohol level")
}