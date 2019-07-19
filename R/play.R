#' Play the prediction game.
#' 
#' @param data A numeric vector.
#' @param n The number of times to play the game.
#' @param guess_1 A number.
#' @param guess_2 A number.
#' 
#' @return A tibble with the results of the game.
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' 
#' @export
#' 
#' @examples
#' play(1:10, 5, 3, 7)

play <- function(data, n, guess_1, guess_2){
  
  # We need to add an argument which is some function which will be run on data
  # and produce an answer which can then be compared to guess_1 and guess_2.
  # Easiest if the function includes the variable names in a simple fashion.
  # That is, data should be a tibble with x and y or whatever. The function
  # (FUN) argument would then take something like "mean(x)". This code would be
  # run as is. 
  
  # We want to allow for more complex functions as well. Example:
  # "median(sample(x, 5, replace = FALSE))". Might be cool if we could allow
  # chains. Or mayb anonymous functions are enough?


  # For now, hard code take a random sample of the data vector. But next
  # version, data should be a tibble and then this won't work. And that is OK!
  # No need to allow someone to pass in a vector. Or maybe they can pass in
  # anything which works in their function . . .
  
  # For now, do in a loop, but we want this to be purrr at some point.
  
  # Need to add some test cases.
  
  x <- tibble::tibble(.rows = n) %>% 
    tibble::add_column("guess_1" = NA_real_, 
                       "guess_2" = NA_real_, 
                       "answer" = NA_real_)
  
  for(i in seq(n)){
    x$guess_1[i] <- guess_1
    x$guess_2[i] <- guess_2
    x$answer[i] <- sample(data, size = 1)
  }

  x <- x %>% 
     mutate(winner = case_when(
       abs(guess_1 - answer) <  abs(guess_2 - answer) ~ "guess_1",
       abs(guess_1 - answer) >  abs(guess_2 - answer) ~ "guess_2",
       abs(guess_1 - answer) == abs(guess_2 - answer) ~ "tie"))
  
  
  # Output will be a tibble with n rows, one for each run of the experiment.
  # Plan is to pass that tibble on to a new function, using nice Tidyverse
  # chaining, which will display the result in some pleasing fashion. Check out
  # the d3rain package. A dual column histogram that fills over time would be
  # cool!
  
  x
}