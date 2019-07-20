#' Play the prediction game.
#' 
#' @param data A numeric vector.
#' @param n The number of times to play the game.
#' @param guess_1 A number.
#' @param guess_2 A number.
#' @param FUN A function to apply to data.
#' @param ... Optional arguments to FUN.
#' 
#' @return A tibble with the results of the game.
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom purrr map_dbl
#' 
#' @export
#' 
#' @examples
#' play(data = 1:10, n = 5, 3, 7, sample, size = 1)

play <- function(data, n, guess_1, guess_2, FUN, ...){
  
  # What is the best way to deal with the specific names of the variables we are
  # working with? That is, data should be a tibble with x and y and whatever.
  # But we are just passing in the name of the tibble. We don't mention the
  # variable names. Right now, the function (FUN) argument is just something
  # like mean. But we don't know which column in "data" the function "mean"
  # should be applied to. For now, we just hard code it. Must be a better way!
  
  # We want to allow for more complex functions as well. Example:
  # "median(sample(x, 5, replace = FALSE))". Might be cool if we could allow
  # chains. Or mayb anonymous functions are enough?


  # For now, hard code take a random sample of the data vector. But next
  # version, data should be a tibble and then this won't work. And that is OK!
  # No need to allow someone to pass in a vector. Or maybe they can pass in
  # anything which works in their function . . .
  
  # For now, do in a loop, but we want this to be purrr at some point.
  
  # Maybe this should be a Shiny app? Would then need to pre-load all the data
  # sets we care about.
  
  # Need to add some test cases.
  
  # We should not need to calculate an empty tibble at the start. Instead, we
  # should be using map_df() which will produce a tibble automatically.
  
  x <- tibble::tibble(.rows = n) %>% 
    tibble::add_column("guess_1" = NA_real_, 
                       "guess_2" = NA_real_, 
                       "answer" = NA_real_)
  
  x <- x %>% 
    mutate(guess_1 = {{guess_1}},
           guess_2 = {{guess_2}}) %>% 
    
    mutate(answer = map_dbl(rep(list({{data}}), {{n}}), FUN, ...)) %>% 
    
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
